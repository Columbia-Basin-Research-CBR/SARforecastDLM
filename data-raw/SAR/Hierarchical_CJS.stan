functions {
  int first_capture(int[] y_i) {
    for (t in 1:size(y_i))
      if (y_i[t])
        return t;
    return 0;
  }

  int last_capture(int[] y_i) {
    for (t_rev in 0:(size(y_i) - 1)) {
      int t;
      t = size(y_i) - t_rev;
      if (y_i[t])
        return t;
    }
    return 0;
  }

  row_vector prob_uncaptured(int T, row_vector p, row_vector phi) {
    row_vector[T] chi;
    chi[T] = 1.0;
    for (t in 1:(T - 1)) {
      int t_curr;
      int t_next;
      t_curr = T - t;
      t_next = t_curr + 1;
      chi[t_curr] = (1 - phi[t_curr]) + phi[t_curr] * (1 - p[t_next]) * chi[t_next];
    }
    return chi;
  }
}

data {
  int<lower=2> T;                   // number of capture events (includes marking)
  int<lower=0> M;                   // number of unique capture histories
  int<lower=0> group_phi[M,T-1];    // phi group IDs for each unique capture history
  int<lower=0> group_p[M,T];        // p group IDs for each unique capture history
  int<lower=0,upper=1> y[M,T];      // y[m,t]: history m captured at t
  int<lower=1> n[M];                // n[m]: number of individuals with capture history y[m,]
}

transformed data {
  int<lower=1> J_phi;              // number of groups for phi
  int<lower=1> J_p;                // number of groups for p
  int<lower=0,upper=T> first[M];   // first capture occasion
  int<lower=0,upper=T> last[M];    // last capture occasion
  int<lower=0,upper=T-1> last_minus_first[M];  // duh

  J_phi = max(to_array_1d(group_phi));
  J_p = max(to_array_1d(group_p));

  for (m in 1:M) {
    first[m] = first_capture(y[m,]);
    last[m] = last_capture(y[m,]);
    last_minus_first[m] = last[m] - first[m];
  }
}

parameters {
  real<lower=0, upper=1> mu_phi;       // mean survival probability
  real<lower=0, upper=1> mu_p;         // mean recapture probability
  real<lower=0> sigma_phi;             // standard deviation of survival probability
  real<lower=0> sigma_p;               // standard deviation of recapture probability
  vector[J_phi] phi_raw;               // raw survival probabilities for each group
  vector[J_p] p_raw;                   // raw recapture probabilities for each group
}

transformed parameters {
  vector<lower=0, upper=1>[J_phi] phi; // Survival probabilities for each group
  vector<lower=0, upper=1>[J_p] p;     // Recapture probabilities for each group
  matrix[M,T-1] phi_mat;               // Survival probabilities for each capture history
  matrix[M,T] p_mat;                   // Recapture probabilities for each capture history
  matrix[M,T] chi;                     // Probability of not being captured after t
  vector[M] LL;                        // Log-likelihood of each capture history

  for (g in 1:J_phi) {
    phi[g] = inv_logit(mu_phi + sigma_phi * (phi_raw[g] - 0.5));
  }

  for (g in 1:J_p) {
    p[g] = inv_logit(mu_p + sigma_p * (p_raw[g] - 0.5));
  }

  for (m in 1:M) {
    for (t in 1:(T-1)) {
      if (group_phi[m, t] == 0)  // special code: fix survival to 0
        phi_mat[m, t] = 0;
      else
        phi_mat[m, t] = phi[group_phi[m, t]];
    }

    for (t in 1:T) {
      if (group_p[m, t] == 0)  // special code: fix recapture to 1
        p_mat[m, t] = 1;
      else
        p_mat[m, t] = p[group_p[m, t]];
    }

    chi[m, ] = prob_uncaptured(T, p_mat[m, ], phi_mat[m, ]);
  }

  LL = rep_vector(0, M);

  for (m in 1:M) {
    if (last_minus_first[m] > 0)  // if history m was recaptured
    {
      for (t in (first[m] + 1):last[m]) {
        LL[m] += n[m] * log(phi_mat[m, t - 1]);                 // survival from t - 1 to t
        LL[m] += n[m] * bernoulli_lpmf(y[m, t] | p_mat[m, t]); // observation (captured or not)
      }
    }
    LL[m] += n[m] * log(chi[m, last[m]]);   // Pr[not detected after last[m]]
  }
}

model {
  // Priors
  mu_phi ~ beta(1, 1);
  mu_p ~ beta(1, 1);
  sigma_phi ~ normal(0, 1);
  sigma_p ~ normal(0, 1);
  phi_raw ~ normal(0.5, 0.2);
  p_raw ~ normal(0.5, 0.2);

  // Likelihood
  target += sum(LL);
}


