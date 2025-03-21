#' @title Generate acornym table
#' @description Generate a table of acronyms -
#'  @return html generated table to be used in `mod_main_page`, saved in www/ folder
#'  @noRd
require(knitr)
require(kableExtra)
require(readr)
require(dplyr)
generate_list <- function() {
  listofacronyms <- data.frame(
    Acronyms = c(
      "BOA",
      "BON",
      "CJS",
      "CUI",
      "CUTI",
      "DART",
      "DLM",
      "ICPB",
      "LGA",
      "LGR",
      "NCBI",
      "SAR"
    ),
    Description = c(
      "Bonneville Dam, adult detection site",
      "Bonneville Dam, juvenile detection site",
      "Cormack-Jolly-Seber (Cormack 1964, Jolly 1965, Seber 1965)",
      "Pacific Coastal Upwelling Index or Bakun index (Bakun 1973,1975)",
      "Coastal Upwelling Transport Index (Jacox et al., 2018)",
      "Data Access in Real-Time (Columbia Basin Research, University of Washington)",
      "Dynamic Linear Model",
      "Index of Coastal Prey Biomass (Daly et al., 2013)",
      "Lower Granite Dam, adult detection site",
      "Lower Granite Dam, juvenile detection site",
      "Northern Copepod Biomass Index (Hooff and Peterson, 2006)",
      "Smolt-to-adult return survival"
    )
  )


  # Generate the table
  acronym_table <- knitr::kable(
    listofacronyms,
    format = "html",
    table.attr = "class='table table-striped table-hover'",
    caption = "Table of acronyms and their descriptions"
  ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
    # row_spec(0, background = "#024c63", color = "white") %>% # Apply color to header row
    # row_spec(1:nrow(covariates), background = "white") %>% # Apply white background to body rows
    column_spec(1, bold = TRUE)  # Apply bold to first column


  # Export to www/ folder
  readr::write_file(acronym_table, "inst/app/www/mod_main_tbl_acronyms.html")
}

generate_list()
