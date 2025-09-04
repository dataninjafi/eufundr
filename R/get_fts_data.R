#' Download and parse FTS data for a given year
#'
#' @param year An integer year between 2007 and 2023
#' @param country Optional beneficiary country name (e.g., "Finland"). If NULL, includes all countries.
#' @return A tibble of filtered and cleaned FTS data
#' @export
get_fts_data <- function(year, country = NULL) {

  country_regex = tibble::tribble(
    ~iso.name.en, ~eurostat,
    "Austria",      "AT",
    "Belgium",      "BE",
    "Bulgaria",      "BG",
    "Croatia",      "HR",
    "Cyprus",      "CY",
    "Czechia",      "CZ",
    "Denmark",      "DK",
    "Estonia",      "EE",
    "Finland",      "FI",
    "France",      "FR",
    "Germany",      "DE",
    "Greece",      "EL",
    "Hungary",      "HU",
    "Ireland",      "IE",
    "Italy",      "IT",
    "Latvia",      "LV",
    "Lithuania",      "LT",
    "Luxembourg",      "LU",
    "Malta",      "MT",
    "Netherlands (the)",      "NL",
    "Poland",      "PL",
    "Portugal",      "PT",
    "Romania",      "RO",
    "Slovakia",      "SK",
    "Slovenia",      "SI",
    "Spain",      "ES",
    "Sweden",      "SE",
    "United Kingdom",      "UK"
  ) %>%
    filter(iso.name.en %in% country | eurostat %in% country) %>%
    flatten_chr()


  stopifnot(year %in% 2007:2024)

  url <- sprintf("https://ec.europa.eu/budget/financial-transparency-system/download/%d_FTS_dataset_en.xlsx", year)
  dest <- tempfile(fileext = ".xlsx")
  download.file(url, dest, mode = "wb", quiet = TRUE)

  out <- tryCatch({
    df <- readxl::read_excel(dest) %>%
      janitor::clean_names()
    if (!is.null(country)) {
      df <- dplyr::filter(df, beneficiary_country %in% country_regex)
    }
    df %>%
      dplyr::transmute(
        rahasto = paste(budget, year),
        hakijanimi = name_of_beneficiary,
        hankenimi = subject_of_grant_or_contract,
        beneficiary_country
      )
  }, error = function(e) {
    warning(sprintf("Skipping year %d due to read error: %s", year, e$message))
    NULL
  })

  return(out)
}
