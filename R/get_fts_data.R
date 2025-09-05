#' #' Download and parse FTS data for a given year
#' #'
#' #' @param year An integer year between 2007 and 2023
#' #' @param country Optional beneficiary country name (e.g., "Finland"). If NULL, includes all countries.
#' #' @return A tibble of filtered and cleaned FTS data
#' #' @export
#' get_fts_data <- function(year, country = NULL) {
#'
#'
#'   country = enrich_eu_country_info(country)
#'
#'   stopifnot(year %in% 2007:2024)
#'
#'   url <- sprintf("https://ec.europa.eu/budget/financial-transparency-system/download/%d_FTS_dataset_en.xlsx", year)
#'   dest <- tempfile(fileext = ".xlsx")
#'   download.file(url, dest, mode = "wb", quiet = TRUE)
#'
#'   out <- tryCatch({
#'     df <- readxl::read_excel(dest) %>%
#'       janitor::clean_names()
#'     if (!is.null(country)) {
#'       df <- dplyr::filter(df, beneficiary_country %in% country)
#'     }
#'     df
#'     # %>%
#'     #   dplyr::transmute(
#'     #     rahasto = paste(budget, year),
#'     #     hakijanimi = name_of_beneficiary,
#'     #     hankenimi = subject_of_grant_or_contract,
#'     #     beneficiary_country
#'     #   )
#'   }, error = function(e) {
#'     warning(sprintf("Skipping year %d due to read error: %s", year, e$message))
#'     NULL
#'   })
#'
#'   return(out)
#' }
