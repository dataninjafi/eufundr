#' Download and load Kohesio project data
#'
#' Downloads project data from the Kohesio API. By default, retrieves data from all EU countries.
#' @param country Optional ISO2 country code (e.g. "FI"). If NULL (default), downloads all available countries.
#' @return A tibble of projects with cleaned column names
#' @export
get_kohesio_projects <- function(country = NULL) {
  countries <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI",
    "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
    "NL", "PL", "PT", "RO", "SE", "SI", "SK"
  )

  if (!is.null(country)) {
    country <- toupper(country)
    if (!country %in% countries) {
      stop(paste("Unknown or unsupported country code:", country))
    }
    countries <- country
  }

  urls <- paste0("https://kohesio.ec.europa.eu/api/data/object?id=data/projects/latest_", countries, ".xlsx")

  data_list <- purrr::map2(
    urls, countries,
    function(url, ctry) {
      dest <- tempfile(fileext = ".xlsx")
      download.file(url, dest, mode = "wb", quiet = TRUE)
      readxl::read_excel(dest) %>%
        janitor::clean_names() %>%
        dplyr::mutate(country = ctry)
    }
  )

  dplyr::bind_rows(data_list)
}

#' Download and load Kohesio beneficiary data
#'
#' Downloads beneficiary data from the Kohesio API. By default, retrieves data from all EU countries.
#' @param country Optional ISO2 country code (e.g. "FI"). If NULL (default), downloads all available countries.
#' @return A tibble of beneficiaries with cleaned column names
#' @export
get_kohesio_beneficiaries <- function(country = NULL) {
  countries <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI",
    "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
    "NL", "PL", "PT", "RO", "SE", "SI", "SK"
  )

  if (!is.null(country)) {
    country <- toupper(country)
    if (!country %in% countries) {
      stop(paste("Unknown or unsupported country code:", country))
    }
    countries <- country
  }

  urls <- paste0("https://kohesio.ec.europa.eu/api/data/object?id=data/beneficiaries/latest_", countries, ".csv")

  data_list <- purrr::map2(
    urls, countries,
    function(url, ctry) {
      dest <- tempfile(fileext = ".csv")
      download.file(url, dest, quiet = TRUE)
      read.csv(dest) %>%
        janitor::clean_names() %>%
        dplyr::mutate(country = ctry)
    }
  )

  dplyr::bind_rows(data_list)
}
