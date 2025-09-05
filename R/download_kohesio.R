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

  urls <- paste0(
    "https://kohesio.ec.europa.eu/api/data/object?id=data/projects/latest_",
    rep(countries, each = 2),
    c("-21-27.xlsx", "-14-20.xlsx")
  )

  # Check if file exists (HEAD request)
  urls = map(urls,
      function(url){
        resp <- try(httr::HEAD(url, httr::timeout(2)), silent = TRUE)
        if (inherits(resp, "try-error") || httr::status_code(resp) != 200) {
          message("Skipping: ", url, " (not available)")
          return(NA)
        }
        url
      }
      ) %>%
    flatten_chr() %>%
    .[!is.na(.)]




  data_list <- purrr::map2(
    urls,
    str_extract(urls, '[A-Z]{2}'),
    function(url, ctry) {
      dest <- tempfile(fileext = ".xlsx")
      download.file(url, dest, mode = "wb", quiet = TRUE)
      readxl::read_excel(dest) %>%
        janitor::clean_names() %>%
        dplyr::mutate(country = country)
    }
  )

  # Drop NULL entries (skipped files) and combine
  dplyr::bind_rows(Filter(Negate(is.null), data_list))
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
