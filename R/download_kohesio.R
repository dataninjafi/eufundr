#' Download and load Kohesio project data (CSV version, direct read)
#'
#' Downloads project data from the Kohesio API in CSV format. By default, retrieves data from all EU countries.
#' @param country Optional ISO2 country code (e.g. "FI"). If NULL (default), downloads all available countries.
#' @return A tibble of projects with cleaned column names
#' @export

get_kohesio_projects <- function(country = NULL){
  countries <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI",
    "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
    "NL", "PL", "PT", "RO", "SE", "SI", "SK"
  )

  if (!is.null(country)) {
    country <- toupper(country)
    if (any(!country %in% countries)) {
      stop(paste("Unknown or unsupported country code:", country))
    }
    countries <- country
  }


  urls <- c(
    paste0(
      "https://kohesio.ec.europa.eu/api/data/object?id=data/projects-2021-2027/latest/",
      countries,
      "-pp21-27-latest.csv"
    ),
    paste0(
      "https://kohesio.ec.europa.eu/api/data/object?id=data/projects-2014-2020/latest/",
      countries,
      "-pp14-20-latest.csv"
    )
  )


  # Check if file exists (HEAD request)
  urls <- purrr::map(urls, function(url) {
    resp <- try(httr::HEAD(url, httr::timeout(10)), silent = TRUE)
    if (inherits(resp, "try-error") || httr::status_code(resp) != 200) {
      return(NA)
    }
    url
  }) |>
    purrr::flatten_chr() |>
    (\(x) x[!is.na(x)])()

  data_list <- purrr::map2(
    urls,
    stringr::str_extract(urls, '[A-Z]{2}'),
    function(url, ctry) {
      tryCatch({
        readr::read_csv(url, col_types = readr::cols(.default = "c")) |>
          janitor::clean_names() |>
          dplyr::mutate(country = ctry)
      }, error = function(e) {
        message("Failed to read: ", url)
        NULL
      })
    }
  )

   dplyr::bind_rows(Filter(Negate(is.null), data_list))
}


#' Download and load Kohesio beneficiary data (CSV direct read)
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
    if (any(!country %in% countries)) {
      stop(paste("Unknown or unsupported country code:", country))
    }
    countries <- country
  }

  urls <- paste0("https://kohesio.ec.europa.eu/api/data/object?id=data/beneficiaries/latest/", countries, "-latest.csv")

  data_list <- purrr::map2(
    urls, countries,
    function(url, ctry) {
      tryCatch({
        readr::read_csv(url, col_types = readr::cols(.default = "c")) |>
          janitor::clean_names() |>
          dplyr::mutate(country = ctry)
      }, error = function(e) {
        message("Failed to read: ", url)
        NULL
      })
    }
  )

  dplyr::bind_rows(Filter(Negate(is.null), data_list))
}
