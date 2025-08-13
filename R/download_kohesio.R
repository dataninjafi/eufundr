#' Download and load Kohesio project data
#'
#' Downloads project data from the Kohesio API. By default, retrieves data from all EU countries.
#' @param country Optional ISO2 country code (e.g. "FI"). If NULL (default), downloads all available countries.
#' @return A tibble of projects with cleaned column names
#' @export
get_kohesio_projects <- function(country = NULL) {
  # Lista maista, joille dataa on saatavilla
  countries <- c(
    "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI",
    "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
    "NL", "PL", "PT", "RO", "SE", "SI", "SK"
  )

  # Tarkistetaan, onko annettu maa-koodi tuettu
  if (!is.null(country)) {
    country <- toupper(country)
    if (!country %in% countries) {
      stop(paste("Unknown or unsupported country code:", country))
    }
    countries <- country
  }

  # Luodaan URL-osoitteet molemmille ajanjaksoille
  urls_21_27 <- paste0("https://kohesio.ec.europa.eu/en/data/programmingPeriod20212027/latest_", countries, "-21-27.xlsx")
  urls_14_20 <- paste0("https://kohesio.ec.europa.eu/en/data/programmingPeriod20142020/latest_", countries, "-14-20.xlsx")

  # Yhdistetään URL-osoitteet
  all_urls <- c(urls_21_27, urls_14_20)

  # Ladataan ja käsitellään dataa
  data_list <- purrr::map(all_urls, function(url) {
    # Yritetään ladata tiedosto ja käsitellään virhetilanteet
    tryCatch({
      dest <- tempfile(fileext = ".xlsx")
      download.file(url, dest, mode = "wb", quiet = TRUE)

      # Tarkistetaan, onko ladattu tiedosto olemassa
      if (file.exists(dest)) {
        # Määritetään maa ja ajanjakso URL:n perusteella
        country_code <- toupper(sub(".*latest_([A-Z]+)-.*", "\\1", url))
        period_code <- sub(".*latest_.*-([0-9]+)-([0-9]+).*", "\\1-\\2", url)

        # Luetaan data ja muokataan se sopivaan muotoon
        readxl::read_excel(dest) %>%
          janitor::clean_names() %>%
          dplyr::mutate(country = country_code, period = period_code)
      } else {
        # Palautetaan NULL, jos tiedostoa ei löydy
        NULL
      }
    }, error = function(e) {
      # Jos lataus epäonnistuu, tulostetaan virhe ja palautetaan NULL
      message(paste("Failed to download or read:", url, " - Error:", e$message))
      NULL
    })
  })

  # Poistetaan NULL-arvot listasta
  data_list <- data_list[!sapply(data_list, is.null)]

  # Yhdistetään datat ja palautetaan tulos
  if (length(data_list) > 0) {
    dplyr::bind_rows(data_list)
  } else {
    # Palautetaan tyhjä data-frame, jos yhtään tiedostoa ei voitu ladata
    message("No data could be retrieved for the specified country/countries.")
    dplyr::tibble()
  }
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
