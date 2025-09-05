#' Download and parse FTS data for a given year
#'
#' @param year An integer year between 2007 and 2023
#' @param country Optional beneficiary country name (e.g., "Finland"). If NULL, includes all countries.
#' @return A tibble of filtered and cleaned FTS data
#' @export
get_fts_data <- function(year, country = NULL) {

  country = enrich_eu_country_info(country) %>%
    paste(collapse = "|")

  stopifnot(year %in% 2007:2024)

  url <- sprintf("https://ec.europa.eu/budget/financial-transparency-system/download/%d_FTS_dataset_en.xlsx", year)
  dest <- tempfile(fileext = ".xlsx")
  download.file(url, dest, mode = "wb", quiet = TRUE)

  out <- tryCatch({
    df <- readxl::read_excel(dest) %>%
      janitor::clean_names()
    if (!is.null(country)) {
      df <- dplyr::filter(df, str_detect(beneficiary_country, country))
    }
    df
    # %>%
    #   dplyr::transmute(
    #     rahasto = paste(budget, year),
    #     hakijanimi = name_of_beneficiary,
    #     hankenimi = subject_of_grant_or_contract,
    #     beneficiary_country
    #   )
  }, error = function(e) {
    warning(sprintf("Skipping year %d due to read error: %s", year, e$message))
    NULL
  })

  return(out)
}

#' Get all available FTS data combined
#'
#' @param country Optional beneficiary country name. If NULL, includes all countries.
#' @return A combined tibble of FTS data for all years
#' @export
get_all_fts <- function(country = NULL) {
  purrr::map_dfr(2007:2024, ~get_fts_data(.x, country))
}

#' Load Horizon Europe data
#'
#' @param country Optional ISO2 country code (e.g., "FI"). If NULL, includes all countries.
#' @return A tibble of Horizon Europe projects with organizations from the selected country or all countries
#' @export
get_horizon_europe <- function(country = NULL){

  country_filter = enrich_eu_country_info(country)

  zipfile <- tempfile(fileext = ".zip")
  url <- "https://cordis.europa.eu/data/cordis-HORIZONprojects-csv.zip"
  download.file(url, zipfile, mode = "wb", quiet = TRUE)

  projects <- read_csv2(unz(zipfile, "project.csv"))
  # %>%
  #   dplyr::select(projectID = id, hankenimi = title, toteutuneet_maksut = totalCost,
  #                 rahasto = frameworkProgramme, kuvaus = objective,
  #                 aloitusp = startDate, lopetusp = endDate)

  organizations <- read_csv2(unz(zipfile, "organization.csv"))

  if (!is.null(country)) organizations <- filter(organizations, country %in% country_filter)


  # organizations <- organizations %>%
  #   dplyr::mutate(ytunnus = stringr::str_remove(vatNumber, "FI")) %>%
  #   dplyr::select(projectID, ytunnus, hakijanimi = name, country)


  projects %>%
    rename(projectID = id) %>%
    inner_join(organizations, "projectID") %>%
    dplyr::select(-projectID)

}

#' Load Horizon 2020 data
#'
#' @param country Optional ISO2 country code (e.g., "FI"). If NULL, includes all countries.
#' @return A tibble of Horizon 2020 projects with organizations from the selected country or all countries
#' @export
get_horizon_2020 <- function(country = NULL) {

  country_filter = enrich_eu_country_info(country)

  zipfile <- tempfile(fileext = ".zip")
  url <- "https://cordis.europa.eu/data/cordis-h2020projects-csv.zip"
  download.file(url, zipfile, mode = "wb", quiet = TRUE)

  projects <- readr::read_csv2(unz(zipfile, "project.csv"))
  # %>%
  #   dplyr::select(projectID = id, hankenimi = title, toteutuneet_maksut = totalCost,
  #                 rahasto = frameworkProgramme, kuvaus = objective,
  #                 aloitusp = startDate, lopetusp = endDate)

  organizations <- readr::read_csv2(unz(zipfile, "organization.csv"))

  if (!is.null(country)) {
    organizations <- dplyr::filter(organizations, country %in% country_filter)
  }

  # organizations <- organizations %>%
  #   dplyr::mutate(ytunnus = stringr::str_remove(vatNumber, "FI")) %>%
  #   dplyr::select(projectID, ytunnus, hakijanimi = name, country)

  projects %>%
    rename(projectID = id) %>%
    inner_join(organizations, "projectID") %>%
    dplyr::select(-projectID)
}
