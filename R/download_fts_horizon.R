
#' Download and parse FTS data for a given year
#'
#' @param year An integer year between 2007 and 2024
#' @param country Optional beneficiary country name (e.g., "Finland"). If NULL, includes all countries.
#' @return A tibble of filtered and cleaned FTS data
#' @export
get_fts_data <- function(year, country = NULL) {

  stopifnot(year %in% 2007:2024)

  if (!is.null(country)) {
    country <- enrich_eu_country_info(country) |> paste(collapse = "|")
  }

  url <- sprintf("https://ec.europa.eu/budget/financial-transparency-system/download/%d_FTS_dataset_en.xlsx", year)
  dest <- tempfile(fileext = ".xlsx")
  download.file(url, dest, mode = "wb", quiet = TRUE)

  out <- tryCatch({
    df <- readxl::read_excel(dest, col_types = "text") |>
      janitor::clean_names()

    if (!is.null(country)) {
      df <- dplyr::filter(df, stringr::str_detect(beneficiary_country, country))
    }

    df
  }, error = function(e) {
    warning(sprintf("Skipping year %d due to read error: %s", year, e$message))
    NULL
  })

  return(out)
}


#' Load Horizon project data (Europe or 2020)
#'
#' @param programme Character string: either "HORIZON" (Europe) or "h2020"
#' @param country Optional ISO2 country code (e.g., "FI"). If NULL, includes all countries.
#' @return A tibble of Horizon projects with organizations from the selected country or all countries
#' @export
get_horizon_data <- function(programme = c("HORIZON", "h2020"), country = NULL) {

  # Validate programme input
  programme <- match.arg(programme)

  # Enrich country filter if provided
  country_filter <- enrich_eu_country_info(country)

  # Construct download URL based on programme
  base_url <- "https://cordis.europa.eu/data/"
  zipfile <- tempfile(fileext = ".zip")
  url <- paste0(base_url, "cordis-", programme, "projects-csv.zip")

  # Download ZIP archive
  download.file(url, zipfile, mode = "wb", quiet = TRUE)

  # Read project and organization data
  projects <- readr::read_csv2(unz(zipfile, "project.csv"))
  organizations <- readr::read_csv2(unz(zipfile, "organization.csv"))

  # Filter organizations by country if applicable
  if (!is.null(country)) {
    organizations <- dplyr::filter(organizations, country %in% country_filter)
  }

  # Optional: clean and rename columns for clarity
  # projects <- projects |>
  #   dplyr::select(projectID = id, title, totalCost, frameworkProgramme, objective, startDate, endDate)

  # organizations <- organizations |>
  #   dplyr::mutate(vat_id = stringr::str_remove(vatNumber, "FI")) |>
  #   dplyr::select(projectID, vat_id, organization_name = name, country)

  # Join datasets and return result
  projects |>
    dplyr::rename(projectID = id) |>
    dplyr::inner_join(organizations, by = "projectID")
}




