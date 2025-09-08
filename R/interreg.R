#' Retrieve Interreg project data from keep.eu API
#'
#' This function sends a POST request to the [keep.eu](https://keep.eu) API and downloads an Excel file containing data on Interreg projects and their partners.
#' The results can be filtered by country if specified.
#'
#' @param country A character string or vector indicating the country name(s) or code(s) to filter results by.
#'   If `NULL`, data for all countries is returned.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{partners}{A tibble containing partner data, optionally filtered by country.}
#'   \item{projects}{A tibble containing project data linked to the filtered partners.}
#' }
#'
#' @examples
#' \dontrun{
#'   # Retrieve all projects and partners
#'   data <- get_interreg_data()
#'
#'   # Retrieve only projects related to Finland
#'   data <- get_interreg_data(country = "Finland")
#' }
#'
#' @export
get_interreg_data <- function(country = NULL) {

  country_filter <- enrich_eu_country_info(country)

  url <- "https://keep.eu/api/search/projects/"

  payload_json <- '{
  "projects":{"status":null,"prizes":false,"only_projects_with_documents":false,"project_details":{"start":[],"without_start":false,"end":[],"without_end":false},"project_budget":{"range":[],"without_budget":false},"themes":{"list":[],"type":"or"},"macro_regional_strategies":[],"only_infrastructure_financed":false},
  "programmes":{"type":[],"period":[],"available":[]},
  "partners":{"status":[],"type":[],"nuts_lead":[],"nuts_partner":[],"nuts_search_type":"both","selectedAreas":{}},
  "contribution_2014_2020":{"specific_objectives":{"thematic_objectives":[],"thematic_priorities":[]},"thematic_objectives_eni":[]},
  "contribution_2021_2027":{"specific_objectives":[],"intervention":[],"common_output_indicators":[],"common_result_indicators":[]},
  "search":{"list":[],"type":null,"fields":["name__unaccent__contains","acronym__unaccent__contains","description__unaccent__contains","expected_results__unaccent__contains","achievements__unaccent__contains","expected_achievements__unaccent__contains","actual_achievements__unaccent__contains","expected_outputs__unaccent__contains","delivered_outputs__unaccent__contains","partner__name__unaccent__exact","partner__name_translated__unaccent__contains"],"rawSearchString":""},
  "documents":{"document_lang":[],"languages":[],"types":[],"name":"","search":""},
  "project_lang":[],
  "project_desc_lang":[],
  "languages":[],
  "translation_languages":[],
  "thematic_objectives_eni":[],
  "location":null,
  "response_type":"excel"
}'

  req <- httr2::request(url) |>
    httr2::req_headers(
      "Accept" = "*/*",
      "Content-Type" = "application/json",
      # Nämä eivät yleensä ole pakollisia, mutta matkivat selainta:
      "Origin"  = "https://keep.eu",
      "Referer" = "https://keep.eu/projects/?hide-sidebar=true",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/139.0.0.0 Safari/537.36"
    ) |>
    httr2::req_body_raw(charToRaw(payload_json), type = "application/json") |>
    httr2::req_method("POST") |>
    httr2::req_timeout(300)

  resp <- httr2::req_perform(req)

  tmpfile <- tempfile(fileext = ".xlsx")
  writeBin(httr2::resp_body_raw(resp), tmpfile)

  projects <- readxl::read_excel(tmpfile, sheet = 2) |>
    janitor::clean_names()

  partners <- readxl::read_excel(tmpfile, sheet = 3) |>
    janitor::clean_names()

  if (!is.null(country)) {
    partners <- partners |>
      dplyr::filter(country_code %in% country_filter)
  }

  projects <- projects |>
    dplyr::semi_join(
      partners |>
        dplyr::select(project_acronym) |>
        dplyr::distinct(),
      by = "project_acronym"
    )

  list(partners = partners, projects = projects)
}
