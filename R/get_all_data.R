#' Get all EU funding datasets in a single call
#'
#' @param country Optional country code (for Kohesio and Horizon) or country name (for FTS). If NULL, includes all countries.
#' @return A list of data frames: projects, beneficiaries, fts, horizon2020, horizon_europe
#' @export
get_all_data <- function(country = NULL) {
  list(
    kohesio_projects     = get_kohesio_projects(country),
    kohesio_beneficiaries = get_kohesio_beneficiaries(country),
    fts = purrr::map(2007:2024, function(x) get_fts_data(x, country)) |>
      dplyr::bind_rows(),
    horizon         = dplyr::bind_rows(
      get_horizon_data("HORIZON", country = country),
      get_horizon_data("h2020", country = country)),
    interreg  = get_interreg_data(country)
  )
}
