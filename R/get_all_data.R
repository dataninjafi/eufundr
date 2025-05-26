#' Get all EU funding datasets in a single call
#'
#' @param country Optional country code (for Kohesio and Horizon) or country name (for FTS). If NULL, includes all countries.
#' @return A list of data frames: projects, beneficiaries, fts, horizon2020, horizon_europe
#' @export
get_all_data <- function(country = NULL) {
  list(
    kohesio_projects     = get_kohesio_projects(country),
    kohesio_beneficiaries = get_kohesio_beneficiaries(country),
    fts                  = get_all_fts(country),
    horizon_2020         = get_horizon_2020(country),
    horizon_europe       = get_horizon_europe(country)
  )
}
