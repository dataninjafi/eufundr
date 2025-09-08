#' Enrich country names or codes with EU membership information
#'
#' This helper function checks whether the provided country names or ISO2 codes
#' belong to EU member states (based on the EU-28 list available in
#' \pkg{countrycode}).
#'
#' The function relies on \code{countrycode::codelist} to retrieve the list of EU-28
#' countries. It filters out only EU-28 members (\code{eu28} not \code{NA}) and
#' matches against both English country names (\code{country.name.en}) and ISO2
#' codes (\code{iso2c}).
#'
#' @param country A character vector of country names (English) or ISO2 country codes.
#'
#' @return A character vector containing the matching country names and ISO2 codes
#' for EU-28 member states. Returns an empty vector if no match is found.
#'
#' @export
enrich_eu_country_info <- function(country) {
  countrycode::codelist |>
    dplyr::filter(!is.na(eu28)) |>
    dplyr::select(country.name.en, iso2c) |>
    dplyr::filter(country.name.en %in% country | iso2c %in% country) |>
    unlist(use.names = FALSE)
}

