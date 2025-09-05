enrich_eu_country_info <- function(country){

  countrycode::codelist %>%
    drop_na(eu28) %>%
    select(country.name.en, iso2c) %>%
    filter(country.name.en %in% country | iso2c %in% country) %>%
    flatten_chr()
}
