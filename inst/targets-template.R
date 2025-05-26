library(targets)

# Load package functions
source("_targets_packages.R")

list(
  tar_target(kohesio_projects_all, get_kohesio_projects()),
  tar_target(kohesio_beneficiaries_all, get_kohesio_beneficiaries()),
  tar_target(fts_all, get_all_fts()),
  tar_target(horizon_europe_all, get_horizon_europe()),
  tar_target(horizon_2020_all, get_horizon_2020())
)
