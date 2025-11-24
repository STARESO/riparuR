#' ---
#' title : "RIPARU - Paths of folders and files"
#' author : Aubin Woehrel
#' date : 2024-09-23
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - PATHS OF FOLDERS AND FILES
#'
#' Description :
#' This script contains the paths of the folders and files used in the project.
#' All the paths are stored in the `paths` object.
#'
#' =============================================================================

# Creating new environment just for paths
paths_env <- new.env()

# Populating paths in the path environment
with(paths_env, {
  # Raw data ----
  microplastics_raw <- "data/raw/categorisation_microplastiques_riparu.xlsx"
  macrodechets_raw <- "data/raw/export_releves_2024-06.xlsx"
  typology_sites_raw <- "data/raw/typologie_sites.xlsx"

  # Processed data rds ----
  microplastics_processed <- "data/processed/riparu_microplastics.rds"
  microplastics_total_processed <- "data/processed/riparu_microplastics_total.rds"
  macrodechets_general_processed <- "data/processed/riparu_macrodechets_general.rds"
  macrodechets_counts_processed <- "data/processed/riparu_macrodechets_counts.rds"
  macrodechets_essential_processed <- "data/processed/riparu_macrodechets_essential.rds"
  typology_sites_processed <- "data/processed/riparu_typology_sites.rds"

  # Processed data csv ----
  microplastics_processed_csv <- "data/processed/riparu_microplastics.csv"
  microplastics_total_processed_csv <- "data/processed/riparu_microplastics_total.csv"
  macrodechets_general_processed_csv <- "data/processed/riparu_macrodechets_general.csv"
  macrodechets_counts_processed_csv <- "data/processed/riparu_macrodechets_counts.csv"
  macrodechets_essential_processed_csv <- "data/processed/riparu_macrodechets_essential.csv"
  typology_sites_processed_csv <- "data/processed/riparu_typology_sites.csv"
})

# Create folders if missing ----

# Extract all character paths from the environment
all_paths <- as.list(paths_env)
all_paths_vector <- unlist(all_paths)

all_folders <- sapply(all_paths_vector, function(p) {
  if (grepl("/$", p)) {
    # It's already a directory path
    p
  } else {
    # It's a file path; extract folder
    dirname(p)
  }
}, USE.NAMES = FALSE)

all_folders <- unique(all_folders)

# Create directories if needed
sapply(all_folders, function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
})

# Export for sourcing in other scripts ----
paths <- as.list(paths_env)
rm(all_paths, all_folders, all_paths_vector, paths_env)
