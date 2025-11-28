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
  raw_microplastiques <- "data/raw/categorisation_microplastiques_riparu_2025.xlsx"
  raw_macrodechets <- "data/raw/data_zds_2025.xlsx"
  raw_typologie_sites <- "data/raw/typologie_sites.xlsx"

  # Processed data rds ----
  processed_microplastiques <- "data/processed/riparu_microplastiques.rds"
  processed_microplastiques_total <- "data/processed/riparu_microplastiques_total.rds"
  processed_macrodechets_general <- "data/processed/riparu_macrodechets_general.rds"
  processed_macrodechets_nb <- "data/processed/riparu_macrodechets_nb.rds"
  processed_macrodechets_essentiel <- "data/processed/riparu_macrodechets_essentiel.rds"
  processed_typologie_sites <- "data/processed/riparu_typologie_sites.rds"

  # Processed data csv ----
  processed_microplastiques_csv <- "data/processed/riparu_microplastiques.csv"
  processed_microplastiques_total_csv <- "data/processed/riparu_microplastiques_total.csv"
  processed_macrodechets_general_csv <- "data/processed/riparu_macrodechets_general.csv"
  processed_macrodechets_nb_csv <- "data/processed/riparu_macrodechets_nb.csv"
  processed_macrodechets_essentiel_csv <- "data/processed/riparu_macrodechets_essentiel.csv"
  processed_typologie_sites_csv <- "data/processed/riparu_typologie_sites.csv"

  # Figures ----
  output_timeseries <- "output/timeseries/"
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
