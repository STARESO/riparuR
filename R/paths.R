#' ---
#' title : "RIPARU - Paths of folders and files"
#' author : Aubin Woehrel
#' date : 2024-09-23
#' version : 1.0
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


# Raw data ----
microplastics_raw <- "data/raw/categorisation_microplastiques_riparu.xlsx"
macrodechets_raw <- "data/raw/export_releves_2024-06.xlsx"
typology_sites_raw <- "data/raw/typologie_sites.xlsx"


# Processed data rds ----
microplatics_processed <- "data/processed/riparu_microplastics.rds"
microplastics_total_processed <- "data/processed/riparu_microplastics_total.rds"
macrodechets_general_processed <- "data/processed/riparu_macrodechets_general.rds"
macrodechets_counts_processed <- "data/processed/riparu_macrodechets_counts.rds"
macrodechets_essential_processed <- "data/processed/riparu_macrodechets_essential.rds"
typology_sites_processed <- "data/processed/riparu_typology_sites.rds"

# Processed data csv ----
microplatics_processed_csv <- "data/processed/riparu_microplastics.csv"
microplastics_total_processed_csv <- "data/processed/riparu_microplastics_total.csv"
macrodechets_general_processed_csv <- "data/processed/riparu_macrodechets_general.csv"
macrodechets_counts_processed_csv <- "data/processed/riparu_macrodechets_counts.csv"
macrodechets_essential_processed_csv <- "data/processed/riparu_macrodechets_essential.csv"
typology_sites_processed_csv <- "data/processed/riparu_typology_sites.csv"

# Preparing paths object for sourcing in other scripts :
# Takes all the variables and stores them in the object paths 
paths_names <- ls(envir = .GlobalEnv)
paths <- mget(paths_names, envir = .GlobalEnv)
paths <- as.list(paths)


rm(list = paths_names)
rm(list = ls()[!ls() %in% c("paths")])
