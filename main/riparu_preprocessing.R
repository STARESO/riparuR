#' ---
#' title : "RIPARU - Preprocessing"
#' author : Aubin Woehrel
#' date : 2024-09-23
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - PLASTICS DATA PREPROCESSING
#'
#' Description :
#' This script contains all the steps to preprocess the raw data from the RIPARU
#' project to obtain clean and tidy data sets for further analysis.
#'
#' =============================================================================

# Initialization ----

## Clean up ----
rm(list = ls())

## Library imports ----

# Data import
library("openxlsx")
library("readxl")

# Data tidying
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

# Data verification
library("skimr")

# Plotting
library("ggplot2")

## Source paths and custom functions ----
source("R/paths.R")
source("R/constants.R")
source("R/fct_load_microplastics.R")


# Typologie sites ----
typologie_sites <- read_excel(paths$raw_typologie_sites, sheet = "typologies")

typologie_sites <- typologie_sites %>%
  separate(transect_debut, c("latitude_debut", "longitude_debut"), sep = ",", remove = TRUE) %>%
  separate(transect_fin, c("latitude_fin", "longitude_fin"), sep = ",", remove = TRUE) %>%
  mutate(
    latitude_debut = gsub("[^0-9.]", "", latitude_debut),
    longitude_debut = gsub("[^0-9.]", "", longitude_debut),
    latitude_fin = gsub("[^0-9.]", "", latitude_fin),
    longitude_fin = gsub("[^0-9.]", "", longitude_fin)
  ) %>%
  mutate(
    latitude_debut = as.numeric(latitude_debut),
    longitude_debut = as.numeric(longitude_debut),
    latitude_fin = as.numeric(latitude_fin),
    longitude_fin = as.numeric(longitude_fin)
  )

# Reposition in the order : Longitude_Debut, Latitude_Debut, Longitude_Fin, Latitude_Fin
typologie_sites <- typologie_sites %>%
  relocate(longitude_debut, latitude_debut, longitude_fin, latitude_fin, .before = everything()) %>%
  mutate(transect_surface = 100 * plage_largeur) %>% # 100 mètres de longueur théorique * largeur plage
  relocate(transect_surface, .after = plage_largeur)
# Structure check
skimr::skim(typologie_sites)

# Microplastics ----

# Function to read all the sheets of the excel file containing the microplastics data by site and
# transforming it into a tidy format for future analysis

# get the sheet names and remove "Modèle" and "TOTAL"
sheet_names <- excel_sheets(paths$raw_microplastiques)
sheet_names <- sheet_names[!sheet_names %in% c("Modèle", "TOTAL", "DATA")]
microplastiques <- load_microplastics(paths$raw_microplastiques, sheet_names)

microplastiques <- microplastiques %>%
  rename_all(stringr::str_to_lower)

# Computing a summary data of microplastics per date (i.e. also the combination of saison and annee)
# and site without the details of type and Description but keep also all other metadata with all
# Latitude and Longitude per point, and site, annee, saison
microplastiques_total <- microplastiques %>%
  group_by(
    date,
    site,
    annee,
    saison,
    latitude_a,
    latitude_b,
    latitude_c,
    latitude_d,
    longitude_a,
    longitude_b,
    longitude_c,
    longitude_d
  ) %>%
  summarise(across(
    c(
      meso_5mm,
      micro_1mm,
      total,
      meso_normalise,
      micro_normalise,
      total_normalise
    ),
    sum,
    na.rm = TRUE
  ))

# Put the latitude and longitude data at the end instead
microplastiques_total <- microplastiques_total %>%
  ungroup() %>%
  relocate(
    c(
      latitude_a,
      latitude_b,
      latitude_c,
      latitude_d,
      longitude_a,
      longitude_b,
      longitude_c,
      longitude_d
    ),
    .after = everything()
  )

# Macrodechets ----

# Loading data
macrodechets <- read_excel(paths$raw_macrodechets)

## Observation and check ----
# All names to lower case

macrodechets <- macrodechets %>%
  rename_with(
    .fn = function(x) {
      x <- str_replace_all(x, " ", "_")
      x <- str_replace_all(x, ",", "_")
      # x <- str_replace_all(x, "...", "_")
      x <- str_replace_all(x, "/", " ")
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
      x <- str_replace_all(x, "__", "_")
    },
    .cols = everything()
  ) %>%
  rename_all(stringr::str_to_lower) %>%
  ungroup()

names_macrodechets <- names(macrodechets)

sort(unique(macrodechets$nom_zone))
sort(unique(macrodechets$lieu_ville))
sort(unique(macrodechets$date))

# Number of observations per locality (Nom_zone) as a nice representation
sort(table(macrodechets$nom_zone), decreasing = TRUE)

checks_macrodechets <- FALSE

if (checks_macrodechets) {
  # Check the first few rows of the data
  head(macrodechets)
  # Check the last few rows of the data
  tail(macrodechets)
  # Check the structure of the data
  str(macrodechets)
  # skim(macrodechets)
  # Check the summary of the data
  summary(macrodechets)
  # Check the number of rows and columns
  dim(macrodechets)
  # Check the column names
  names(macrodechets)

  # Check the unique values in each column and make it a data frame
  unique_values <- sapply(macrodechets, function(x) {
    length(unique(x))
  }) %>%
    as.data.frame(.) %>%
    rename(unique_Values = 1)

  unique_values
}

## Date, Year and Season ----
macrodechets <- macrodechets %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  mutate(
    saison = case_when(
      lubridate::month(date) %in% 3:5 ~ "Printemps",
      lubridate::month(date) %in% 6:8 ~ "Ete",
      lubridate::month(date) %in% 9:11 ~ "Automne",
      lubridate::month(date) %in% c(12, 1, 2) ~ "Hiver"
    )
  ) %>%
  mutate(annee = lubridate::year(date))

# Put Date, Season and annee at the beginning of the data set
macrodechets <- macrodechets %>%
  select(id_releve, date, saison, annee, everything())

# Group the observations by locality (Nom_zone) and date (Date) and count the number of observations
# in each group and save the result in a new data frame where it is sorted by Date and site
t1 <- macrodechets %>%
  group_by(nom_zone, date) %>%
  summarise(count = n()) %>%
  arrange(date, nom_zone) %>%
  as.data.frame()

# Group the observations by locality (Nom_zone) and count the number of observations in each group
# And save the result in a new data frame where it is sorted by the number of observations in each
# site
t2 <- macrodechets %>%
  group_by(nom_zone) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  as.data.frame()

# New verifying dataset with Date, season, Year, site (Nom_zone)
t3 <- macrodechets %>%
  group_by(date, saison, year = lubridate::year(date), nom_zone) %>%
  summarise(count = n()) %>%
  arrange(nom_zone, date) %>%
  as.data.frame()

## Geographical position ----

# Latitude + Longitude then removing the other characters that are not numbers except for the
# decimal (.) and then converting them in numeric

macrodechets <- macrodechets %>%
  # separate(LIEU_COORD_GPS, c("Longitude", "Latitude"), sep = ",", remove = TRUE) %>%
  rename(latitude = "lieu_coord_gps_lat", longitude = "lieu_coord_gps_lon")

# Verify that all identical site names have the same coordinates
t4 <- macrodechets %>%
  group_by(nom_zone, latitude, longitude) %>%
  summarise(
    # latitude = unique(latitude),
    # longitude = unique(longitude),
    n = n()
  ) %>%
  arrange(nom_zone) %>%
  as.data.frame()


# Reference data set where names given in a different column site for each nom_zone
macrodechets <- macrodechets %>%
  mutate(site = nom_zone)

# Change names of site on condition of each name with case when dplyr and all other not known are
# not changed and place it after the dates and before the other columns
macrodechets <- macrodechets %>%
  mutate(nom_zone = str_to_sentence(nom_zone)) %>%
  # Take the character "La" in the beggining of the sentence if it exists
  mutate(nom_zone = str_replace(nom_zone, "La", "")) %>%
  mutate(nom_zone = str_remove(nom_zone, "L'")) %>%
  mutate(nom_zone = str_remove(nom_zone, "Plage de")) %>%
  mutate(nom_zone = str_remove(nom_zone, "Plage d'")) %>%
  mutate(nom_zone = str_remove(nom_zone, "Plage")) %>%
  # Remove all whitespaces at the beginning
  mutate(nom_zone = str_trim(nom_zone)) %>%
  mutate(nom_zone = str_to_sentence(nom_zone)) %>%
  mutate(
    site = case_when(
      # Plages riparu
      nom_zone == "D’aliso" ~ "Alisu",
      nom_zone == "Barcaggio" ~ "Barcaghju",
      nom_zone == "Bonifaziu ville et littoral" ~ "Bonifaziu",
      nom_zone == "Zone de collecte feringule" ~ "Ferringule",
      nom_zone == "D'aliso" ~ "Aliso",
      nom_zone == "Du ricanto" ~ "Ricantu",
      nom_zone == "Faringule coté nord" ~ "Ferringule",
      nom_zone == "Farinole" ~ "Ferringule",
      nom_zone == "La roya" ~ "La Roya",
      nom_zone == "La roya cesm" ~ "La Roya",
      nom_zone == "La stagnola" ~ "La Stagnola",
      nom_zone == "Stagnola" ~ "La Stagnola",
      nom_zone == "L'ostriconi" ~ "Ostriconi",
      nom_zone == "Pietracorbara" ~ "Petracurbara",
      nom_zone == "Punta di capineru" ~ "Punta di Capineru",
      nom_zone == "La marana" ~ "La Marana",
      nom_zone == "Lido de la marana" ~ "Lido de la Marana",
      nom_zone == "Isula rossa" ~ "Isula Rossa",
      nom_zone == "Place vincetti" ~ "Place Vincetti",
      # Autres plages
      nom_zone == "Stella mare" ~ "Stella Mare",
      nom_zone == "Trottel est" ~ "Trottel Est",
      nom_zone == "Santa savera" ~ "Santa Severa",
      nom_zone == "Embouchure nord golo" ~ "Embouchure Nord Golo",
      TRUE ~ nom_zone
    )
  ) %>%
  select(id_releve, date, saison, annee, site, commentaire, everything())


# Checking surface and width of OG dataset
macrodechets <- typologie_sites %>%
  select(site, transect_surface, plage_largeur, plage_longueur) %>%
  left_join(macrodechets, ., by = join_by("site"))

# Selecting only Mare Vivu data
macrodechets <- macrodechets %>%
  filter(nom_structure == "Mare Vivu" | site == "Lozari") # Lozari pas suivi par Mare Vivu

# Big differences in ZDS surface vs theoretical transect !
# Need to keep theoretical as best here
t5 <- macrodechets %>%
  select(site, date, transect_surface, surface, plage_longueur, longueur_lineaire, nom_structure)

# Keeping best info from typologie instead of original dataset
macrodechets <- macrodechets %>%
  mutate(
    transect_surface = ifelse(!is.na(transect_surface), transect_surface, surface),
    transect_largeur = ifelse(!is.na(plage_largeur), plage_largeur, NA)
  ) %>%
  select(-c("surface", "plage_longueur", "plage_largeur")) %>%
  relocate(transect_largeur, transect_surface, .after = longueur_lineaire)

t6 <- macrodechets %>%
  select(site, transect_surface, transect_largeur) %>%
  distinct()

macrodechets <- macrodechets %>%
  rename(
    volume_total = global_volume_total,
    poids_total = global_poids_total
  ) %>%
  mutate(
    volume_total_m2 = volume_total / transect_surface, # division surface pour valeur/m2
    poids_total_m2 = poids_total / transect_surface # division surface pour valeur/m2
    # recompute _100m if different values of transect length in reality
  ) %>%
  select(
    1:match("volume_total", names(.)),
    duree,
    volume_total,
    volume_total_m2,
    poids_total,
    poids_total_m2,
    everything()
  ) %>%
  mutate(across(c("niveau_carac", "autres_dechets"), function(x) {
    as.numeric(x)
  }))

# Same for the typologie_sites dataset, where Transect 100m début and Transect 100 fin correspond
# to the coordinates of the beginning and the end of a transect. 4 columns are obtained with
# Latitude and Longitude for the beginning and the end of the transect. The latitude and longitude
# are set at the beginning of the data set and inversed for the longitude being before the latitude
# each time

## Creating longer dataset from riparu macrodechets ----

# Checking the classes of all columns before pivoting longer
classes <- sapply(macrodechets, class) %>%
  as.data.frame() %>%
  rename(classes = ".") %>%
  # Give row number in new column
  mutate(row = seq_len(n())) %>%
  filter(classes != "numeric")

# Pivot longer
macrodechets_longer <- macrodechets %>%
  dplyr::select(-c("commentaire_volumineux", "marques_exhaustives")) %>%
  pivot_longer(
    cols = match("volume_total", names(.)):dim(.)[2],
    names_to = "type",
    values_to = "valeur"
  )

# Column of the first word of the type column, i.e. the general category
macrodechets_longer <- macrodechets_longer %>%
  mutate(categorie = str_extract(type, "^[^_]+")) %>%
  mutate(categorie = str_trim(categorie))

# Column of the second word of the type column, i.e. the sub-category
macrodechets_longer <- macrodechets_longer %>%
  mutate(categorie_sub = case_when(
    categorie == "nb" ~ sapply(str_split(type, "_"), function(x) {
      if (length(x) >= 3) {
        x[3]
      } else {
        NA
      }
    }),
    TRUE ~ sapply(str_split(type, "_"), function(x) {
      x[2]
    })
  ))

# Column of all the other words of the type column, i.e. the specific category
macrodechets_longer <- macrodechets_longer %>%
  mutate(categorie_specifique = case_when(
    categorie == "nb" ~ str_match(type, "^(?:[^_]*_){3}(.*)")[, 2],
    TRUE ~ sapply(str_split(type, "_"), function(x) {
      if (length(x) >= 3) {
        paste(x[3:length(x)], collapse = "_")
      } else {
        NA_character_
      }
    })
  )) %>%
  mutate(
    across(c(categorie, categorie_sub), str_to_lower),
    categorie_specifique = str_to_sentence(categorie_specifique)
  )

# Following is true if transect is not 100 m long so it compensates for absence. But not changed
# %>% mutate(valeur_100m = (valeur / largeur_transect) / longueur_lineaire * 100)

# Separating the longer into 2 data sets : one with categorie == Nb and one with the rest
macrodechets_counts <- macrodechets_longer %>%
  filter(categorie == "nb") %>%
  mutate(valeur_m2 = valeur / transect_surface) %>%
  select(
    id_releve:type,
    categorie,
    categorie_sub,
    categorie_specifique,
    valeur,
    # valeur_100m,
    valeur_m2
  ) %>%
  mutate(categorie_sub = case_when(
    categorie_sub %in% c("rep", "dcsmm") ~ toupper(categorie_sub),
    TRUE ~ categorie_sub
  ))

macrodechets_general <- macrodechets_longer %>%
  filter(categorie != "nb") %>%
  select(
    id_releve:type,
    categorie,
    categorie_sub,
    categorie_specifique,
    valeur
  )

# Saving processed data ----

## RDS files ----
saveRDS(microplastiques, paths$processed_microplastiques)
saveRDS(microplastiques_total, paths$processed_microplastiques_total)
saveRDS(macrodechets_general, paths$processed_macrodechets_general)
saveRDS(macrodechets_counts, paths$processed_macrodechets_nb)
saveRDS(typologie_sites, paths$processed_typologie_sites)

## CSV files ----
saveRDS(microplastiques, paths$processed_microplastiques_csv)
saveRDS(microplastiques_total, paths$processed_microplastiques_total_csv)
saveRDS(macrodechets_general, paths$processed_macrodechets_general_csv)
saveRDS(macrodechets_counts, paths$processed_macrodechets_nb_csv)
saveRDS(typologie_sites, paths$processed_typologie_sites_csv)
