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
source("R/fct_load_microplastics.R")


# Typologie sites ----

typologie_sites <- read_excel(paths$typology_sites_raw, sheet = "RIPARU_Better")

typologie_sites <- typologie_sites %>%
  separate(
    `Transect 100m début`,
    c("Latitude_Debut", "Longitude_Debut"),
    sep = ",",
    remove = TRUE
  ) %>%
  separate(
    `Transect 100m fin`,
    c("Latitude_Fin", "Longitude_Fin"),
    sep = ",",
    remove = TRUE
  ) %>%
  mutate(
    Latitude_Debut = gsub("[^0-9.]", "", Latitude_Debut),
    Longitude_Debut = gsub("[^0-9.]", "", Longitude_Debut),
    Latitude_Fin = gsub("[^0-9.]", "", Latitude_Fin),
    Longitude_Fin = gsub("[^0-9.]", "", Longitude_Fin)
  ) %>%
  mutate(
    Latitude_Debut = as.numeric(Latitude_Debut),
    Longitude_Debut = as.numeric(Longitude_Debut),
    Latitude_Fin = as.numeric(Latitude_Fin),
    Longitude_Fin = as.numeric(Longitude_Fin)
  )

# Reposition in the order : Longitude_Debut, Latitude_Debut, Longitude_Fin, Latitude_Fin
typologie_sites <- typologie_sites %>%
  select(9, 8, 11, 10, 1:7, 12:dim(typologie_sites)[2])

# Lower all column names and pass to ascii
typologie_sites <- typologie_sites %>%
  rename_with(
    .fn = function(x) {
      stringi::stri_trans_general(x, "Latin-ASCII")
    },
    .cols = everything()
  ) %>%
  rename_all(stringr::str_to_lower)

# Structure check
skimr::skim(typologie_sites)

# Microplastics ----

# Function to read all the sheets of the excel file containing the microplastics data by site and
# transforming it into a tidy format for future analysis


# get the sheet names and remove "Modèle" and "TOTAL"
sheet_names <- getSheetNames(paths$microplastics_raw)
sheet_names <- sheet_names[!sheet_names %in% c("Modèle", "TOTAL")]
microplastics <- load_microplastics(paths$microplastics_raw, sheet_names)

# Computing a summary data of microplastics per Date (i.e. also the combination of saison and annee)
# and site without the details of type and Description but keep also all other metadata with all
# Latitude and Longitude per point, and site, annee, saison

microplastics_total <- microplastics %>%
  group_by(
    Date,
    Site,
    Annee,
    Saison,
    Latitude_a,
    Latitude_b,
    Latitude_c,
    Latitude_d,
    Longitude_a,
    Longitude_b,
    Longitude_c,
    Longitude_d
  ) %>%
  summarise(across(
    c(
      Meso_5mm,
      Micro_1mm,
      Total,
      Meso_normalise,
      Micro_normalise,
      Total_normalise
    ),
    sum,
    na.rm = TRUE
  ))

# Put the latitude and longitude data at the end instead
microplastics_total <- microplastics_total %>%
  select(
    -c(
      Latitude_a,
      Latitude_b,
      Latitude_c,
      Latitude_d,
      Longitude_a,
      Longitude_b,
      Longitude_c,
      Longitude_d
    ),
    Latitude_a,
    Latitude_b,
    Latitude_c,
    Latitude_d,
    Longitude_a,
    Longitude_b,
    Longitude_c,
    Longitude_d
  ) %>%
  ungroup()


# macrodechets ----

# Loading data
macrodechets <- read_excel(paths$macrodechets_raw)

## Observation and check ----
# All names to lower case
names(macrodechets) <- str_to_lower(names(macrodechets))

unique(macrodechets$nom_zone)
unique(macrodechets$lieu_ville)
unique(macrodechets$date)

# Number of observations per locality (Nom_zone) as a nice representation
sort(table(macrodechets$nom_zone))

# Check the first few rows of the data
head(macrodechets)
# Check the last few rows of the data
tail(macrodechets)
# Check the structure of the data
str(macrodechets)
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
  group_by(nom_zone) %>%
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude)
  ) %>%
  arrange(nom_zone) %>%
  as.data.frame()


# Create a reference data set where I can give names in a different column site for each nom_zone

macrodechets <- macrodechets %>%
  mutate(site = nom_zone)

# Change names of site on condition of each name with case when dplyr and all other not known are
# not changed and place it after the dates and before the other columns
macrodechets <- macrodechets %>%
  mutate(nom_zone = str_to_sentence(nom_zone)) %>%
  # Take the character "La" in the beggining of the sentence if it exists
  mutate(nom_zone = str_replace(nom_zone, "La", "")) %>%
  mutate(nom_zone = str_remove(nom_zone, "L'")) %>%
  # Remove all whitespaces at the beginning
  mutate(nom_zone = str_trim(nom_zone)) %>%
  mutate(nom_zone = str_to_sentence(nom_zone)) %>%
  mutate(
    site = case_when(
      # Plages riparu
      nom_zone == "Plage d’aliso" ~ "Alisu",
      nom_zone == "Plage de barcaggio" ~ "Barcaghju",
      nom_zone == "Plage de ferringule" ~ "Ferringule",
      nom_zone == "Zone de collecte feringule" ~ "Ferringule",
      nom_zone == "Plage de farinole" ~ "Ferringule",
      nom_zone == "Plage de la roya" ~ "La Roya",
      nom_zone == "Roya" ~ "La Roya",
      nom_zone == "Plage de macinaggio" ~ "Macinaghju",
      nom_zone == "Plage de macinaggio" ~ "Macinaghju",
      nom_zone == "Plage de l'ostriconi" ~ "L'Ostriconi",
      nom_zone == "Ostriconi" ~ "L'Ostriconi",
      nom_zone == "l'ostriconi" ~ "Ostriconi",
      nom_zone == "Plage de pietracorbara" ~ "Petracurbara",
      nom_zone == "Plage de saleccia" ~ "Saleccia",

      # Autres plages
      nom_zone == "Plage de chevano" ~ "Chevanu",
      nom_zone == "Plage de Tizzano" ~ "Tizzano",
      nom_zone == "Plage d'Algajola" ~ "Algajola",
      nom_zone == "Plage de Stella Mare" ~ "Stella Mare",
      nom_zone == "Plage Trottel Est" ~ "Trottel Est",
      nom_zone == "Plage de miomu" ~ "Miomu",
      nom_zone == "Plage de santa savera" ~ "Santa Savera",
      TRUE ~ nom_zone
    )
  ) %>%
  select(id_releve, date, saison, annee, site, commentaire, everything())


largeur_transect <- 2.5
macrodechets <- macrodechets %>%
  mutate(
    volume_total_m2 = volume_total / surface,
    poids_total_m2 = poids_total / surface,
    volume_total_100m = (volume_total / largeur_transect) / longueur_lineaire * 100,
    poids_total_100m = (volume_total / largeur_transect) * 100
  ) %>%
  select(
    1:27,
    duree,
    volume_total,
    volume_total_m2,
    volume_total_100m,
    poids_total,
    poids_total_m2,
    poids_total_100m,
    everything()
  ) %>%
  mutate(across(c("niveau_carac", "autres_dechets"), function(x) {
    as.numeric(x)
  }))


# Removing the Ostriconi site because its not part of the project
macrodechets <- macrodechets %>%
  filter(site != "L'Ostriconi")

# Same for the typologie_sites dataset, where Transect 100m début and Transect 100 fin correspond
# to the coordinates of the beginning and the end of a transect. 4 columns are obtained with
# Latitude and Longitude for the beginning and the end of the transect. The latitude and longitude
# are set at the beginning of the data set and inversed for the longitude being before the latitude
# each time

macrodechets_essentiel <- macrodechets %>%
  # Select all the columns to 33 and then select all the columns containing "Global_volume"
  # and "Global_poids" in their names
  select(1:33, contains("global_volume")) %>%
  pivot_longer(
    cols = contains("global_volume"),
    names_to = "type",
    values_to = "volume_categorie"
  ) %>%
  # Remove the "Global_volume_" in the type column
  mutate(type = str_remove(type, "global_volume_")) %>%
  # Compute the volume_type per surface and per 100m
  mutate(
    volume_type_m2 = volume_categorie / surface,
    volume_type_100m = volume_categorie / longueur_lineaire * 100
  )

# Compute the sum of all volume_categorie to verify that it is the same as the volume_total
t <- macrodechets_essentiel %>%
  group_by(date, saison, annee, site) %>%
  summarise(volume_total_calcule = sum(volume_categorie, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(
    macrodechets %>% select(date, saison, annee, site, volume_total),
    by = c("date", "saison", "annee", "site")
  ) %>%
  mutate(comparison = volume_total_calcule == volume_total)


## Creating longer dataset from riparu macrodechets ----

# Checking the classes of all columns before pivoting longer
classes <- sapply(macrodechets, class) %>%
  as.data.frame() %>%
  rename(classes = ".") %>%
  # Give row number in new column
  mutate(row = 1:n()) %>%
  filter(classes != "numeric")

# Pivot longer
macrodechets_longer <- macrodechets %>%
  select(-"commentaire_volumineux") %>%
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

# Separating the longer into 2 data sets : one with categorie == Nb and one with the rest
macrodechets_counts <- macrodechets_longer %>%
  filter(categorie == "nb") %>%
  mutate(valeur_100m = (valeur / largeur_transect) / longueur_lineaire * 100) %>%
  select(
    id_releve:type,
    categorie,
    categorie_sub,
    categorie_specifique,
    valeur,
    valeur_100m
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
saveRDS(microplastics, paths$microplatics_processed)
saveRDS(microplastics_total, paths$microplastics_total_processed)
saveRDS(macrodechets_general, paths$macrodechets_general_processed)
saveRDS(macrodechets_counts, paths$macrodechets_counts_processed)
saveRDS(macrodechets_essentiel, paths$macrodechets_essential_processed)
saveRDS(typologie_sites, paths$typology_sites_processed)

## CSV files ----
write.csv(microplastics, paths$microplatics_processed_csv, row.names = FALSE)
write.csv(microplastics_total, paths$microplastics_total_processed_csv, row.names = FALSE)
write.csv(macrodechets_general, paths$macrodechets_general_processed_csv, row.names = FALSE)
write.csv(macrodechets_counts, paths$macrodechets_counts_processed_csv, row.names = FALSE)
write.csv(macrodechets_essentiel, paths$macrodechets_essential_processed_csv, row.names = FALSE)
write.csv(typologie_sites, paths$typology_sites_processed_csv, row.names = FALSE)
