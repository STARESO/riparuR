#' ---
#' title : "RIPARU - Macrodechets time series"
#' author : Aubin Woehrel
#' date : 2024-09-23
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - macrodechets TIME SERIES
#'
#' Description :
#' Time series of macrodechets counts
#'
#' =============================================================================


# Initialization ----

## Clean up of environment ----
rm(list = ls())

## Library imports -----

# Data import
library("openxlsx")
library("readxl")

# Data tidying
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

# Plotting
library("ggplot2")
library("ggpubr")
library("echarts4r")

## Source paths ----
source("R/paths.R")
source("R/constants.R")

## Loading data ----
macrodechets_general <- readRDS(paths$processed_macrodechets_general)
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)
typologie_sites <- readRDS(paths$processed_typologie_sites)

# Plot all time series on one same graph of values 100m of categorie_sub == REP and categorie_specifique in :
# Articles de bricolage et de jardin
# Articles de sport et de loisir
# Déchêts REP bâtiment
# Emballages ménagers
# Engins de pêche
# Jouets
# Produits du tabac
# Textiles sanitaires à usages uniques
# Secteur agriculture

# Filière REP ----

# Fast data check

## Preparing data ----
t1 <- macrodechets_nb %>%
  select(annee, date, site) %>%
  distinct() %>%
  arrange(date)

# View(t1)

macrodechets_nb %>%
  filter(categorie_sub == "REP") %>%
  pull(categorie_specifique) %>%
  unique()

# Filter data by type
macrodechets_nb_filt <- macrodechets_nb %>%
  filter(categorie_sub == "REP") %>%
  filter(categorie_specifique %in% c(
    "Articles_de_bricolage_et_de_jardin",
    "Articles_de_sport_et_de_loisirs",
    "Batiment",
    "Dechets_d'emballages_menagers",
    "Engins_de_peche",
    "Jouets",
    "Produits_du_tabac",
    "Textiles_sanitaires_a_usage_unique"
  ))

unique(macrodechets_nb_filt$categorie_specifique)

# Filter data by keeping sites usable for time series
macrodechets_nb_filt <- macrodechets_nb_filt %>%
  filter(site %in% sites_macrodechets)

unique(macrodechets_nb_filt$site)

# Filter all dates of 2022 and after (in case of previous tests)
macrodechets_nb_filt <- macrodechets_nb_filt %>%
  filter(annee >= 2022)

## Preparing plots ----

### Color mapping ----
palette_rep_manual <- c(
  "Articles_de_bricolage_et_de_jardin" = "red",
  "Articles_de_sport_et_de_loisirs" = "blue",
  "Batiment" = "green",
  "Dechets_d'emballages_menagers" = "orange",
  "Engins_de_peche" = "purple",
  "Jouets" = "black",
  "Produits_du_tabac" = "brown",
  "Textiles_sanitaires_a_usage_unique" = "pink"
)

palette_rep_2 <- paletteer::paletteer_d("colorblindr::OkabeIto")

palette_rep <- palette_rep_2

### Plotting function ----
macrodechets_plotting <- function(
  points = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  span = 0.6,
  palette,
  date_breaks = "3 months",
  date_limits = c(
    as.Date("2022-01-01"),
    as.Date(paste0(max(macrodechets_nb_filt$annee), "-12-31"))
  )
) {
  ggdechets <- ggplot(
    macrodechets_nb_filt,
    aes(x = date, y = valeur_100m, color = categorie_specifique)
  )

  # Showing points
  if (points) {
    ggdechets <- ggdechets + geom_point()
  }

  # If smoothing wanted
  if (smooth) {
    ggdechets <- ggdechets + geom_smooth(se = FALSE, span = span)
  } else {
    ggdechets <- ggdechets + geom_line()
  }

  ggdechets <- ggdechets +
    facet_wrap(~site, scales = "free_y", axes = "all") +
    scale_x_date(date_breaks = date_breaks, limits = date_limits) +
    scale_color_manual(values = palette_rep) +
    theme_pubr() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      legend.position = "right"
    ) +
    labs(
      title = "Nombre de macrodéchets trouvés par catégorie spécifique",
      x = "",
      y = "Nombre d'objets"
    )

  if (log_scale) {
    ggdechets <- ggdechets + scale_y_log10()
  }

  return(ggdechets)
}

## Plotting data ----

# Macrodechets REP count
g1 <- macrodechets_plotting(
  point = TRUE,
  log_scale = FALSE,
  smooth = FALSE,
  palette = palette_rep
)

ggsave(
  plot = g1,
  width = 2000,
  height = 1200,
  scale = 3.5,
  filename = paste0(paths$output_timeseries, "macrodechets_counts.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

paths$output_timeseries
# Macrodechets REP count with log scale
g2 <- macrodechets_plotting(
  point = TRUE,
  log_scale = TRUE,
  smooth = FALSE,
  palette = palette_rep
)

ggsave(
  plot = g2,
  width = 2000,
  height = 1000,
  scale = 4,
  filename = paste0(paths$output_timeseries, "macrodechets_counts_log.png"),
  units = "px",
  dpi = "print",
  limitsize = FALSE
)

# Macrodechets REP count with smooth

# Looping on a series of different spans for visual choices
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.05)) {
  gsmooth <- macrodechets_plotting(
    point = TRUE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette = palette_rep
  )

  ggsave(
    plot = gsmooth,
    width = 2000,
    height = 1000,
    scale = 4,
    filename = paste0(
      paths$output_timeseries,
      "macrodechets_counts_smooth",
      spanwant * 100,
      "_log.png"
    ),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
}

# Same but without points for better intuition of trends
for (spanwant in seq(from = 0.4, to = 0.9, by = 0.05)) {
  gsmooth <- macrodechets_plotting(
    point = FALSE,
    log_scale = TRUE,
    smooth = TRUE,
    span = spanwant,
    palette = palette_rep
  )

  ggsave(
    plot = gsmooth,
    width = 2000,
    height = 1000,
    scale = 4,
    filename = paste0(
      paths$output_timeseries,
      "macrodechets_counts_smooth",
      spanwant * 100,
      "_log_without_points.png"
    ),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
}


## echarts4r representations ----

# Pivot the data wider
t1 <- macrodechets_nb_filt %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  pivot_wider(names_from = categorie_specifique, values_from = valeur_100m) %>%
  group_by(site)


g1 <- t1 %>%
  e_charts(date, timeline = TRUE) %>%
  e_bar(
    serie = `Articles_de_bricolage_et_de_jardin`,
    name = "Articles de bricolage et de jardin",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Articles_de_sport_et_de_loisirs`,
    name = "Articles de sport et de loisirs",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Batiment`,
    name = "Bâtiment",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Dechets_d'emballages_menagers`,
    name = "Déchets d'emballages ménagers",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Engins_de_peche`,
    name = "Engins de pêche",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Jouets`,
    name = "Jouets",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Produits_du_tabac`,
    name = "Produits du tabac",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_bar(
    serie = `Textiles_sanitaires_a_usage_unique`,
    name = "Textiles sanitaires à usage unique",
    legend = TRUE,
    timeline = TRUE
  ) %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "time", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Nombre d'objets") %>%
  e_title("Nombre d'objets macroplastiques filière REP") %>%
  e_axis_labels(x = "date", y = "Objets/100m linéaire côtier") %>%
  e_theme("vintage") %>%
  e_legend(orient = "vertical", right = "10%")

g1

# Pivot the data wider and aggregate by Year-Month
t2 <- macrodechets_nb_filt %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  mutate(yearmonth = lubridate::floor_date(lubridate::ymd(date), "month")) %>% # Create Year-Month column
  pivot_wider(names_from = site, values_from = valeur_100m) %>%
  group_by(categorie_specifique)

g2 <- t2 %>%
  e_charts(yearmonth, timeline = TRUE) %>%
  # Bar series for each site
  e_bar(serie = `Ferringule`, name = "Ferringule") %>%
  e_bar(serie = `La Roya`, name = "La Roya") %>%
  e_bar(serie = `Saleccia`, name = "Saleccia") %>%
  e_bar(serie = `Pietracorbara`, name = "Petracurbara") %>%
  e_bar(serie = `Macinaghju`, name = "Macinaghju") %>%
  e_bar(serie = `Barcaghju`, name = "Barcaghju") %>%
  e_bar(serie = `Alisu`, name = "Alisu") %>%
  # Tooltip and axis configuration
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "categorie_specifique", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Objets/100m linéaire côtier") %>%
  e_title("Nombre d'objets macroplastiques filière REP") %>%
  # Adjust the legend to avoid overlap
  e_legend(
    orient = "vertical",
    right = "5%", # Adjust the right position
    top = "10%", # Move it down
    itemWidth = 20, # Set legend item size
    itemHeight = 14,
    padding = 5
  ) %>%
  # DataZoom configuration with adjusted position
  e_datazoom(
    x_index = 0,
    type = "slider",
    bottom = "10%" # Adjust the position of the datazoom slider
  ) %>%
  e_timeline_opts(
    bottom = "0%" # Adjust the bottom so the timeline is visible
    # label = list(
    #   rotate = 90
    # )
  ) %>%
  # Adjust grid size to reduce the size of the main plot and make room for other elements
  e_grid(
    top = "10%", # Space for title
    left = "5%", # Space for y-axis label
    right = "5%", # Space for the legend
    bottom = "20%" # Space for timeline and datazoom
  ) %>%
  e_theme("vintage")

g2

# Catégorisation groupes ----

# Filter data
macrodechets_nb_group <- macrodechets_nb %>%
  filter(categorie_sub == "Groupe") %>%
  filter(categorie_specifique %in% c(
    "Bâtons de sucette",
    "Cotons-tiges",
    "Cotons-tiges en carton",
    "Médias filtrants",
    "Pailles en plastique",
    "Sacs plastique"
  ))

# Echarts site group

# Pivot the data wider
t1 <- macrodechets_nb_group %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  pivot_wider(names_from = categorie_specifique, values_from = valeur_100m) %>%
  group_by(site)

g1 <- t1 %>%
  e_charts(date, timeline = TRUE) %>%
  e_bar(serie = `Bâtons de sucette`, name = "Bâtons de sucette") %>%
  e_bar(serie = `Cotons-tiges`, name = "Cotons-tiges") %>%
  e_bar(serie = `Cotons-tiges en carton`, name = "Cotons-tiges en carton") %>%
  e_bar(serie = `Médias filtrants`, name = "Médias filtrants") %>%
  e_bar(serie = `Pailles en plastique`, name = "Pailles en plastique") %>%
  e_bar(serie = `Sacs plastique`, name = "Sacs plastique") %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "time", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Nombre d'objets") %>%
  e_title("Nombre d'objets macroplastiques par groupe de catégorisation") %>%
  e_axis_labels(x = "date", y = "Objets/100m linéaire côtier") %>%
  e_theme("vintage") %>%
  e_legend(orient = "vertical", right = "10%")
g1

t2 <- macrodechets_nb_group %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  mutate(YearMonth = lubridate::floor_date(ymd(date), "month")) %>% # Create Year-Month column
  pivot_wider(names_from = site, values_from = valeur_100m) %>%
  group_by(categorie_specifique)

g2 <- t2 %>%
  e_charts(YearMonth, timeline = TRUE) %>%
  e_bar(serie = `Ferringule`, name = "Ferringule") %>%
  e_bar(serie = `La Roya`, name = "La Roya") %>%
  e_bar(serie = `Saleccia`, name = "Saleccia") %>%
  e_bar(serie = `Petracurbara`, name = "Petracurbara") %>%
  e_bar(serie = `Macinaghju`, name = "Macinaghju") %>%
  e_bar(serie = `Barcaghju`, name = "Barcaghju") %>%
  e_bar(serie = `Alisu`, name = "Alisu") %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "category", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Objets/100m linéaire côtier") %>%
  e_title("Nombre d'objets macroplastiques par groupe de catégorisation") %>%
  e_legend(
    orient = "vertical",
    right = "5%", # Adjust the right position
    top = "10%", # Move it down
    itemWidth = 20, # Set legend item size
    itemHeight = 14,
    padding = 5
  ) %>%
  e_datazoom(
    x_index = 0,
    type = "slider",
    bottom = "10%" # Adjust the position of the datazoom slider
  ) %>%
  e_timeline_opts(
    bottom = "0%" # Adjust the bottom so the timeline is visible
  ) %>%
  e_grid(
    top = "10%", # Space for title
    left = "5%", # Space for y-axis label
    right = "5%", # Space for the legend
    bottom = "20%" # Space for timeline and datazoom
  ) %>%
  e_theme("vintage")

g2




# Catégorisation Secteur ----
macrodechets_nb_secteurs <- macrodechets_nb %>%
  filter(categorie_sub == "Secteur") %>%
  filter(categorie_specifique %in% c(
    "Alimentation",
    "Aquaculture",
    "Bâtiment, travaux et matériaux de construction",
    "Chasse et armement",
    "Cosmétiques, hygiène et soins personnels",
    "Détergents et produits d'entretiens",
    "Jouets et loisir",
    "Plasturgie",
    "Pêche",
    "Tabac",
    "Traitement des eaux",
    "Vaisselle à usage unique"
  ))

t1 <- macrodechets_nb_secteurs %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  pivot_wider(names_from = categorie_specifique, values_from = valeur_100m) %>%
  group_by(site)

g1 <- t1 %>%
  e_charts(date, timeline = TRUE) %>%
  e_bar(serie = `Alimentation`, name = "Alimentation") %>%
  e_bar(serie = `Aquaculture`, name = "Aquaculture") %>%
  e_bar(serie = `Bâtiment, travaux et matériaux de construction`, name = "Bâtiment, travaux et matériaux de construction") %>%
  e_bar(serie = `Chasse et armement`, name = "Chasse et armement") %>%
  e_bar(serie = `Cosmétiques, hygiène et soins personnels`, name = "Cosmétiques, hygiène et soins personnels") %>%
  e_bar(serie = `Détergents et produits d'entretiens`, name = "Détergents et produits d'entretiens") %>%
  e_bar(serie = `Jouets et loisir`, name = "Jouets et loisir") %>%
  e_bar(serie = `Plasturgie`, name = "Plasturgie") %>%
  e_bar(serie = `Pêche`, name = "Pêche") %>%
  e_bar(serie = `Tabac`, name = "Tabac") %>%
  e_bar(serie = `Traitement des eaux`, name = "Traitement des eaux") %>%
  e_bar(serie = `Vaisselle à usage unique`, name = "Vaisselle à usage unique") %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "time", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Nombre d'objets") %>%
  e_title("Nombre d'objets macroplastiques - secteur d'activité") %>%
  e_axis_labels(x = "date", y = "Objets/100m linéaire côtier") %>%
  e_theme("vintage") %>%
  e_legend(orient = "horizontal", top = "8%", right = "10%", left = "10%")

g1



t2 <- macrodechets_nb_secteurs %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  mutate(YearMonth = lubridate::floor_date(ymd(date), "month")) %>% # Create Year-Month column
  pivot_wider(names_from = site, values_from = valeur_100m) %>%
  group_by(categorie_specifique)

g2 <- t2 %>%
  e_charts(YearMonth, timeline = TRUE) %>%
  e_bar(serie = `Ferringule`, name = "Ferringule") %>%
  e_bar(serie = `La Roya`, name = "La Roya") %>%
  e_bar(serie = `Saleccia`, name = "Saleccia") %>%
  e_bar(serie = `Petracurbara`, name = "Petracurbara") %>%
  e_bar(serie = `Macinaghju`, name = "Macinaghju") %>%
  e_bar(serie = `Barcaghju`, name = "Barcaghju") %>%
  e_bar(serie = `Alisu`, name = "Alisu") %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "category", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Objets/100m linéaire côtier") %>%
  e_title("Nombre d'objets macroplastiques - secteur d'activité") %>%
  e_legend(
    orient = "vertical",
    right = "5%", # Adjust the right position
    top = "10%", # Move it down
    itemWidth = 20, # Set legend item size
    itemHeight = 14,
    padding = 5
  ) %>%
  e_datazoom(
    x_index = 0,
    type = "slider",
    bottom = "10%" # Adjust the position of the datazoom slider
  ) %>%
  e_timeline_opts(
    bottom = "0%" # Adjust the bottom so the timeline is visible
  ) %>%
  e_grid(
    top = "10%", # Space for title
    left = "5%", # Space for y-axis label
    right = "5%", # Space for the legend
    bottom = "20%" # Space for timeline and datazoom
  ) %>%
  e_theme("vintage")

g2



# Catégorisation DCSMM ----

macrodechets_nb_dcsmm <- macrodechets_nb %>%
  filter(categorie_sub == "DCSMM") %>%
  filter(categorie_specifique %in% c(
    "Bouchons et couvercles de bouteille",
    "Bouchons et couvercles non alimentaire",
    "Bouteilles en plastique alimentaire inférieures à 0,5 l",
    "Bouteilles en plastique alimentaire supérieures à 0,5 l",
    "Bouteilles et contenants produit de nettoyage",
    "Cartouches et bourre de chasse",
    "Contenants alimentaire en polystyrène",
    "Emballages alimentaires",
    "Emballages alimentaires autres",
    "Emballages non-alimentaires identifiés",
    "Emballages sucreries et chips",
    "Fragments de polystyrène",
    "Fragments de polystyrène  supérieurs à 50 cm",
    "Fragments de polystyrène 0 - 2,5 cm",
    "Fragments de polystyrène 2,5 - 50 cm",
    "Lingettes jetables"
  ))


t1 <- macrodechets_nb_dcsmm %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  pivot_wider(names_from = categorie_specifique, values_from = valeur_100m) %>%
  group_by(site)

g1 <- t1 %>%
  e_charts(date, timeline = TRUE) %>%
  e_bar(serie = `Bouchons et couvercles de bouteille`, name = "Bouchons et couvercles de bouteille") %>%
  e_bar(serie = `Bouchons et couvercles non alimentaire`, name = "Bouchons et couvercles non alimentaire") %>%
  e_bar(serie = `Bouteilles en plastique alimentaire inférieures à 0,5 l`, name = "Bouteilles en plastique alimentaire inférieures à 0,5 l") %>%
  e_bar(serie = `Bouteilles en plastique alimentaire supérieures à 0,5 l`, name = "Bouteilles en plastique alimentaire supérieures à 0,5 l") %>%
  e_bar(serie = `Bouteilles et contenants produit de nettoyage`, name = "Bouteilles et contenants produit de nettoyage") %>%
  e_bar(serie = `Cartouches et bourre de chasse`, name = "Cartouches et bourre de chasse") %>%
  e_bar(serie = `Contenants alimentaire en polystyrène`, name = "Contenants alimentaire en polystyrène") %>%
  e_bar(serie = `Emballages alimentaires`, name = "Emballages alimentaires") %>%
  e_bar(serie = `Emballages alimentaires autres`, name = "Emballages alimentaires autres") %>%
  e_bar(serie = `Emballages non-alimentaires identifiés`, name = "Emballages non-alimentaires identifiés") %>%
  e_bar(serie = `Emballages sucreries et chips`, name = "Emballages sucreries et chips") %>%
  e_bar(serie = `Fragments de polystyrène`, name = "Fragments de polystyrène") %>%
  e_bar(serie = `Fragments de polystyrène supérieurs à 50 cm`, name = "Fragments de polystyrène  supérieurs à 50 cm") %>%
  e_bar(serie = `Fragments de polystyrène 0 - 2,5 cm`, name = "Fragments de polystyrène 0 - 2,5 cm") %>%
  e_bar(serie = `Fragments de polystyrène 2,5 - 50 cm`, name = "Fragments de polystyrène 2,5 - 50 cm") %>%
  e_bar(serie = `Lingettes jetables`, name = "Lingettes jetables") %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "time", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Nombre d'objets") %>%
  e_title("Nombre d'objets macroplastiques - DCSMM") %>%
  e_axis_labels(x = "date", y = "Objets/100m linéaire côtier") %>%
  e_theme("vintage") %>%
  e_legend(orient = "horizontal", top = "8%", right = "10%", left = "10%")
g1


t2 <- macrodechets_nb_dcsmm %>%
  select(site, date, categorie_specifique, valeur_100m) %>%
  mutate(YearMonth = lubridate::floor_date(ymd(date), "month")) %>% # Create Year-Month column
  pivot_wider(names_from = site, values_from = valeur_100m) %>%
  group_by(categorie_specifique)

g2 <- t2 %>%
  e_charts(YearMonth, timeline = TRUE) %>%
  e_bar(serie = `Ferringule`, name = "Ferringule") %>%
  e_bar(serie = `La Roya`, name = "La Roya") %>%
  e_bar(serie = `Saleccia`, name = "Saleccia") %>%
  e_bar(serie = `Petracurbara`, name = "Petracurbara") %>%
  e_bar(serie = `Macinaghju`, name = "Macinaghju") %>%
  e_bar(serie = `Barcaghju`, name = "Barcaghju") %>%
  e_bar(serie = `Alisu`, name = "Alisu") %>%
  e_tooltip(trigger = "item") %>%
  e_x_axis(type = "category", splitLine = list(show = TRUE)) %>%
  e_y_axis(name = "Objets/100m linéaire côtier") %>%
  e_title("Nombre d'objets macroplastiques - DCSMM") %>%
  e_legend(
    orient = "vertical",
    right = "5%", # Adjust the right position
    top = "10%", # Move it down
    itemWidth = 20, # Set legend item size
    itemHeight = 14,
    padding = 5
  ) %>%
  e_datazoom(
    x_index = 0,
    type = "slider",
    bottom = "10%" # Adjust the position of the datazoom slider
  ) %>%
  e_timeline_opts(
    bottom = "0%" # Adjust the bottom so the timeline is visible
  ) %>%
  e_grid(
    top = "10%", # Space for title
    left = "5%", # Space for y-axis label
    right = "5%", # Space for the legend
    bottom = "20%" # Space for timeline and datazoom
  ) %>%
  e_theme("vintage")
g2
