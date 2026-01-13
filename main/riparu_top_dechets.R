#' ---
#' title : "RIPARU - Top déchets"
#' author : Aubin Woehrel
#' date : 2025-11-28
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - Top déchets
#'
#' Description :
#' Small script for representation of most important trash all times combined
#'
#' =============================================================================

# Initialization ----

## Clean up of environment ----
rm(list = ls())

## Library imports -----

# Data tidying
library("dplyr")
library("tidyr")
library("tibble")
library("stringr")

# Plotting
library("ggplot2")
library("ggpubr")
library("treemapify")
# library("highcharter")

# Saving plots
library("webshot")
library("htmlwidgets")

## Source paths & constants----
source("R/paths.R")
source("R/constants.R")

## Source custom functions ----
source("R/fct_dechets_wordcloud.R")
source("R/fct_dechets_cat.R")
source("R/fct_dechets_treemap.R")
source("R/fct_dechets_barplot.R")

## Loading data ----
macrodechets_nb <- readRDS(paths$processed_macrodechets_nb)
microplastiques <- readRDS(paths$processed_microplastiques)
microplastiques_total <- readRDS(paths$processed_microplastiques_total)

# Preparing data ----
unique(macrodechets_nb$categorie_sub)

cat_types <- macrodechets_nb %>%
  select(categorie_sub, categorie_specifique) %>%
  distinct()

## Groupe ----
macrodechets_grp_all <- dechets_cat(categorie_sub_sel = "groupe", sites_selected = "all")
macrodechets_grp_suivi <- dechets_cat(categorie_sub_sel = "groupe", sites_selected = sites_macrodechets)

## Marque ----
macrodechets_mrq_all <- dechets_cat(categorie_sub_sel = "marque", sites_selected = "all")
macrodechets_mrq_suivi <- dechets_cat(categorie_sub_sel = "marque", sites_selected = sites_macrodechets)

## REP ----
macrodechets_rep_all <- dechets_cat(categorie_sub_sel = "REP", sites_selected = "all")
macrodechets_rep_suivi <- dechets_cat(categorie_sub_sel = "REP", sites_selected = sites_macrodechets)

## Secteur ----
macrodechets_sect_all <- dechets_cat(categorie_sub_sel = "secteur", sites_selected = "all")
macrodechets_sect_suivi <- dechets_cat(categorie_sub_sel = "secteur", sites_selected = sites_macrodechets)

## Standard ----
macrodechets_std_all <- dechets_cat(categorie_sub_sel = "standard", sites_selected = "all")
macrodechets_std_suivi <- dechets_cat(categorie_sub_sel = "standard", sites_selected = sites_macrodechets)

## Generique ----
macrodechets_gen_all <- dechets_cat(categorie_sub_sel = "generique", sites_selected = "all")
macrodechets_gen_suivi <- dechets_cat(categorie_sub_sel = "generique", sites_selected = sites_macrodechets)

## Specifique ----
macrodechets_spe_all <- dechets_cat(categorie_sub_sel = "specifique", sites_selected = "all")
macrodechets_spe_suivi <- dechets_cat(categorie_sub_sel = "specifique", sites_selected = sites_macrodechets)

macrodechets_grp_site <- dechets_cat(categorie_sub_sel = "groupe", sites_selected = "all", sum_by_cat = FALSE)
macrodechets_grp_site_suivi <- dechets_cat(categorie_sub_sel = "groupe", sites_selected = sites_macrodechets, sum_by_cat = FALSE)

# Treemaps ----

# Datasets and corresponding parameters
treemap_params <- list(
  list(data = macrodechets_grp_suivi, subcat = "groupe", suffix = "sites_suivis"),
  list(data = macrodechets_grp_all, subcat = "groupe", suffix = "tous_sites"),
  list(data = macrodechets_mrq_suivi, subcat = "marque", suffix = "sites_suivis"),
  list(data = macrodechets_mrq_all, subcat = "marque", suffix = "tous_sites"),
  list(data = macrodechets_rep_suivi, subcat = "rep", suffix = "sites_suivis"),
  list(data = macrodechets_rep_all, subcat = "rep", suffix = "tous_sites"),
  list(data = macrodechets_sect_suivi, subcat = "secteur", suffix = "sites_suivis"),
  list(data = macrodechets_sect_all, subcat = "secteur", suffix = "tous_sites"),
  list(data = macrodechets_std_suivi, subcat = "standard", suffix = "sites_suivis"),
  list(data = macrodechets_std_all, subcat = "standard", suffix = "tous_sites"),
  list(data = macrodechets_gen_suivi, subcat = "generique", suffix = "sites_suivis"),
  list(data = macrodechets_gen_all, subcat = "generique", suffix = "tous_sites")
)

# Looping treemap plotting custom function
for (treemap_param in treemap_params) {
  print(paste(treemap_param$subcat, "-", treemap_param$suffix))
  dechets_treemap(
    dechets_data = treemap_param$data,
    subcat = treemap_param$subcat,
    save_name = paste0("macrodechets_treemap_", treemap_param$suffix, ".png")
  )
}

# paletteer::scale_fill_paletteer_c("grDevices::Heat")
# paletteer::scale_fill_paletteer_c("ggthemes::Classic Orange-White-Blue Light")
# paletteer::scale_fill_paletteer_c("ggthemes::Orange Light")

# Barplots ----

# Test of custom function
dechets_barplot(
  dechets_data = macrodechets_grp_suivi,
  subcat = "tests",
  save_name = "macrodechets_barplottests",
  scale_log = TRUE
)

# Datasets and corresponding parameters
barplot_params <- list(
  list(data = macrodechets_grp_suivi, subcat = "groupe", suffix = "sites_suivis"),
  list(data = macrodechets_grp_all, subcat = "groupe", suffix = "tous_sites"),
  list(data = macrodechets_mrq_suivi, subcat = "marque", suffix = "sites_suivis"),
  list(data = macrodechets_mrq_all, subcat = "marque", suffix = "tous_sites"),
  list(data = macrodechets_rep_suivi, subcat = "rep", suffix = "sites_suivis"),
  list(data = macrodechets_rep_all, subcat = "rep", suffix = "tous_sites"),
  list(data = macrodechets_sect_suivi, subcat = "secteur", suffix = "sites_suivis"),
  list(data = macrodechets_sect_all, subcat = "secteur", suffix = "tous_sites"),
  list(data = macrodechets_std_suivi, subcat = "standard", suffix = "sites_suivis"),
  list(data = macrodechets_std_all, subcat = "standard", suffix = "tous_sites"),
  list(data = macrodechets_gen_suivi, subcat = "generique", suffix = "sites_suivis"),
  list(data = macrodechets_gen_all, subcat = "generique", suffix = "tous_sites")
)

# Looping barplot plotting custom function
for (barplot_param in barplot_params) {
  print(paste(barplot_param$subcat, "-", barplot_param$suffix))

  for (scale_log in c(FALSE, TRUE)) {
    dechets_barplot(
      dechets_data = barplot_param$data,
      subcat = barplot_param$subcat,
      save_name = paste0("macrodechets_barplots_", barplot_param$suffix),
      scale_log = scale_log
    )
  }
}

# Wordclouds ----

## Groupes ----
dechets_wordcloud(
  dechets_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_grp_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Marques ----
dechets_wordcloud(
  dechets_data = macrodechets_mrq_all,
  offset = 1,
  graph_size = 0.9,
  subcat = "marque",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_mrq_suivi,
  offset = 1,
  graph_size = 0.9,
  subcat = "marque",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## REP ----
dechets_wordcloud(
  dechets_data = macrodechets_rep_all,
  to_remove = c("Vide"),
  offset = 10,
  graph_size = 0.5,
  subcat = "rep",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_rep_suivi,
  to_remove = c("Vide"),
  offset = 10,
  graph_size = 0.5,
  subcat = "rep",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Secteurs ----
dechets_wordcloud(
  dechets_data = macrodechets_sect_all,
  offset = 1,
  graph_size = 0.9,
  subcat = "secteur",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_sect_suivi,
  offset = 1,
  graph_size = 0.9,
  subcat = "secteur",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Standard ----
dechets_wordcloud(
  dechets_data = macrodechets_std_all,
  offset = 100,
  graph_size = 0.4,
  subcat = "standard",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_std_suivi,
  offset = 100,
  graph_size = 0.4,
  subcat = "standard",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

## Generique ----
dechets_wordcloud(
  dechets_data = macrodechets_gen_all,
  offset = 1,
  graph_size = 0.4,
  subcat = "generique",
  save_name = "macrodechets_wordcloud_tous_sites.png"
)

dechets_wordcloud(
  dechets_data = macrodechets_gen_suivi,
  offset = 1,
  graph_size = 0.4,
  subcat = "generique",
  save_name = "macrodechets_wordcloud_sites_suivis.png"
)

# Wordcloud style tests ----
## Groupe ----
### Macrodechets groupe all sites ----
dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_01.png"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_02.png"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_03.png",
  text_color = "#122c38"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_04.png",
  text_color = "random-light",
  background_color = "black"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_all,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 10,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_groupe_tous_sites_05.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

### Macrodéchets groupe suivi ----
dechets_wordcloud(
  dechet_data = macrodechets_grp_suivi,
  # to_remove = c("Fragments_plastique_non-identifies"),
  offset = 100,
  graph_size = 0.5,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis_01.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis_02.png",
  text_color = "random-light",
  background_color = "#1a1a1a"
)

dechets_wordcloud(
  dechet_data = macrodechets_grp_suivi,
  to_remove = c("Fragments_plastique_non-identifies"),
  offset = 1,
  graph_size = 0.8,
  subcat = "groupe",
  save_name = "macrodechets_wordcloud_sites_suivis_03.png",
  text_color = "random-dark",
  background_color = "#ffffff"
)


# Site-detailed group representations ----

# Note : hard coded for lack of time. Future refactoring if needed

## Ten largest groups per site all years combined ----
macrodechets_sites_top10 <- macrodechets_grp_site_suivi %>%
  group_by(categorie_specifique, site) %>%
  summarize(total_m2 = sum(valeur_m2, na.rm = TRUE)) %>%
  arrange(desc(total_m2)) %>%
  group_by(site) %>%
  slice_max(order_by = total_m2, n = 10) %>%
  arrange(site, desc(total_m2)) %>%
  mutate(total_m2_log = log(total_m2 + 1)) %>%
  mutate(total_site = sum(total_m2)) %>%
  mutate(freq = total_m2 / total_site)

# Looping over pieplots for better color paletter management
for (site_selected in unique(macrodechets_sites_top10$site)) {
  print(site_selected)
  g1 <- macrodechets_sites_top10 %>%
    filter(site == site_selected) %>%
    arrange(desc(freq)) %>%
    mutate(level = row_number()) %>%
    mutate(print_name = factor(paste(level, ":", str_replace_all(categorie_specifique, "_", " ")))) %>%
    ggplot(., aes(area = freq, fill = print_name, label = print_name)) +
    treemapify::geom_treemap(layout = "srow") +
    treemapify::geom_treemap_text(
      place = "centre",
      layout = "srow",
      size = 18,
      reflow = TRUE,
      color = "white"
    ) +
    labs(title = paste("Proportions relatives des dix déchets les plus abondants au site", site_selected)) +
    paletteer::scale_fill_paletteer_d("rcartocolor::Prism") +
    theme_pubr() +
    theme(legend.position = "none", plot.title = element_text(size = 20))

  ggsave(
    plot = g1,
    width = 1000,
    height = 1000,
    scale = 4,
    filename = paste0(paths$output_macrobysite, "top10dechetsparsite_treemap_", site_selected, ".png"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
}

## Mean value and standard deviation per site (barplots) ----

# Data from main dataset
macrodechets_sites_top10mean <- macrodechets_grp_site_suivi %>%
  group_by(categorie_specifique, site) %>%
  summarize(
    mean_m2 = sum(valeur_m2, na.rm = TRUE),
    sd_m2 = sd(valeur_m2, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_m2)) %>%
  group_by(site) %>%
  slice_max(order_by = mean_m2, n = 10) %>%
  arrange(site, desc(mean_m2))

# Geombar graph with inverted and fliped axes
for (site_selected in unique(macrodechets_sites_top10mean$site)) {
  print(site_selected)
  g1 <- macrodechets_sites_top10mean %>%
    filter(site == site_selected) %>%
    mutate(level = row_number()) %>%
    arrange(desc(mean_m2)) %>%
    filter(level <= 10) %>%
    mutate(print_name = factor(
      paste(level, ":", str_replace_all(categorie_specifique, "_", " ")),
      levels = paste(1:10, ":", str_replace_all(unique(categorie_specifique), "_", " "))
    )) %>%
    ggplot(., aes(x = print_name, y = mean_m2, fill = print_name, label = print_name)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = mean_m2 - sd_m2, ymax = mean_m2 + sd_m2), width = .2) +
    scale_x_discrete(limits = rev) +
    coord_flip() +
    labs(
      x = "Groupes de macrodéchets (parmi les 10 plus abondants)",
      y = "Nombre moyen d'objets / m<sup>2</sup>"
    ) +
    paletteer::scale_fill_paletteer_d("rcartocolor::Prism") +
    theme_pubr() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 20),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.title.x = ggtext::element_markdown()
    )

  ggsave(
    plot = g1,
    width = 1500,
    height = 1000,
    scale = 3,
    filename = paste0(paths$output_macrobysite, "top10dechetsparsite_barplotmoyennes_", site_selected, ".png"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
}


## Mean value and standard deviation per site and per year (barplots) ----

# Data from main dataset
macrodechets_sites_top10meanyear <- macrodechets_grp_site_suivi %>%
  group_by(categorie_specifique, site, annee) %>%
  summarize(
    mean_m2 = sum(valeur_m2, na.rm = TRUE),
    sd_m2 = sd(valeur_m2, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_m2)) %>%
  group_by(site) %>%
  arrange(site, annee, desc(mean_m2))

# Geom bar loop over sites
for (site_selected in unique(macrodechets_sites_top10mean$site)) {
  print(site_selected)


  # Site selection
  selected_categories <- macrodechets_sites_top10mean %>%
    filter(site == site_selected) %>%
    pull(categorie_specifique)

  # Subassigning the 40 lines of data needed (10 max per year of studies)
  submacro <- macrodechets_sites_top10meanyear %>%
    filter(
      site == site_selected,
      categorie_specifique %in% selected_categories
    ) %>%
    mutate(meansdplus = mean_m2 + sd_m2)

  # max_macro <- max(submacro$meansdplus)

  # List of graphs to be ploted (one per year)
  yeargraphs <- c()

  # Year loop (1 graph per year)
  for (annee_selected in unique(macrodechets_sites_top10meanyear$annee)) {
    print(annee_selected)

    # main ggplot
    g1 <- submacro %>%
      filter(annee == annee_selected) %>%
      arrange(desc(mean_m2)) %>%
      ggplot(., aes(y = categorie_specifique, x = mean_m2, fill = categorie_specifique)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_errorbar(aes(xmin = mean_m2 - sd_m2, xmax = mean_m2 + sd_m2), width = .2) +
      # scale_y_sqrt() +
      labs(
        x = "Moyenne annuelle d'objets / m<sup>2</sup>",
        subtitle = annee_selected
      ) +
      # xlim(0, max_macro) +
      paletteer::scale_fill_paletteer_d("rcartocolor::Prism") +
      theme_pubr() +
      theme(
        plot.title = element_text(size = 20),
        axis.title.x = ggtext::element_markdown(),
        axis.title.y = element_blank()
      )
    yeargraphs <- c(yeargraphs, g1)
  }

  # regrouping of all graphs in one
  gfin <- ggpubr::ggarrange(
    plotlist = yeargraphs,
    align = "hv",
    legend = "none"
  )

  ggsave(
    plot = gfin,
    width = 1600,
    height = 1000,
    scale = 4,
    filename = paste0(paths$output_macrobysite, "top10dechetsparsiteparan_barplotmoyennes_", site_selected, ".png"),
    units = "px",
    dpi = "print",
    limitsize = FALSE
  )
}
