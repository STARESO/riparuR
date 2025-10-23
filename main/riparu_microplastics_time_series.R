###                                      ###
###                                      ###
### --- Mare Vivu RIPARU Time Series --- ###
###                                      ###
###                                      ###                                         


# Initialization ----

## Clean up and working directory if needed-----
rm(list = ls())

setwd("C:/Users/aubin/Documents/Travail/Stareso/MareVivu/RIPARU/")

## Library imports -----

# Data imports
library(readxl)

# Data tidying 
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)

# Plotting
library(ggplot2)
library(ggforce)
library(ggpubr)
library(RColorBrewer)
library(scales)

## Data imports ----

microplastics <- readRDS("Analyses/OutputData/Riparu_microplastics.rds")
microplastics_total <- readRDS("Analyses/OutputData/Riparu_microplastics_total.rds")
macroplastics <- readRDS("Analyses/OutputData/Riparu_macroplastics.rds")
macroplastics_essentiel <- readRDS("Analyses/OutputData/Riparu_macroplastics_essentiel.rds")
typologie_sites <- readRDS("Analyses/OutputData/Riparu_typologie_sites.rds")




# All sites combined time series ----

microplastics_time_total <- microplastics %>%
  filter(Annee != 2024) %>%
  group_by(Type, Annee, Saison) %>%
  # Add variable number of sites used for the Moyenne_normalise
  summarise(Moyenne_normalise = mean(Total_normalise, na.rm = FALSE),
            N_sites = n()) %>%
  mutate(Saison = factor(Saison, levels = c("Printemps", "Eté", "Automne", "Hiver"))) %>%
  arrange(Annee, Saison) %>%
  mutate(Annee_Saison = paste0(Annee, "-", Saison)) %>%
  mutate(Annee_Saison = factor(Annee_Saison, levels = unique(Annee_Saison))) %>%
  arrange(Annee_Saison)

mycolors <- colorRampPalette(brewer.pal(n = 8, name = "Dark2"))(length(unique(microplastics_time_total$Type)))

# Types separated
g1 <- ggplot(microplastics_time_total, aes(x = Annee_Saison, y = log(1 + Moyenne_normalise), group = Type, 
                                     color = Type)) +
  geom_point() +
  geom_smooth(alpha = 0.15) +
  geom_text(aes(label = paste("n = ", N_sites), x = Annee_Saison, y = log(1 + Moyenne_normalise)), 
            color = "black", size = 3, vjust = 1.5) +
  theme_pubr() +
  theme(legend.position = "bottom") +
  labs(title = "Microplastiques - Moyenne tous sites",
       x = "", 
       y = "log(Concentration (nombre de fragments / m²) + 1)") +
  scale_color_manual(values = mycolors) +
  facet_wrap(~Type, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

ggsave(g1, 
       filename = "Analyses/Figures/MicroplasticsTimeSeries/Evolution_Plastiques_par_type_log.png",
       width = 3000,
       height = 1500,
       scale = 2,
       units = "px",
       limitsize = FALSE,
       dpi = 300,
       bg = "white")


# Types in one graph
g2 <- ggplot(microplastics_time_total, 
             aes(x = Annee_Saison, y = log(1 + Moyenne_normalise), group = Type, color = Type)) +
  #geom_point() +
  geom_smooth(se = FALSE) +
  theme_pubr() +
  theme(legend.position = "bottom") +
  labs(title = "Microplastiques - Moyenne tous sites",
       x = "", 
       y = "log(Concentration (nombre de fragments / m²) + 1)") +
  scale_color_manual(values = mycolors) +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  +
  # geom_text(data = microplastics_time_total %>% 
  #             group_by(Annee_Saison) %>% 
  #             summarize(Max = max(Moyenne_normalise), N_sites = unique(N_sites)), 
  #           aes(label = paste("n = ", N_sites), x = Annee_Saison, y = max(log(1 + Max))), 
  #           color = "black", size = 3, vjust = 1.5) 

ggsave(g2, 
       filename = "Analyses/Figures/MicroplasticsTimeSeries/Evolution_Plastiques_all_log.png",
       width = 3000,
       height = 1500,
       scale = 2,
       units = "px",
       limitsize = FALSE,
       dpi = 300,
       bg = "white")


expm1_trans <-  function() trans_new("expm1", "expm1", "log1p")


# Types separated
g3 <- ggplot(microplastics_time_total, aes(x = Annee_Saison, y = Moyenne_normalise, group = Type, 
                                           color = Type)) +
  geom_point() +
  geom_smooth(alpha = 0.15, se = FALSE) +
  geom_text(aes(label = paste("n = ", N_sites), x = Annee_Saison, y = Moyenne_normalise), 
            color = "black", size = 3, vjust = - 1.5) +
  theme_pubr() +
  theme(legend.position = "bottom") +
  labs(title = "Microplastiques - Moyenne tous sites",
       x = "", 
       y = "Concentration (nombre de fragments / m²)") +
  scale_color_manual(values = mycolors) +
  facet_wrap(~Type, scales = "free_y") +
  scale_y_continuous(trans = log1p_trans()) +
  coord_trans(y = expm1_trans()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(g3, 
       filename = "Analyses/Figures/MicroplasticsTimeSeries/Evolution_Plastiques_par_type_smooth.png",
       width = 3000,
       height = 1500,
       scale = 2,
       units = "px",
       limitsize = FALSE,
       dpi = 300,
       bg = "white")

g4 <- ggplot(microplastics_time_total, aes(x = Annee_Saison, y = Moyenne_normalise, group = Type, 
                                           color = Type)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste("n = ", N_sites), x = Annee_Saison, y = Moyenne_normalise), 
            color = "black", size = 3, vjust = 1.5) +
  theme_pubr() +
  theme(legend.position = "bottom") +
  labs(title = "Microplastiques - Moyenne tous sites",
       x = "", 
       y = "Concentration (nombre de fragments / m²)") +
  scale_color_manual(values = mycolors) +
  facet_wrap(~Type, scales = "free_y") +
  scale_y_continuous(trans = log1p_trans()) +
  coord_trans(y = expm1_trans()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(g4, 
       filename = "Analyses/Figures/MicroplasticsTimeSeries/Evolution_Plastiques_par_type_ligne.png",
       width = 3000,
       height = 1500,
       scale = 2,
       units = "px",
       limitsize = FALSE,
       dpi = 300,
       bg = "white")

  