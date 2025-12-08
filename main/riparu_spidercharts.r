#' ---
#' title : "RIPARU - spidercharts"
#' author : Aubin Woehrel
#' date : 2025-12-08
#' ---
#'
#' =============================================================================
#'
#' MARE VIVU RIPARU - SPIDERCHARTS
#'
#' Description :
#' Script for representing data with spidercharts
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
macrodechets_general <- readRDS(paths$processed_macrodechets_general)
microplastiques <- readRDS(paths$processed_microplastiques)
microplastiques_total <- readRDS(paths$processed_microplastiques_total)
typologie_sites <- readRDS(paths$processed_typologie_sites)

# Preparing data ----
