# Programme de traitement des données en vue de la production des statistiques
# Enquête : RMS (Result Monitoring Survey) 2024 - HCR Cameroun
# Author : charles.moute@gmail.com

#****************************************************************************
#* Chargement des bibliothéques utiles
#****************************************************************************
pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr, 
  readr, labelled, pastecs, psych, tableone, outbreaks, ggplot2, 
  unhcrthemes, scales, gt, webshot2, sjlabelled, waffle, writexl, 
  haven, readxl, dm, janitor, visdat, DiagrammeR, robotoolbox, remotes
)

# Nettoyage de l'espace de travail
rm(list=ls())

#****************************************************************************
#* Execution des programmes
#****************************************************************************

# LANCEMENT DU PROGRAMME
# cat(glue::glue_col(
#   "{green [ EXECUTION DU PROGRAMME RMS_UNHCR ] }"
# ),"\n")

#* [Below indicators will be used to disaggregate during the analysis].
##Country of origin : `citizenship`
##Age categories : `HH07_cat` and `HH07_cat2`
##Gender : `HH04`
##Population groups: `pop_groups`
###Disability: disability

#  Traitement des données
cat(glue::glue_col(
  "{green [ TRAITEMENT DES DONNEES ] }"
),"\n")
source("data_preparation.R")

# Calcul des indicateurs
# cat(glue::glue_col(
#   "{green [ CALCUL DES INDICATEURS ] }"
# ),"\n")
# source("indicator_calculations.R")
#* [Calcul des indircateurs]
# cat(glue::glue_col(
#   "{green [ LANCEMENT DU PROGRAMME DE CALCUL DES INDICATEURS ] }"
# ),"\n")
source("launch_calculations.R")


# FIN EXECUTION DU PROGRAMME
cat(glue::glue_col(
  "{green [ FIN EXECUTION PROGRAMME ] }"
),"\n")