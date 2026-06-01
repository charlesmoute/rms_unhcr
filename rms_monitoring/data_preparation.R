# Standard Scripts for RMS CAPI Data Preparation (RMS version CAPI 3.2)
# Original version : Ilgi Bozdag - October 2024
# Adaptation aux données du Cameroun 
# Author : Charles Mouté (charles.moute@gmail.com)
# Données colléectées par DEDI Cameroun - Octobre 2024


# Nettoyage de l'espace de travaim
rm(list = ls())

#****************************************************************************
## Chargement des bibliothèques d'intérêt ####
#****************************************************************************
# Install pacman if not already installed
# if(!require(pacman)) install.packages('pacman')

# pacman::p_load(
#   tidyverse, dplyr, tidyr, rlang, purrr, magrittr, expss, srvyr, 
#   readr, labelled, pastecs, psych, tableone, outbreaks, ggplot2, 
#   unhcrthemes, scales, gt, webshot2, sjlabelled, waffle, writexl, 
#   haven, readxl, dm, janitor, visdat, DiagrammeR, robotoolbox, remotes
# )

# Import des fonctions utilitaires
source("analysis_utilities.R")

#****************************************************************************
## Téléchargement et importation des données ####
#****************************************************************************
#  On utilise le package robotoobox. Le nom utilisateur, ainsi que les mots de
#  passe et url à Kobo sont sauvegardés dans des variables d'environnmenet.

# Configuration automatique
kobo_setup()
#  Telechargement des donnees
uid <- Sys.getenv("form_id")
asset <- kobo_asset(uid)
df <- kobo_data(asset,progress=TRUE)

# names(df)
# La base de données est constituée de 4 tables de données

# #Check repeat group datasets
# dm_draw(df)
# ### Check the columns
# glimpse(df)
# ##Check number of entries in each sheet
# dm_nrow(df)


## Extraction des différentes tables de données

# La base main regroupe toutes les questions au niveau ménage
main <- pull_tbl(df, main, keyed = TRUE)
S1 <- pull_tbl(df, S1, keyed = TRUE)
S2_repeat <- pull_tbl(df, S2_repeat, keyed = TRUE)
rpt_hhmnames <- pull_tbl(df, rpt_hhmnames, keyed = TRUE)


# #Quelques veriffications
# # Est-ce que la position de l'individu est correctement reporte dans chaque boucle
# sum(S1$personId != S1$hhmnames_pos_match) 
# #La valeur ci-dessus  indique la présence de valeur manquante
# S1 %>% filter(is.na(S1$personId)) %>% glimpse()
# # Pas de valeurs manquantes dans personId
# S1 %>% filter(is.na(S1$hhmnames_pos_match)) %>% glimpse()
# # Des valeurs manquantes dans hhmnames_pos_match du fait de la suppression d'une membre de ménage
# # depuis l'application KoboCollect... cette erreur est assez fréquente ...Par ailleurs,
# # pour ces cas les variables sont systématiquement vide ... donc on peut les supprimer sans aucun soucis
# # NB : personId = hhroster_pos_aux  ce qui était entendu vu l'utilisation de position(..)
# # dans le fichier xlsform
# # Ci-dessous on verifie que hhmnames_pos_match et toujours égale à personId
# # une fois que l'on aura traite le cas des valeurs manquantes dans hhmnames_pos_match
# with(S1 %>% filter(!is.na(hhmnames_pos_match)), sum(hhmnames_pos_match!=personId))
# # Le resultat est 0 donc on aura systematique hhmnames_pos_match= personId
# # on va toutefois utiliser hhmnames_pos_match au cas où ...

#* # [On verifie que l'on dans chaque ménage des noms distincts pour les individus]
# S2_repeat %>%
#   group_by(`_parent_index`,name_individual) %>% 
#   mutate(eff=n()) %>% ungroup() %>% 
#   filter(eff>1) %>% 
#   select(`_parent_index`,name_individual,ind_gender,ind_age_year,ind_age_month)
# 
#* # [Au regard des résultats on a des doublons exacts, pour une raison ou une autre]
# # certaines informations ont été dupliqués pour le traitement on en conserve qu'une seul observation
# S2_repeat %>%
#   group_by(`_parent_index`,name_individual,ind_gender,ind_age_year,ind_age_month) %>% 
#   slice_head(n=1) %>%  # on conserve la première observation 
#   mutate(eff=n()) %>% ungroup() %>% 
#   filter(eff>1) #On verifie que le resultat sera correct


# On reconstruit la base individu
# HHH01_2_aux = Nom du chef de ménage
# HH01_2_aux = Nom de l'individu,HH03_2_aux = lien de parenté (1= chef de ménage)
ind <- S1 %>% 
  mutate(
    ind_name = HH01,
    oag_HH04 = HH04,
    HH04 = individu_sexe,
    HH04 = if_else(is.na(individu_sexe),as.character(oag_HH04),HH04)
  ) %>% #individu_sexe
  filter(!is.na(hhmnames_pos_match)) %>% #Si NA alors données supprimées par l'agent lors de la saisie
  left_join(
    rpt_hhmnames %>% 
      select(hhmnames_pos_match=hhmnames_pos,head_name = HHH01_2_aux,
             `_parent_index`,
             ind_name = HH01_2_aux,
             relationship=HH03_aux,relationship_code=HH03_2_aux) %>% 
      # Par defaut la question sur le lien de parenté n'est pas posé au chef de ménage
      # donc on va procéder à une imputation pour porter la correction
      mutate(
        relationship = if_else(relationship_code==1,"1",relationship),
        ind_relationship =relationship,
        ind_relationship_code = relationship_code,
      ),
    #On fusionne sur l'identifiant ménage (`_parent_index`), 
    # le numéro d'ordre du membre de ménage (hhmnames_pos_match) et pour plus de fiabilité
    # le nom du membre de ménage (member_name), en principe il doit toujour avoir le
    #  même numéro d'ordre
    by=c("_parent_index","hhmnames_pos_match","ind_name")
  ) %>% 
  mutate(
    ind_gender = parse_number(HH04),#MOUTE
    ind_age = HH07,
  ) %>% 
  left_join(
    S2_repeat %>% 
      # suppresion dans les ménages des doublons sur la base du nom, du genre,
      # et de l'âge de l'individu
      group_by(`_parent_index`,name_individual,ind_gender,ind_age_year) %>% 
      slice_head(n=1) %>% ungroup() %>% 
      mutate(
        # On crée des variables utilitaires pour faciliter la fusion
        ind_name=name_individual,
        ind_age = ind_age_year
      ), 
    # Pour la fusion ici, on utilisera l'identifiant du ménage et le nom du membre de ménage
    # Pour ce module, dans le fichier xlsform, on a pas de variable identifiant le numéro d'ordre
    # de l'individu dans le ménage. on fusionne sur la base de l'identifiant ménage,
    #  nom de l'individu, age de l'individu et age de l'individu
    by = c("_parent_index","ind_name","ind_gender","ind_age")
  ) 

# Pour la suite : hhmnames_pos_match, ind_name, ind_gender, ind_age_year,ind_age_month
# ind_relationship, ind_relationship_code

#* #[Verifions que l'on a pas de varibale manquante sur les variables clés]
# sum(is.na(ind$hhmnames_pos_match)) # Correct
# sum(is.na(ind$ind_name)) # Correct
# sum(is.na(ind$ind_gender)) # Correct
# sum(is.na(ind$ind_age_year)) # Correct
# sum(is.na(ind$ind_relationship)) # Correct
# sum(is.na(ind$ind_relationship_code)) # Correct
# # Fusion correct des données au niveau individu

#Now you should have only two datasets one is called 'main' for household level questions 
###and other one is called 'ind' for individual level questions
###Remove all other datasets that are not needed
# rm(asset,asset_list,df,P2.3,S1, S2_repeat)
rm(asset,df,S1, S2_repeat,rpt_hhmnames)

#***************************************** 
## Export des données brutes ####
#*****************************************

# not_logical_columns <- names(main)[!sapply(main, is.logical)]
main <-  main %>% select(-c(`_attachments`))

cat(glue::glue_col(
  "{green [ EXPORT DES BASES DE DONNEES BRUTES ] }"
),"\n")
#Enregistrement des bases de données brutes
# suppressWarnings(saving_datasets("raw"))
saving_datasets("raw")

# Chargement de la variable configuration
# board <- pins::legacy_github("charlesmoute/data_sharing",path = "dedi")
# config <- pins::pin_get(name="unhcr23724_config",board)
config <- rio::import("rms_chad26_params.rds",trust=TRUE)

#' #' [Lecture des données depuis le dépôt public github]
#' web_board <- pins::board_url(c(
#'   config = github_raw("charlesmoute/datasets/main/oag/rms23524_config.rds"),
#'   data= github_raw("charlesmoute/datasets/main/oag/rms23524_data.rds")
#' ))
#' #' 
#' # Téléchargement et sauvegarde de l'objet
#' db_config <- web_board %>%
#'   pins::pin_download(name="config") %>%
#'   rio::import(trust=TRUE)
#' 
#' db_data <- web_board %>% # objet openxlxs
#'   pins::pin_download(name="data") %>%
#'   rio::import(trust=TRUE)
#' # rm(web_board, db_config,db_data)
#' 

# Kisre des questionnaires ayant 
rejected_list <-  config$monitoring_data %>% 
  filter(type=="tx_missing_intra",value>=10) %>% 
  pluck("caseid") %>% unique()


#' [Sauvegarde de l'environnement de travail]
save.image("rms_space.RData") #load("rms_space.RData")

#****************************************************************************
## Data cleaning for ind and main datasets ####
#****************************************************************************
###Before you start creating variables for further disaggregation, DO THE PRIMARY DATA CLEANING (missing values, duplicates)
###You can get inspired from below steps to help you with your primary cleaning
###You will continue with data cleaning once you create your disaggregation variables 

###Step 0. Suppression des observations du pilote et des observations indiqués par
# les agents comme non compléte.

# Traitement sommaire
main <- main %>%
  mutate(
    caseid=`_id`,
    zone_save = zone, #* [On fait une sauvegarde...]
    zone = region,#*[zone  sera region... pour que le programme fonctionne correctement ]
    
    #* [CECI EST INUTILE AU REGARD DES DONNEES]
    # La localité DAR ES SALAM est censé appartenir à la région du LAC et non à celle 
    # de MANDOUL
    # zone = if_else(zone == '10' ,'7',zone), #*[Confusion dans le xmlform]
    zone_name = to_character(zone),
    # zone_name = if_else(zone_name=="MANDOUL","LAC",zone_name),
    
    region_name = to_character(region),
    localite_name = to_character(localite),
    enumerator_name = name_enumerator,
    enumerator = parse_number(enumerator),
    population_tmp = parse_number(pop_groups),
    population_name = case_when(
      population_tmp %in% c(1,2,3) ~ var_REFUGEES, #"Refugees/Asylum seekers", #"Réfugié(e)s/Demandeurs d'asile",
      population_tmp %in% c(5) ~ var_IDPs, #"IDPs", #"PDIs",
      population_tmp %in% c(4,6) ~ var_RETURNED, #"Returned", #"Retourné(e)s",
      population_tmp %in% c(8) ~ var_HOST, #"Host communities",#"Communautés hôtes"#,
      # population_tmp %in% c(7) ~ "Error [Apatrides]",
    ),
    population_code = case_when(
      population_tmp %in% c(1,2,3) ~ "refugee",
      population_tmp %in% c(5) ~ "idp",
      population_tmp %in% c(4,6) ~ "returnee",
      population_tmp %in% c(8) ~ "host"#,
      # population_tmp %in% c(7) ~ "other",
    ),
    population = case_when(
      population_tmp %in% c(1,2,3) ~ 1,
      population_tmp %in% c(5) ~ 2,
      population_tmp %in% c(4,6) ~ 3,
      population_tmp %in% c(8) ~ 4#,
      # population_tmp %in% c(7) ~ 0,
    ),
    hhid = Intro02,
    hh_located = as.numeric(parse_number(Intro03)==1),
    hh_size = hh_size_001,
    consent = as.numeric(parse_number(Intro03)==1 & parse_number(Intro04)==1),
    consent_motif = to_character(Intro05),
    consent_motif = ifelse(parse_number(Intro05)==96,
                           to_character(Intro05_other),consent_motif),
    end_result_code = parse_number(end_result),
    end_result = to_character(end_result),
    today = end,
    validation_status = to_character(`_validation_status`),
    
  # ) %>% select(-population_tmp) %>% 
  # filter(
  #   # On conserve uniquement les données envoyes par les comptes prévus pour la 
  #   # collecte des données
  #   `_submitted_by` %in% config$user_list,
  #   # On ne conserve que les données envoyés pendant la collecte des données
  #   date(start) >= config$start_date & date(end) <= config$end_date,
  #   # On ne conserve que les données avec consentement ainsi que celle qui sont dite
  #   # complete
  #   consent == 1 & end_result_code %in% c(1), #Nous ne considérons que les questionnaires complets
  #   # On s'assurer d'exclurer les observations marquées explicitement sur le serveur comme "Not approved"
  #   is.na(`_validation_status`)
  # ) %>% #Suppression des doublons systèmes
  # group_by(`_uuid`) %>% arrange(desc(end)) %>% 
  # slice_head(n=1) %>% ungroup()  %>% 
  # mutate(
  #' #'**********************************************************************
  #' #' [Imputation pendant la collecte]
  #' #'**********************************************************************
  
  # Un agent a selectionné partiel alors que ses données étaient complétes
  # Donc imputation a complete
  end_result_code = ifelse(
    caseid %in% c(116487430,116487428,116487427,116487426,116487425,116487423),
    1,end_result_code
  )
  
  ) %>% 
  select(-population_tmp)


#* [Suppression des observations inexploitables]
main <-  main %>% 
  filter(
    # On conserve uniquement les données envoyes par les comptes prévus pour la 
    # collecte des données
    `_submitted_by` %in% config$user_list,
    # On s'assure de ne garder que les observations collectées dans les localités du plan d'échantillonnage
    localite %in% unique(config$localite$value), 
    # On ne conserve que les données envoyés pendant la collecte des données
    date(start) >= config$start_date & date(end) <= config$end_date,
    # On s'assurer d'exclurer les observations marquées explicitement sur le serveur comme "Not approved"
    is.na(validation_status),
    # On ne conserve que les données avec consentement ainsi que celle qui sont dite
    # complete
    consent == 1 & end_result_code %in% c(1), #Nous ne considérons que les questionnaires complets
    # On retire egalement tous les questionnaires rejetees
    !caseid %in% rejected_list
  ) %>% #Suppression des doublons systèmes
  group_by(`_uuid`) %>% arrange(desc(end)) %>% 
  slice_head(n=1) %>% ungroup()

# On ne conserve que les individus dont les ménages ont été retenus pour 
# l'analyse
ind <-  ind %>% 
  #' mutate(
  #'   #' #'**********************************************************************
  #'   #' #' [Imputation pendant la collecte]
  #'   #' #'**********************************************************************
  #'   # La localité DAR ES SALAM est censé appartenir à la région du LAC et non à celle 
  #'   # de MANDOUL
  #'   zone = if_else(localite == '202' ,'7',zone), #*[Confusion dans le xmlform]
  #'   zone_id =  if_else(zone_id==10,7,zone_id),
  #'   zone_name = if_else(zone_name=="MANDOUL","LAC",zone_name),
  #' ) %>% 
  filter(`_parent_index` %in% unique(main$`_index`))
# rpt_hhmnames <- rpt_hhmnames %>% filter(`_parent_index` %in% unique(main$`_index`))

### Step 1. Check duplicates ####
#* [Verification des doublons - déjà traité plus haut]
# duplicated(main) # Check if there are any duplicates
# sum(duplicated(main)) # Number of duplicates


# duplicated(ind) # Check if there are any duplicates
# sum(duplicated(ind)) #Number of duplicates

# get_dupes(main)
# get_dupes(ind)
###DELETE IF YOU HAVE ANY DUPLICATES!

### Step 2. Check for missing data ####


####R provides functions like is.na(), complete.cases(), and na.omit() for handling missing values. 
###The tidyr package's drop_na() function is also useful for removing rows with missing data
###Check for certain variables to see if there are any missing values

###Missing ind Analysis: Visualize the extent of missing ind using bar charts or heatmaps to identify patterns of missingness.

# Example of a missing ind heatmap using the `visdat` package
#* vis_dat(main,warn_large_data=FALSE)


### Calculate disaggregation variables ####

####Calculate population groups from the mobility section to confirm population group
### If you were surveying internally displaced persons, you can run the code below and compare with the actual
##population groups entered at the beginning

####IDPs 

###EGRISS defines IDPs as those who have been forcibly displaced , including preventative movements, by:
###Armed conflit; generalised violence; violations of human rights; natural or human-made disasters; other forced displacement or evictions


# table(ind$IDP01_1) # Armed conflict
# table(ind$IDP01_2) # Generalised Violence
# table(ind$IDP01_3) # Persecution and or violations of human rights
# table(ind$IDP01_4) # Natural or human-made disasters
# table(ind$IDP01_5) # Other forced displacement or evictions
# table(ind$IDP01_6) # Other voluntary movements
# table(ind$IDP01_7) # Never moved home while in ${countryname}
# table(ind$IDP01_98) # Don't know
# table(ind$IDP01_99) # Prefer not to respond

ind <- ind %>%
  mutate(idp_valid=
           case_when(IDP01_1==1 | IDP01_2==1 | IDP01_3==1 | IDP01_4==1 | IDP01_5==1 ~ 1, 
                     IDP01_6==0 | IDP01_7==0 ~ 0,
                     TRUE ~ NA_real_)
  ) %>%
  mutate(idp_valid = labelled(idp_valid,
                              labels = c(
                                "Pas une personne déplacée à l'intérieur" = 0,
                                "Personnes déplacées à l'intérieur" = 1)
                              
  ))


###Check the results and compare with population group selected for the household for ind cleaning
# table(ind$idp_valid)


####Refugees and Asylum Seekers

###You should check the primary citizenship of all household members and confirm that 
####refugees and asylum seekers are NOT the citizens of the country of enumeration

# Traitement des variables manquantes pour REF01 en affectant les valeurs du chef de 
# de menage
mobility_infos <- ind %>% 
  filter(personId==1) %>% 
  select(`_parent_index`,c(REF01:REF16a_other)) %>% 
  rename_with( ~ as.character(str_glue("unhcr_{.x}")),REF01:REF16a_other) %>% 
  distinct() %>% as_tibble()
  

###Primary citizenship
ind <- ind %>%
  left_join(mobility_infos,by = join_by(`_parent_index`)) %>% 
  mutate( 
    REF02 = if_else(is.na(REF01),unhcr_REF02,REF02),
    REF01 = if_else(is.na(REF01),unhcr_REF01,REF01),
    # primary citizenship from REF01 and REF02
    citizenship = case_when(
      REF01 == "1" ~ "NER",  # here enter the country code (where RMS took place)
      REF01 %in% c("0", "98") ~ as.character(REF02),
      REF01 == "99" ~ "99"
    )
  ) %>%
  mutate(citizenship = labelled(
    citizenship,
    # labels = setNames(unique(val_labels(ind$REF02)), unique(val_labels(ind$REF02))),
    labels = setNames(unique(val_labels(ind$REF02)), unique(to_character(ind$REF02))),
    label = var_label(ind$REF02)
  )) %>% select(-c(unhcr_REF01:unhcr_REF16a_other))



# table(ind$citizenship)

#####Age groups

# Les codes ci-dessous ne semblent pas très fonctionnels, car considére les âges
# exact en lieu et place des âges révolus. Alors on va reproduire un qui nous semble
# plus fiable
# ind$HH07_cat <- cut(ind$HH07,
#                     breaks = c(-1, 4, 17, 59, Inf),
#                     labels = c("0-4", "5-17", "18-59", "60+"))
# ind$HH07_cat2 <- cut(ind$HH07 ,
#                      breaks = c(-1, 17, Inf),
#                      labels = c("0-17", "18-60+"))

# table(ind$HH07_cat)
# table(ind$HH07_cat2)

# HH07 représente l'âge révolu et non l'âge exact
ind <- ind %>% 
  mutate(
    HH07_cat1 = case_when(
      ind$HH07<5 ~"0-4",
      ind$HH07>=5 & ind$HH07<18 ~ "5-17",
      ind$HH07>=18 & ind$HH07<60 ~ "18-59",
      ind$HH07>=60 ~"60+",
    ) %>% factor(),
    HH07_cat2 = case_when(
      ind$HH07<18 ~ "0-17",
      ind$HH07>=18 ~ "18-60+"
    ) %>% factor(),
    HH07_cat = HH07_cat1 # change 1 to 2 if you want HH07_cat2 for main variable
  )

# Verification que tout est ok
# unique(ind$HH07_cat[ind$HH07>=5 & ind$HH07<18])
# unique(ind$HH07_cat2[ind$HH07>=17])

# # Autre version qui aurait pu être acceptable
# ind$HH07_cat <- cut(ind$HH07,
#            breaks = c(-1, 4.999, 17.999, 50.999, Inf),
#            labels = c("0-4", "5-17", "18-59", "60+"))
# ind$HH07_cat2 <- cut(ind$HH07 ,
#                      breaks = c(-1, 17.9999, Inf),
#                      labels = c("0-17", "18-60+"))
# unique(ind$HH07_cat[ind$HH07>=60])
# unique(ind$HH07_ca2t[ind$HH07>=60])

### Disability

####The calculation for this section is standard. For more details, please refer here: https://www.washingtongroup-disability.com/fileadmin/uploads/wg/WG_Document__7A_-_Analytic_Guidelines_for_the_WG-SS_Enhanced__SPSS_.pdf

###Step 1: Generate frequency distributions on each of the six WG-SS domain variables

### 1	No difficulty
### 2	Some difficulty
### 3	A lot of difficulties
### 4	Cannot do at all
### 98	Don’t know
### 99	Prefer not to respond

# #Vision
# barplot(table(ind$DIS01), main = "Vision")
# #Hearing
# barplot(table(ind$DIS02), main = "Hearing")
# #Mobility
# barplot(table(ind$DIS03), main = "Mobility")
# #Communication
# barplot(table(ind$DIS04), main = "Communication")
# #Self-care
# barplot(table(ind$DIS05), main = "Self-care")
# #Cognition
# barplot(table(ind$DIS06), main = "Cognition")


#######Step 2. Codes (99) Prefer not to respond and (98) Don’t know, are recoded to Missing.

# Replace "98"  and "99" with "NA" using dplyr
ind <- ind %>%
  mutate(
    DIS01 = ifelse(DIS01 == "98" | DIS01 == "99", NA, DIS01),
    DIS02 = ifelse(DIS02 == "98" | DIS02 == "99", NA, DIS02),
    DIS03 = ifelse(DIS03 == "98" | DIS03 == "99", NA, DIS03),
    DIS04 = ifelse(DIS04 == "98" | DIS04 == "99", NA, DIS04),
    DIS05 = ifelse(DIS05 == "98" | DIS05 == "99", NA, DIS05),
    DIS06 = ifelse(DIS06 == "98" | DIS06 == "99", NA, DIS06)
  )

# ####Double check for missing values
# table(ind$DIS01, useNA = "ifany")
# # frequencies_DIS01 <- table(ind$DIS01, useNA = "ifany")
# # print(frequencies_DIS01)

# On s'assure que pour tout ceux aui ont un âge <=5 qu'il n'ont pas de valeur  
# sur les variables du handicap
ind <- ind %>%
  mutate(
    DIS01 = ifelse(HH07<=5, NA, DIS01),
    DIS02 = ifelse(HH07<=5, NA, DIS02),
    DIS03 = ifelse(HH07<=5, NA, DIS03),
    DIS04 = ifelse(HH07<=5, NA, DIS04),
    DIS05 = ifelse(HH07<=5, NA, DIS05),
    DIS06 = ifelse(HH07<=5, NA, DIS06)
  ) 

####Create disability status indicator for the Washington Group short set on disability

ind$disability <- ifelse(ind$HH07<=5,NA,0) #0
ind <- ind %>%
  mutate(disability = ifelse(
    parse_number(DIS01) %in% c(3, 4) |
      parse_number(DIS02) %in% c(3, 4) |
      parse_number(DIS03) %in% c(3, 4) |
      parse_number(DIS04) %in% c(3, 4) |
      parse_number(DIS05) %in% c(3, 4) |
      parse_number(DIS06) %in% c(3, 4),
    1,
    disability
  ),
  disability_visual = as.numeric(parse_number(DIS01) %in% c(3, 4) & disability==1),
  disability_hearing = as.numeric(parse_number(DIS02) %in% c(3, 4)  & disability==1 ),
  disability_mobility = as.numeric(parse_number(DIS03) %in% c(3, 4)  & disability==1),
  disability_intelpsych = as.numeric((parse_number(DIS04) %in% c(3, 4) | parse_number(DIS05) %in% c(3, 4) | parse_number(DIS06) %in% c(3, 4)) & disability==1)
)

##Put the labels
ind <- ind %>%
  mutate(
    disability = factor(disability, levels = c(0, 1), labels = c("Non handicapé", "Handicapé")),
    disability_visual = factor(disability_visual, levels = c(0, 1), labels = c("Non handicapé", "Handicap visuel")),
    disability_hearing = factor(disability_hearing, levels = c(0, 1), labels = c("Non handicapé", "Handicap auditif")),
    disability_mobility = factor(disability_mobility, levels = c(0, 1), labels = c("Non handicapé", "Handicap moteur")),
    disability_intelpsych = factor(disability_intelpsych, levels = c(0, 1), labels = c("Non handicapé", "Handicap intellectuel/psychique"))
  )

####Check final frequencies
# table(ind$disability)


###* [Below indicators will be used to disaggregate during the analysis].
##Country of origin : `citizenship`
##Age categories : `HH07_cat` and `HH07_cat2`
##Gender : `HH04`
##Population groups: `pop_groups`,population_name
###Disability: disability 

# table(main$pop_groups)
# table(ind$HH04)

##Label the variables below 
pop_groups_labels <- c(
  "1" = "Demandeurs d'asile",
  "2" = "Réfugiés",
  "3" = "Personnes dans une situation de type réfugié",
  "4" = "Réfugiés rapatriés",
  "5" = "PDI",
  "6" = "Personnes déplacées retournant chez elles",
  "7" = "Apatrides",
  "8" = "Communautés d'accueil"
)

main <- main %>%
  mutate(pop_groups = recode_factor(pop_groups, !!!setNames(as.character(pop_groups_labels), as.character(seq_along(pop_groups_labels)))))

# Verification du resultat
# table(main$pop_groups)

##Label HH04 - sex variable
# Define labels for HH04
HH04_labels <- c(
  "1" = "Femme",
  "2" = "Homme",
  "3" = "Intersexe",
  "99" = "Préfère ne pas répondre"
)

ind <- ind %>%
  mutate(
    HH04 = recode_factor(HH04, !!!setNames(as.character(HH04_labels), as.character(seq_along(HH04_labels))))#,
    # HH04 = relevel(HH04, ref = "Homme")
  )



# Verification du resultat
# table(ind$HH04)

###Put labels for EDU1_random - education variable for randomly selected adult 
main <- main %>%
  mutate(EDU01_random = factor(EDU01_random, 
                               levels = c(0, 1, 2, 3, 4, 5, 6, 8, 9, 98, 99),
                               labels = c("Pas d'education formelle",
                                          "Scolarite informelle uniquement",
                                          "Niveau inferieur au primaire",
                                          "Ecole primaire terminé",
                                          "Premier cycle du secondaire terminé",
                                          "Secondaire superieur termine",
                                          "Enseignement post-secondaire non superieur",
                                          "Licence / diplome equivalent complete",
                                          "Master / diplome equivalent ou superieur",
                                          "Ne sait pas",
                                          "Refuse de repondre")))


# table(main$EDU01_random)


###  Suppression des variables inutiles #### 
instr <- rio::import(file.path("local","input","cleaning_instructions.xlsx"),sheet="drop_varname")
main <-  main %>% select(-any_of(instr$varname))
ind <- ind %>% select(-any_of(instr$varname))


### Traitement des modalités [ autres à préciser ] ####
instr <- rio::import(file.path("local","input","cleaning_instructions.xlsx"),sheet="cleasing_instruction")

###* [Traitement des variables de la base ménage]
varlist <- intersect(names(main),unique(instr$question.name))
dbtmp <- instr %>%  
  filter (question.name %in% varlist, action=="change") %>% 
  select(caseid,varname=question.name,value=new.value) %>% 
  mutate(value = as.character(value)) # une mise à jour du package dplyr convertit automatiquement donc ...

# Check que toutes les données à corriger sont bien dans la base individu
for(hhid in unique(dbtmp$caseid)){
  varnames <- unique(dbtmp %>% filter(caseid==hhid) %>% pluck("varname"))
  for(question in varnames){
    value <- dbtmp %>% filter(caseid==hhid,varname==question) %>% pluck("value")
    # if(length(value)!=1){
    #   cat("\n[DEDI - household] Erreur bien vouloir ajuster le code pour : hhid=",hhid,"; variable=",question," qui a plus de 2 observations \n")
    #   stop(".......")
    # }
    # A adapter le cas echeant
    if(length(value)==1){
      if(any(class(main[[question]]) %in% "integer")) value <-  as.integer(value)
      # if(any(class(main[[question]]) %in% "character")) value <-  as.character(value)
      main[which(main$caseid==hhid),question] <- value    
    } 
  }
}

###* [Traitement des variables de la base individu]
varlist <- intersect(names(ind),unique(instr$question.name))
dbtmp <- instr %>%  
  filter (question.name %in% varlist, action=="change") %>% 
  select(caseid,personId=child_id,varname=question.name,value=new.value) %>% 
  mutate(value = as.character(value)) # une mise à jour du package dplyr convertit automatiquement donc ...

#Ajout dans la base individu l'identifiant de la base base menage
ind <-  ind %>% 
  left_join(main %>% select(hhid=`_index`,caseid) %>% as_tibble(),
            by=join_by(`_parent_index`==hhid)) %>% 
  as_tibble()

# Check que tout les données à corriger sont bien dans la base individu
# setdiff(dbtmp$caseid,ind$caseid)
for(i in  1:nrow(dbtmp)){
  hhid <- dbtmp$caseid[i]
  question <- dbtmp$varname[i]
  personID <- dbtmp$personId[i]
  value <- dbtmp$value[i]
  
  rowid <- which(ind$caseid==hhid & ind$personId==personID)
  # if(length(rowid)!=1){
  #   cat("\n[DEDI - ind] Erreur bien vouloir ajuster le code pour : hhid=",hhid,"; variable=",question," qui a plus de une observation (rowid=",
  #   rowid,")\n")
  #   stop(".......")
  # }
  # A adapter le cas echeant
  if(length(rowid)==1) ind[rowid,question] <- ifelse(is.numeric(unlist(ind[rowid,question])),as.numeric(value),as.character(value))
}

rm(hhid,question,personID,value,rowid,varlist,varnames,i,uid,pop_groups_labels,HH04_labels,instr,dbtmp)
### MERGE DISAGGREGATION VARIABLES FROM INDIVIDUAL TO HOUSEHOLD DATASET ####

####RANDOMLY SELECTED ADULT  

###Run this step if only you have extra variable for the first selected with the same name
#* [Ancien code ... mais semble pas efficace]
#main$name_selectedfirst <- ifelse(is.na(main$name_selectedadult18), main$name_selectedadult18_1, main$name_selectedadult18)
# table(main$name_selectedfirst)

###Create a variable called random_adult to match with the main dataset
# main <- main %>%
#   mutate(random_adult=case_when(
#     random_present %in% c(1,3) ~ name_selectedfirst,
#     random_present_2 %in% c(1,3) ~ name_selectedadult18_2,
#     TRUE ~ name_respondent)
#   )
# # table(main$random_adult)

# #* [BOF..]
#* [Review (1) code ... mais semble pas efficace]
# main <- main %>%
#   mutate(name_selectedfirst = case_when(
#     name_respondent_individual == "1" ~ member1,
#     name_respondent_individual == "2" ~ member2,
#     name_respondent_individual == "3" ~ member3,
#     name_respondent_individual == "4" ~ member4,
#     name_respondent_individual == "5" ~ member5,
#     name_respondent_individual == "6" ~ member6,
#     TRUE ~ name_respondent
#   ))
# 
# main <- main %>%
#   mutate(random_adult=case_when(
#     random_present %in% c(1) ~ name_selectedadult18,
#     random_present_2 %in% c(1) ~ name_selectedadult18_2,
#     TRUE ~ name_selectedfirst)
#   )

#* [Review code (2) ...  semble plus efficace .. mais inutile pour l'objectif visé]
main <- main %>%
  mutate(
    # Recupération du nom du répondant sélectionné aléatoirement pour répondre au questionnaire individuel
    random_adult = case_when(
      # Premier choix répondant au questionnaire individuel choisi au hasard
      random_present %in% c(1) ~ name_selectedadult18, 
      # Deuxieme répondant au questionnaire indibiduel si le premier répondant 
      # sélectionné au harsard n'est pas disponible
      random_present_2 %in% c(1) ~ name_selectedadult18_2,
      # Théoriquement, on se dit que si les 2 premiers répondants ne sont pas disponibles pour répondre
      # Alors le contact principal au questionnaire est forcément celui qui a repondu au questionnaire
      # individuel
      TRUE ~ name_respondent
    ),
    
    #  Indique si la personne sélectionnée aleatoire, que ce soit la première ou la deuxoéme
    #  était disponible pour l'enquête. Dans le cas constraire, on peut supposer que le nom
    #  attribué aléatoire est celui qui a accepté de donnéer son numéro de téléphone et qui peut ne 
    #  pas être la personne qui a participé à l'enquête. Cette personne peut ne pas avoir de numéro
    #  et c'est celui d'une tierce personne dans le ménage qui sera rapporté...
    random_available = as.numeric(random_present %in% c(1) |  random_present_2 %in% c(1))
    
    #  Une question permet également à l'agent d'indiquer explicitement avant name_respondent qui est
    #  effectivement le répondant principal au questionnaire individuel. En principe,
    # random_adult et respondent_individual devrait être identique...
    #' [Correction faite directement dans le masque de saisie - plus nécessaire]
    # respondent_individual = case_when(
    #   name_respondent_individual == "1" ~ member1,
    #   name_respondent_individual == "2" ~ member2,
    #   name_respondent_individual == "3" ~ member3,
    #   name_respondent_individual == "4" ~ member4,
    #   name_respondent_individual == "5" ~ member5,
    #   name_respondent_individual == "6" ~ member6
    # )#,
    # random_adult = case_when(
    #   # Premier choix répondant au questionnaire individuel choisi au hasard
    #   random_present %in% c(1) ~ name_selectedadult18, 
    #   # Deuxieme répondant au questionnaire indibiduel si le premier répondant 
    #   # sélectionné au harsard n'est pas disponible
    #   random_present_2 %in% c(1) ~ name_selectedadult18_2,
    #   # Théoriquement, on se dit que si les 2 premiers répondants ne sont pas disponibles pour répondre
    #   # Alors le contact principal au questionnaire est forcément celui qui a repondu au questionnaire
    #   # individuel
    #   TRUE ~ name_respondent
    # )
  )

# Vérification que le répondant individuel indiqué par l'enquêteur est bien celui 
# qui a été selectionné aléatoirement par le programme.

# main %>%  
#   select(caseid,`_index`,random_adult,respondent_individual,random_available) %>% 
#   mutate(diff_name = as.numeric(random_adult!= respondent_individual)) %>% 
#   # filter(diff_name==1) #496
#   # filter(diff_name==1, random_available==0) #59
#   filter(diff_name==1, random_available==1) #437

#* [Commentaires importants..] 
# On se rend compte que lorsque le répondant au questionnaire individuel indiqué par
# l'agent est différent de celui sélectionné automatiquement par le programme, 
# c'est principalement dû au fait que que le répondant principal au questionnaire, 
# qui a accepté de donner son numéro n'est pas celui que l'agent a indiqué avoir répondu au questionnaire
# au questionnaire individuel Dans le cas d'espèce on s'en tient aux déclarations de l'agent
# En outre, la liste des personnes associées à la déclaration de l'agent de collecte est 
# restreinte aux personnes âgées de 18 ans et plus. Donc elle, plus fiable que la précédente
#  Donc en lieu et place de random_adult pour la fusion on utilisera respondent_individual

##Create a new dataset with indicators for merge, below you can add all other indicators you want to import from individual dataset

#* [On récupére les informations du chef de ménage]
ind_m <- ind %>% 
  filter(ind_relationship_code==1) %>% #sélection du chef de ménage
  mutate(
    unhcr_name_individual =  name_individual,
    ind_name = name_individual
  ) %>% 
  ###Here below add idp_valid if only you have IDPs
  select("_parent_index", "HH07_cat", "HH07_cat1", "HH07_cat2", "disability","citizenship",
         "idp_valid", "HH07", "HH04", "HH03","unhcr_name_individual","disability_visual",
         "disability_hearing","disability_mobility","disability_intelpsych",
         "name_individual","HH01","ind_name","ind_gender","ind_age") %>% 
  distinct()

# On s'assure que l'on n'a pas des noms manquants :
# sum(is.na(ind_m$name_individual))

#* [Les caractéristiques individuels à associer aux ménages sont ceux du chef de ménage]
#* Le nom du chef de ménage est stocké dans la variable HHH01_aux, son age dans la variable
#* HHH01_age et son  genre dans la variable : HHH01_sex
#* LA fusion se fait sur la base de l'identifiant ménage
main <- main %>%
  left_join(ind_m, by = c("_index" = "_parent_index")) %>%
  #on supprime les eventuels doublons, On doit avoir une seule observation
  # par ménage...
  group_by(`_index`) %>% slice_head(n=1) %>% ungroup()
rm(ind_m)

#* # [On verifie que la fusion est bien correct]
# main %>%
#   select(HHH01_aux,ind_name,name_individual,
#          HHH01_sex, ind_gender, HHH01_age, ind_age) %>%
#   filter(HHH01_aux!=ind_name)

# Verification que la fusion est correcte
# "HH07_cat", "HH07_cat2",HH07 doivent égale à grpage, grpage2, HHH01_age
#  HH04 = HHH01_sex

#*********************************************************************************** 
#* [ Dans ce cas on va récupérer les informations du répondants aux questions individuels]
#* identifiant = random_adult, toutes les variables vont être préfixe de [_p]
#* pour tous les indicateurs calculés au niveau individuel, on prendra non pas
#* les valeurs du chef de ménage mais plutôt celle du répondant...
#*....................................................................................

ind_p <- ind %>% 
  mutate(p_unhcr_name_individual =  name_individual,ind_name = name_individual) %>% 
  select(`_parent_index`, p_HH07_cat=HH07_cat, p_HH07_cat1=HH07_cat1,p_HH07_cat2=HH07_cat2, p_disability=disability,
         p_citizenship=citizenship,p_idp_valid=idp_valid, p_HH07=HH07, p_HH04=HH04, p_HH03=HH03,
         p_unhcr_name_individual,p_disability_visual=disability_visual,p_disability_hearing=disability_hearing,
         p_disability_mobility=disability_mobility,p_disability_intelpsych=disability_intelpsych,
         name_individual,p_HH01=HH01,p_ind_name=ind_name,p_ind_gender=ind_gender,p_ind_age=ind_age) %>% 
  distinct()

# Ajout des données du répondant au questionnaire individuel... 
# Pour les questions ayant trait à ce questionnaire, le cas échéant, on pourra juste l'exploiter
main <- left_join(main, ind_p,by = c("random_adult"="name_individual", "_index" = "_parent_index"))

# Suppression des variables inexploitables
rm(ind_p) 
#*********************************************************************************** 


#* [Create a new dataset with the indicators that you want to import]
main_m <- main %>% select("_index", pop_groups,population_name,end_result) %>% distinct() ## add variables here
ind <- ind %>% left_join(main_m %>% as_tibble(), by =join_by("_parent_index"=="_index"))
rm(main_m)


#* [Apurement de la base de données individus]

# En principe 

###Before you start creating variables for further disaggregation, CLEAN your RMS data!
###You can get inspired from below steps to help you with your primary cleaning in addition to the steps below


### Step 3. ind Type Conversion ### 
####Use functions like as.numeric(), as.character(), or as.Date() to convert ind types as needed

### Step 4. Document your cleaning decisions #### 
####Document the ind cleaning steps and decisions using comments in your R script or
##a separate documentation file.
# Add comments in your script or R Markdown to explain what changes were made and why.


### Step 5.  Perform Exploratory ind Analysis (EDA) ####
# Conduct EDA to understand the distribution, relationships, and patterns in the cleaned ind.
# Visualize the ind using plots or charts to identify any anomalies.

####EDA Analysis


###NOTE THAT -  You can access the respective codes for response options within your KoBo form

#####Summary Statistics: Compute basic statistics like mean, median, standard deviation, 
####and quartiles for numerical variables using the summary(), mean(), median(), sd(), quantile(), or summary() functions.


###Histograms: Create histograms to visualize the distribution of numerical ind using the hist() function.

# Example of a histogram

##For instance you should check below and confirm that there are no adults below 18

# hist(main$HHH01_age, main = "Histogram of the age of household head")
# hist(main$HH07)


####Bar Plots: For categorical ind, create bar plots to visualize the distribution using the barplot() 
####or ggplot2 package.


# Example of a bar plot

# barplot(table(main$pop_groups), main = "Population groups")


###Scatter Plots: Visualize relationships between two numerical variables using scatter plots with plot() or ggplot2. 
####This helps identify correlations and patterns.

# Example of a scatter plot
# plot(ind$num_var1, ind$num_var2, main = "Scatter Plot")



# After performing these primary ind quality checks, your data should be validated and ready for calculation of the variables


# Compare Results with Expectations
# Compare the results of your analysis with your expectations to identify discrepancies.

# Document the Validation Process
# Keep detailed records of the checks, validations, and their outcomes for future reference.

# If issues or discrepancies are identified during validation, you may need to revisit the ind cleaning process and make necessary adjustments.


####Remove variables that you do not need for further data manipulation

# Create a new dataset with only required variables
##* [Suppression de la base de données ménages (main) des variables utilisées pour la programmation]
##* DEJA EFFECTUE PLUS HAUT
# vars_to_remove_ind <- c("ageMD", "age18above", "age_est", "month_est", "position", "position18",
#                         "Relation_R", "adult18", "women_b_count", "women_b", "father_b", "childLess2", "childLess2name",
#                         "women", "father", "adult", "adult_sum", "adult01")
# 
# ind <- ind[, !(names(ind) %in% vars_to_remove_ind)]

# #* [Suppression de la base de données ménages (main) des variables utilisées pour la programmation]
# vars_to_remove_main <- c("namechild2less", "nochildless2", "women_name_b_total", "women_name_b", "father_name_b",
#                          "women_name", "father_name", "random1ap", "random1ap2", "eadult_nap", "eadult_nap2",
#                          "epositionap", "epositionap2", "random_indexap", "random_indexap2", "selected_adultap",
#                          "selected_adultap2")
# main <- main[, !(names(main) %in% vars_to_remove_main)]


#***************************************** 
## Export des données nettoyés ####
#*****************************************

cat(glue::glue_col(
  "{green [ EXPORT DES BASES DE DONNEES APUREES ] }"
),"\n")

#* Pour le cas du Tchad aucun retourné n'a été enquêté [A AJUSTER LE CAS ECHEANT]
main <- main %>% 
  filter(population_name %in% c(var_HOST,var_IDPs,var_REFUGEES)) %>% 
  group_by(`_index`) %>%  arrange(desc(end)) %>% 
  slice_head(n=1) %>%  ungroup()

ind <-  ind %>%  
  filter(
    population_name %in% c(var_HOST,var_IDPs,var_REFUGEES),
    `_parent_index` %in% unique(main$`_index`)
  )

#Enregistrement des bases de données apurées
suppressWarnings(saving_datasets("clean"))

# Sauvegar de l'espace de travail à toutes fins utiles
rm(mobility_infos)
save.image("rms_space.Rdata")
cat(glue::glue_col("{green ✔} Espace de travail sauvevargé"),"\n")

