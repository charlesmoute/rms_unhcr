
# Programme de téléchargement des données pour le suivi de la collecte
# Enquête : RMS (Result Monitoring Survey) 2026 - HCR TCHAD

# Nettoyage de l'environnement de travail
rm(list=ls())

#****************************************************************************
# Chargement des packages d'intérêt
#****************************************************************************
pacman::p_load(
  tidyverse,
  openxlsx,
  rio,
  janitor,
  labelled,
  robotoolbox#,
  # pins #Finalement on utilise le package git2r et directement le terminal
)

# Fonctions utilitaires
github_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)

#****************************************************************************
# Configuration de l'environnement de travail
#****************************************************************************

config <- list(
  sample = import(file.path("local","input","params.xlsx"),sheet="sample",setclass="tibble"),
  deploiement = import(file.path("local","input","params.xlsx"),sheet="deploiement_quanti",setclass="tibble"),
  form = import(file.path("local","input","choices.xlsx"),sheet=1,setclass="tibble"),
  varnames = import(file.path("local","input","choices.xlsx"),sheet="rename_vars",setclass="tibble"),
  tool_survey =import(file.path("local","input","survey_form.xlsx"),sheet=1,setclass="tibble"),
  start_date = ymd("2026-03-02"), #* [Date à revise - ymd("2026-03-01")]
  end_date = ymd("2026-03-02")+10,#ymd("2026-02-26")+5,
  aujourdhui = today(),
  form_version = Sys.getenv("form_version"),
  form_id = Sys.getenv("form_id"),
  user_list = c("oag_pole_01","oag_pole_02"),
  #* [La variable ci-dessous ne sert à rien - donc on va la supprimer après test]
  # pole_list = c("ADRE","AMDJARASS","BAGA SOLA","FARCHANA","GORE","GOZ BEIDA",
  #               "GUEREDA","HADJER HADID","IRIBA"),
  STATUS_APPROVED="Approved", #validation_status_approved
  STATUS_NOTAPPROVED="Not Approved",#validation_status_not_approved
  STATUS_WAITING="On Hold", #validation_status_on_hold
  STATUT_UNDEFINED="",
  MESSAGE_DEFAULT = "none",
  NOT_APPLY = "not_apply",
  gh_local_path="/Users/charles/Documents/GitHub/datasets",
  # https://raw.githubusercontent.com/username/repository/main/folder/filename.csv
  # Ci-dessous les principales données de monitoring
  gh_config_file = "rms23526_config.rds",
  gh_config_url ="https://raw.githubusercontent.com/charlesmoute/datasets/main/rms23526_config.rds",
  # Ci-dessous les données de suivi des performances de la collecte et des agents..
  gh_data_file = "rms23526_data.rds",
  gh_data_url="https://raw.githubusercontent.com/charlesmoute/datasets/main/rms23526_data.rds"
)
#* [la variable pole_list n'est pas exploité donc ...] 
#* names(config$pole_list) <- config$user_list

# liste des filtres utiles pour l'application
# config$db_zone <- config$form %>% filter(variable=="zone") %>% select(-variable)
# config$zone <- config$db_zone$label
# names(config$zone) <- config$db_zone$value

config$region <- config$form %>% filter(variable=="province") %>% select(-variable)
config$zone <- config$form %>% filter(variable=="zone") %>% select(-variable)
config$localite <- config$form %>% 
  filter(variable=="localite") %>% 
  select(-variable) %>% 
  filter(value %in% as.character(unique(config$sample$localite_id)))
config$agent <- config$form %>% filter(variable=="enumerator") %>% select(-variable)
config$pop_group <- config$form %>% filter(variable=="pop_groups") %>% select(-variable)

config$sample <- config$sample %>% 
  replace_na(list(nbhh_refugee=0,nbhh_host=0,nbhh_idp=0,echantillon=0))

# Indicateurs à exploiter pour les contrôles de qualité
# config$enumerator_count <- config$sample %>% pluck('enumerator_count') %>% 
#   sum(na.rm = TRUE)
config$enumerator_count <- config$deploiement %>% pluck('enumerator_name') %>%
  unique() %>% length()

# Taille totale de l'échantillon
config$nb_survey_total <-  config$sample %>% pluck('echantillon') %>% 
  sum(na.rm = TRUE)


#'------------------------------------------------------------------------------------------------ 
#' [Ajustement de la date d'aujourd'hui à celle de la période d'enquête]
#'------------------------------------------------------------------------------------------------ 
config$aujourdhui <-  if_else(today()>config$end_date,
                             config$end_date,
                             today())
#'------------------------------------------------------------------------------------------------ 

# Configuration des parametres de contrôles de la qualité des données
config$duree_enquete <- as.numeric(config$end_date - config$start_date)
# config$seuil_journalier_collecte <- round(config$nb_survey_total/config$enumerator_count)
# config$seuil_journalier_agent <- round(config$seuil_journalier_collecte/config$enumerator_count)
config$seuil_journalier_agent <- 8 # on fixe la valeur que de la calculer automatiquement
config$seuil_journalier_collecte <- config$seuil_journalier_agent * config$enumerator_count
config$duree_collecte <- as.numeric(config$aujourdhui - config$start_date+1)
config$total_collecte_aujourdhui <-  config$seuil_journalier_collecte * config$duree_collecte
config$total_agent_aujourdhui <-  config$seuil_journalier_agent * config$duree_collecte

#****************************************************************************
# Fonction utilitaire de construction de la base de données de monitoring
#****************************************************************************

#* [import & traitement des données] 
import_data <- function(){
  
  # variable resulta
  result <- list()
  
  # Import des données depuis le serveur kobo
  kobo_setup()
  cat("\n",glue::glue_col("{yellow Charles >} Connexion au serveur Kobo"),"\n")
  result$raw <- kobo_data(kobo_asset(config$form_id),progress=TRUE)
  
  #****************************************************************************  
  # Traitement des donnees 
  #****************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Traitement de la base de données principalement"))
  
  #  Pour chaque ménage on contruit la liste des membres de ménages avec leurs liens de parenté aux chefs de ménage
  #  tout en précisiant l'identifiant du ménage inscrit par l'enquêteur ainsi que l'identifiant généré par le système
  #  (01) Extraction du nom du chef de ménage
  tmp01 <-  result$raw$main %>% 
    select(caseid=`_id`,hhid = Intro02, index=`_index`,hh_name=HHH01_aux) %>% 
    mutate(
      infos_01 = as.character(str_glue("Ménage {hhid} (caseid={caseid}) : {str_to_upper(str_trim(hh_name))} (Chef.fe de ménage)"))
    ) %>% 
    select(index,infos_01) %>% distinct()
  #  (02) Constitution de la liste des autres membres de ménages, chef de ménage exclu...
  tmp02 <- result$raw$rpt_hhmnames %>% 
    rename(parent_index=`_parent_index`,position=hhmnames_pos, hhmember_name=HH01_aux,hhmember_relation=HH03_aux) %>% 
    select(position,parent_index,hhmember_name,hhmember_relation) %>% 
    filter(position>1) %>% 
    mutate(
      hhmember_relation = to_character(hhmember_relation),
      infos_02 = as.character(str_glue("{str_to_upper(str_trim(hhmember_name))} ({hhmember_relation})"))
    ) %>% group_by(parent_index) %>% 
    mutate(
      infos_02 = str_flatten_comma(infos_02,last = " et ",na.rm = TRUE)
    ) %>% 
    select(parent_index,infos_02) %>% ungroup() %>% distinct()
  # Fusion des deux listes / le nom du chef de m"énage qui est certain et éventuellement les autres membres de ménages
  subtmp <- tmp01 %>% left_join(tmp02, by=join_by(index==parent_index)) %>% 
    rowwise() %>% 
    mutate(hh_infos=str_flatten_comma(c(infos_01,infos_02),na.rm = TRUE)) %>% 
    ungroup() %>% 
    select(index,hh_infos) %>% distinct()
  
  #Ajustement au cas RMS NIGE
  db <-  result$raw$main
  # Pour une raison inconnu certaine variable ne sont pas disponible dans  la base de données
  # Et pourtant bien disponible dans l'application, on les crée manuellement si non 
  # disponible afin d'éviter de créer des beug lors du controle..
  varlist <-  c("DWE01_other","DWE06_other_land","DWE07_other_land","DWE06_other_housing",
                "DWE07_other_housing","COOK02_other","COOK03_other","LIGHT02_other",
                "LIGHT03_other","LIGHT04_other","DWA01_other","TOI01_other","TOI03_other",
                "BIR03_other","BIR04_other","HEA01_other","HEA02_other","hh_in_list")
  for(varname in varlist) if(!varname %in% names(db)) db[,varname] <- NA_character_
  
  # db <- result$raw$main %>%
  db <- db %>%
    # On ajoute la liste des membres de ménages
    left_join(subtmp, by=join_by(`_index`==index)) %>% replace_na(list(hh_infos="")) %>% 
    # Traitement compler de la base de données
    mutate(
      
      # # La localité DAR ES SALAM est censé appartenir à la région du LAC et non à celle 
      # # de MANDOUL
      # region = if_else(localite == '202' ,'7',region), #*[Confusion dans le xmlform]
      
      zone_name = to_character(zone),
      region_name = to_character(region),
      localite_name = to_character(localite),
      enumerator_name = name_enumerator,
      enumerator = parse_number(enumerator),
      population_tmp = parse_number(pop_groups),
      population_name = case_when(
        population_tmp %in% c(1,2,3,4) ~ "Réfugié(e)s/Demandeurs d'asile",
        population_tmp %in% c(5,6) ~ "PDIs",
        population_tmp %in% c(8) ~ "Communautés hôtes",
        population_tmp %in% c(7) ~ "Error [Apatrides]",
      ),
      population_code = case_when(
        population_tmp %in% c(1,2,3,4) ~ "refugee",
        population_tmp %in% c(5,6) ~ "idp",
        population_tmp %in% c(8) ~ "host",
        population_tmp %in% c(7) ~ "other",
      ),
      population = case_when(
        population_tmp %in% c(1,2,3,4) ~ 1,
        population_tmp %in% c(5,6) ~ 2,
        population_tmp %in% c(8) ~ 4,
        population_tmp %in% c(7) ~ 0,
      ),
      
      hhid = Intro02,
      hh_located = as.numeric(parse_number(Intro03)==1),
      hh_size = hh_size_001,
      consent = as.numeric(parse_number(Intro03)==1 & parse_number(Intro04)==1),
      consent_motif = to_character(result$raw$main$Intro05),
      consent_motif = ifelse(parse_number(Intro05)==96,
                             to_character(Intro05_other),consent_motif),
      BIR03_96 = ifelse(BIR03_96==1,'96','0'),
      BIR03_98 = ifelse(BIR03_98==1,'98','0'),
      end_result_code = parse_number(end_result),
      end_result = to_character(end_result),
      today = end
    ) %>% 
    select(
      start,end,today,start_time=start_time_1,end_time=end_time_1,
      zone,zone_name,region,region_name,localite,localite_name,
      enumerator,enumerator_name,population,population_name,
      population_code,hhid,hh_located,hh_size,hh_infos,
      consent,consent_motif,end_result_code,end_result,
      #Coordonnnees gps
      longitude=geopoint_longitude,
      latitude=geopoint_latitude,
      precision=geopoint_precision,#precision doit être <=5m,
      comment=final_notes_entry,
      # Variables en lien avec le taux d'imrpécision
      # Modalités des < Autres à préciser>
      # imprécision (other_variable==96)
      # other_,value_other_,
      other_DWE01=DWE01,value_other_DWE01=DWE01_other,
      other_DWE06_land=DWE06_land,value_other_DWE06_land=DWE06_other_land,
      other_DWE07_land=DWE07_land,value_other_DWE07_land=DWE07_other_land,
      other_DWE06_housing=DWE06_housing,value_other_DWE06_housing=DWE06_other_housing,
      other_DWE07_housing=DWE07_housing,value_other_DWE07_housing=DWE07_other_housing,
      other_COOK02=COOK02,value_other_COOK02=COOK02_other,
      other_COOK03=COOK03,value_other_COOK03=COOK03_other,
      other_LIGHT02=LIGHT02,value_other_LIGHT02=LIGHT02_other,
      other_LIGHT03=LIGHT03,value_other_LIGHT03=LIGHT03_other,
      other_LIGHT04=LIGHT04,value_other_LIGHT04=LIGHT04_other,
      other_DWA01=DWA01,value_other_DWA01=DWA01_other,
      other_TOI01=TOI01,value_other_TOI01=TOI01_other,
      other_TOI03=TOI03,value_other_TOI03=TOI03_other,
      other_BIR03=BIR03_96,value_other_BIR03=BIR03_other,
      other_BIR04=BIR04,value_other_BIR04=BIR04_other,
      other_HEA01=HEA01,value_other_HEA01=HEA01_other,
      other_HEA02=HEA02,value_other_HEA02=HEA02_other,
      
      # Variables en lien avec les valeurs manquantes
      # Modalités <Ne sait pas>, <Ne veut pas répondre>
      # valeurs manquantes (missing)
      # valeur manquante => missing_variable %in% c(98,99)
      # missing_,
      missing_SHEL01=SHEL01,
      missing_SHEL02=SHEL02,missing_SHEL03=SHEL03,missing_SHEL04=SHEL04,missing_SHEL05=SHEL05,
      missing_SHEL06=SHEL06,missing_RISK02=RISK02,missing_DWE06_land=DWE06_land,
      missing_DWE06a_land=DWE06a_land,missing_DWE06_housing=DWE06_housing,
      missing_DWE06a_housing=DWE06a_housing,missing_DWE10=DWE10,
      missing_LIGHT03=LIGHT03,missing_LIGHT05=LIGHT05,missing_LIGHT06=LIGHT06,
      missing_DWA01=DWA01,missing_TOI02=TOI02,missing_TOI03=TOI03,missing_BIR03=BIR03_98,
      missing_HEA01=HEA01,
      missing_SPF01a=SPF01a,missing_SPF01b=SPF01b,missing_SPF01c=SPF01c,missing_SPF01d=SPF01d,
      missing_SPF01e=SPF01e,missing_SPF01f=SPF01f,missing_SPF01g=SPF01g,missing_SPF01h=SPF01h,
      missing_SPF01j=SPF01j,missing_SPF01k=SPF01k,missing_SPF01l=SPF01l,missing_SPF01m=SPF01m,
      missing_SPF01n=SPF01n,missing_SPF01o=SPF01o,missing_SPF01p=SPF01p,
      missing_EDU01_random=EDU01_random,missing_INC01=INC01,missing_INC02=INC02,
      missing_SAF01=SAF01,missing_GBV01a=GBV01a,missing_GBV01b=GBV01b,missing_GBV01c=GBV01c,
      missing_GBV01d=GBV01d,
      missing_VAW01a=VAW01a,missing_VAW01b=VAW01b,missing_VAW01c=VAW01c,missing_VAW01d=VAW01d,
      missing_VAW01e=VAW01e,
      
      # Variable de contrôle
      caseid=`_id`,uuid=`_uuid`,`index`=`_index`,
      submitted_by=`_submitted_by`,
      submission_time=`_submission_time`,
      validation_status=`_validation_status`
    )  %>% 
    mutate(
      #' #'**********************************************************************
      #' #' [Correction manuelle à inscrire ci-dessous]
      #' Ci-dessous on procédera aux affectations afon de corriger les données en
      #' fonction des incohérences identifiés sur le terrain
      #' #'**********************************************************************
      
      # # Correction au 05/03/2026
      # L'agent EIBA KATIGNE ZACHARIA a affecté par défaut <Non complet> à 6 questionnaires
      # Ici nous les réaffectons à <Complet>
      end_result_code = ifelse(
        caseid %in% c(116487430,116487428,116487427,116487426,116487425,116487423),
        1,end_result_code
      )#,
      
      #' # Suspicion d'incoherence de données pour les 20 questionanires soumis
      #' # par EIBA KATIGNE ZACHARIA (id=23) le 04/03/2026
      #' #' [PAR PRUDENCE J'INVALIDE D'ABORD CES DONNEES] 
      #' validation_status = if_else(
      #'   enumerator==23 & as_date(submission_time)==ymd("2026-03-04"),
      #'   'validation_status_not_approved',
      #'   validation_status
      #' )
      
      # Suspicion d’incohérence de données pour  tous ceux ayant soumis plus de 
#     # 12 questionnaires le 04/03/2026  
      #' #' [PAR PRUDENCE J'INVALIDE D'ABORD CES DONNEES]
      #' validation_status = if_else(
      #'   enumerator %in% c(4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 17, 21, 23, 24, 25, 26, 31,
      #'                     33, 35, 37, 38, 50, 57, 59, 60, 63, 65, 70) &
      #'     as_date(start)==ymd("2026-03-04") &
      #'     as_date(end)==ymd("2026-03-04") &
      #'     as_date(submission_time)==ymd("2026-03-04"),
      #'   'validation_status_not_approved',
      #'   validation_status
      #' ),
      #' #' Les données de l'agent [76] sont en cours de vérification
      #' validation_status = if_else(
      #'   enumerator %in% c(760) &
      #'     # Ci-dessous on supprime les cas où les numeros de téléphones n'ont pas été déclaré
      #'     caseid %in% c(116569461L, 116569462L, 116569465L, 116569473L, 116569476L, 
      #'                   116569479L, 116569480L, 116569481L, 116569483L) &
      #'     as_date(start)==ymd("2026-03-04") &
      #'     as_date(end)==ymd("2026-03-04") &
      #'     as_date(submission_time)==ymd("2026-03-04"),
      #'   'validation_status_not_approved',
      #'   validation_status
      #' ),
      #' 
      #' # Suspicion d’incohérence de données pour  tous ceux ayant soumis plus de 
      #' #     # 12 questionnaires le 05/03/2026  
      #' #' [PAR PRUDENCE J'INVALIDE D'ABORD CES DONNEES]
      #' validation_status = if_else(
      #'   enumerator %in% c(7, 12, 13, 14, 17, 18, 19, 20, 22, 23, 24, 25, 26, 27, 30, 
      #'                     31, 32, 34, 36, 37, 38, 39, 50, 57, 58, 61, 62, 63, 81) &
      #'     as_date(start)==ymd("2026-03-05") &
      #'     as_date(end)==ymd("2026-03-05") &
      #'     as_date(submission_time)==ymd("2026-03-05"),
      #'   'validation_status_not_approved',
      #'   validation_status
      #' ),
      #' #' # Suspicion d’incohérence de données pour  tous ceux ayant soumis plus de 
      #' #' #     # 12 questionnaires le 05/03/2026  
      #' #' #' [PAR PRUDENCE J'INVALIDE D'ABORD CES DONNEES]
      #' #' Les données de l'agent [69] sont attestées vraies
      #' validation_status = if_else(
      #'   enumerator %in% c(4, 5, 6, 7, 9, 11, 14, 21, 30, 37, 62, 80) &
      #'     as_date(start)==ymd("2026-03-06") &
      #'     as_date(end)==ymd("2026-03-06") &
      #'     as_date(submission_time)==ymd("2026-03-06"),
      #'   'validation_status_not_approved',
      #'   validation_status
      #' )
      # # Correction de la localité au 05/Mai/2025
      # localite_name = if_else(caseid %in% c(82910571,82910569) & region_name %in% c("Tillaberi"),
      #                         "Ayerou",localite_name),
      # localite = if_else(caseid %in% c(82910571,82910569) & region_name %in% c("Tillaberi"),
      #                    "602",localite)
      
    ) %>% 
    filter(submitted_by %in% config$user_list,localite %in% unique(config$localite$value))
    # #* [LIGNE EN DESSOUS A SUPPRIMER AVANT LE COLLECTE DE DONNEES POUR CELLE DE DESSUS]
    # filter(localite %in% unique(config$localite$value)) 
  
  # Conversion des types des donnees
  db <- db %>% 
    mutate(
      across(c(starts_with(c("other_","missing_")),zone,region,localite),
             parse_number),
      across(starts_with(c("value_other_")),as.character),
      validation_status = to_character(validation_status)
    ) %>% # on ne conserve que les données courant sur la période de l'enquête
    filter(
      date(start) >= config$start_date & date(end) <= config$end_date,
      is.na(validation_status)#, #on s'assure d'exclure les données marqués explicitement non approuvée
      # consent == 1 & end_result_code %in% c(1) #on s'assure de n'examiner que les questionnaires complets avec consentement
      # Si on prend en compte le contrôle ci-dessus on ne pourra plus calculer le taux de consentement
    )
  
  
  # Sauvegarde des donnees dans la variable utilitaire de configuration
  result$clean <- db
  
  
  #**************************************************************************************  
  # Sélection des variables d'intérêt dans la base de données des membres de ménages
  #**************************************************************************************  
  cat("\n",glue::glue_col("{yellow Charles >} Traitement de la liste des membres des ménages (part-01)"))
  db <- result$raw$rpt_hhmnames %>% 
    select(
      # Identifiant du ménage (parent_index), Identifiant de l'individu (index)
      index=hhmnames_pos,parent_index=`_parent_index`,
      
      # Variables en lien avec les valeurs manquantes
      # Modalités <Ne sait pas>, <Ne veut pas répondre>
      # valeurs manquantes (missing)
      # valeur manquante => missing_variable %in% c(98,99)
      # missing_,
      missing_HH03_aux=HH03_aux
    ) %>% mutate(
      across(starts_with(c("other_","missing_")),parse_number)#,
      # across(starts_with(c("value_other_")),as.character)
    ) %>% 
    filter(parent_index %in% unique(result$clean$index)) %>% 
    mutate(index=as.character(str_glue("missing_HH03_aux_{index}"))) %>% 
    pivot_wider(names_from=index,values_from = missing_HH03_aux,
                names_expand = TRUE) %>% 
    mutate(missing_HH03_aux_1=1)
  
  # Ajout dans la base de données traitées
  result$clean <- result$clean %>% 
    left_join(
      db,
      by = join_by(index==parent_index)
    )
  
  
  #**************************************************************************************
  # Selection des variables d'intérêt pour la section01
  #**************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Traitement de la liste des membres des ménages (part-02)"))
  db <- result$raw$S1 %>% 
    select(
      # Identifiant du ménage (parent_index), Identifiant de l'individu (index)
      index=personId,parent_index=`_parent_index`,
      
      # valeurs manquantes (missing)
      # valeur manquante => missing_variable %in% c(98,99)
      # missing_,
      missing_HH08=HH08
      
      # imprécision (other_variable==96)
      # other_,value_other_,
      
    ) %>% mutate(
      across(starts_with(c("other_","missing_")),parse_number)#,
      # across(starts_with(c("value_other_")),as.character)
    ) %>% 
    filter(parent_index %in% unique(result$clean$index)) %>% 
    mutate(index=as.character(str_glue("missing_HH08_{index}"))) %>% 
    pivot_wider(names_from=index,values_from = missing_HH08,names_expand = TRUE)
  
  # Sauvegarde de la base de données en lien avec les caractéristiques socio-démographiques
  # des membres de ménage
  result$clean <- result$clean %>% 
    left_join(
      db,
      by = join_by(index==parent_index)
    )
  
  
  #**************************************************************************************
  # Selection des variables d'intérêt pour la section02
  #**************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Traitement des questions individuels pour chaque membre des ménages"))
  db <- result$raw$S2_repeat %>% 
    mutate(
      IDP01_5 = ifelse(IDP01_5==1,'96','0'),
      IDP01_98 = to_character(IDP01_98),
      IDP01_99 = to_character(IDP01_99)
    ) %>% group_by(`_parent_index`) %>% 
    mutate(`_index`= 1:n()) %>% ungroup() %>% 
    select(
      # Identifiant du ménage (parent_index), Identifiant de l'individu (index)
      index=`_index`,parent_index=`_parent_index`,
      
      # valeurs manquantes (missing)
      # valeur manquante => missing_variable %in% c(98,99)
      # missing_,
      missing_REF01=REF01, missing_REF03=REF03,missing_REF11=REF11,
      missing_REF13=REF13, missing_REF15=REF15,missing_REF16=REF16,
      missing_IDP01_98=IDP01_98, missing_IDP01_99=IDP01_99,
      missing_IDP03=IDP03,missing_IDP04=IDP04,
      missing_REG01a=REG01a,missing_REG01b=REG01b,missing_REG01c=REG01c,
      missing_REG01d=REG01d,missing_REG01f=REG01f,
      #missing_A1=A1,missing_A1b=A1b,
      missing_REG02=REG02,missing_REG03=REG03,missing_REG04=REG04,
      missing_REG05a=REG05a,missing_REG05b=REG05b,missing_REG05c=REG05c,
      missing_REG05e=REG05e,missing_REG06=REG06,
      missing_MMR02=MMR02,missing_MMR03=MMR03,
      missing_DIS01=DIS01,missing_DIS02=DIS02,missing_DIS03=DIS03,
      missing_DIS04=DIS04,missing_DIS05=DIS05,missing_DIS06=DIS06,
      missing_EDU03=EDU03,missing_EDU04=EDU04,missing_EDU05=EDU05,
      missing_COMM03=COMM03,missing_COMM04=COMM04,
      
      # imprécision (other_variable==96)
      # other_,value_other_,
      other_REF16=REF16, value_other_REF16=REF16_other,
      other_REF16a=REF16a, value_other_REF16a=REF16a_other,
      other_IDP01=IDP01_5,value_other_IDP01=IDP01a,
      other_HACC02=HACC02,value_other_HACC02=HACC02_other,
      other_HACC04=HACC04,value_other_HACC04=HACC04_other,
      other_EDU04=EDU04,value_other_EDU04=EDU04_other,
      other_EDU05=EDU05,value_other_EDU05=EDU05_other
            
    ) %>% mutate(
      # across(starts_with(c("other_","missing_")),parse_number),
      across(starts_with(c("other_","missing_")), ~ parse_number(as.character(.))),
      across(starts_with(c("value_other_")),as.character)
      # other_IDP01 = ifelse(other_IDP01==5,96,other_IDP01)
    ) %>% 
    filter(parent_index %in% unique(result$clean$index)) %>% 
    mutate(
      # index=as.character(str_glue("ind{str_pad(as.character(index),width = 2,side='left',pad='0')}"))
      index = as.character(str_glue("{index}"))
      #pour facilement repérer les suffixer avec ind01 pour le premier membre du ménage, etc..
      #index = as.character(str_glue("ind{index}"))
    ) %>%
    pivot_wider(names_from=index,
                values_from = c(missing_REF01:value_other_EDU05),
                names_expand = TRUE)
  
  # Sauvegarde de la base de données
  result$clean <- result$clean %>% 
    left_join(
      db,
      by = join_by(index==parent_index)
    )  
  
  #**************************************************************************************
  #* Export du resultats de traitement des données
  #**************************************************************************************
  invisible(result)
}

#* [Construction de la base de données de monitoring] 
build_monitoringData <- function(database){
  
  cat("\n",glue::glue_col("{green [Construction de la base de données de monitoring]}"))
  
  # Variable utilitaire pour sauvegarder les résultats à retourner
  # Élément de monitoring.
  result <- tibble(date=ymd_hms(character()),
                   zone=character(0),
                   # region=character(0),
                   localite=character(0),
                   enumerator=character(0),
                   population=character(0),
                   caseid=numeric(0),
                   type=character(0),
                   value=numeric(0),
                   message=character(0),
                   infos=character(0))
  #une colonne observation devra être indiqué par le gestionnaire de données
  #afin de documenter toutes les incohérences et surtout comment il a eu à les
  #traiter...
  
  # Initialisation de la variable utilitaire
  db <- database
  
  #*[Toutes les indicateurs ci-dessous ont pour base les observations]
  
  #***************************************************************************************************************
  #* Évaluation des incohérences
  #***************************************************************************************************************
  
  # Doublons dans les données sur la base de uid
  # Cette incohérence n'étant pas du fait de l'agent, elle sera exclu du calcul du taux incohérence
  # L'examen des doublons se fait uniquement parmi les observations qui ont un statut distinct de "Non Approuvée"
  cat("\n",glue::glue_col("{yellow Charles >} Identification des doublons systèmes..."))
  tmp <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>% # on supprime les observations 'non approuvées'
    group_by(uuid) %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      value=n(),
      txt = str_flatten_comma(caseid, ", et "),
      uid = unique(uuid),
      idx = as.numeric(value > 1),
      type = "erreur_doublon_uuid", #vu que cette incoherence doit être exclu on l'a prefixe par erreur_ et non pas error_
      message=ifelse(
        idx==1,
        as.character(str_glue("Les questionnaires {txt} sont des doublons les uns des autres (uuid={uid}).")),
        config$MESSAGE_DEFAULT # "Aucun doublon n'a été identifié pour l'identifiant systeme << {uid} >>."
      ),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% ungroup() %>%
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
    
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  # Erreur : doublon dans l'identifiant rapporté par l'enquêteur
  # hhid... 
  cat("\n",glue::glue_col("{yellow Charles >} Identification des doublons sur l'identifiant ménage"))
  tmp <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>% # on supprime les observations 'non approuvées'
    group_by(hhid) %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      value=n(),
      txt = str_flatten_comma(caseid, ", et "),
      hh_id = unique(hhid),
      idx = as.numeric(value > 1),
      type = "error_doublon_hhid",
      message=ifelse(
        idx==1,
        as.character(str_glue("Les questionnaires {txt} ont le même identifiant ménage << {hh_id} >>.")),
        config$MESSAGE_DEFAULT # "Aucun doublon sur l'identifiant ménage << {hh_id} >> n'a été identifié."
      ),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% ungroup() %>% filter() %>% 
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  # Erreur sur la population cible....
  # population==0 => Apatrides pas dans le champ de l'enquête
  cat("\n",glue::glue_col("{yellow Charles >} Identification des erreurs sur la population cible"))
  tmp <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2), #agent a indiqué complet ou partiellement complet
    ) %>% # on supprime les observations 'non approuvées'
    mutate(
      date=floor_date(submission_time,days(1)),
      value=population,
      type = "error_population_cible",
      message=ifelse(
        population==0,
        as.character(str_glue("[Erreur] Les ménages d'apatrides ne doivent pas être interviewés.")),
        config$MESSAGE_DEFAULT # "Aucune incohérence identifiée dans le type de ménage."
      ),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% ungroup() %>%
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  # Vérification de la précision
  cat("\n",glue::glue_col("{yellow Charles >} Identification des erreurs sur la précision des coordonnées GPS"))
  tmp <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>%
    mutate(
      date=floor_date(submission_time,days(1)),
      type="error_gps_precision",
      value=precision,
      idx = as.numeric(precision>=5),
      message = ifelse(
        idx==1,
        as.character(str_glue("[Erreur] La précision des coordonnées gps ({precision}m) devrait être moins de 5m. Précisez dans la colonne <comment> l'adresse du ménage sous le format suivant : << NOM_VILLE, NOM_QUARTIER, NOM_REPERE, ESTIMATION_DISTANCE_MENAGE_AU_REPERE >>. Exemple : YAOUNDE, MINI PRIX BASTOS, STATION TRADEX, 200m.")),
        config$MESSAGE_DEFAULT # "Précision des coordonnées gps est dans la plage de valeur autorisé"
      ),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% 
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
    
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  # Calcul du tx_incoherence
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux d'incoherences intra-questionnaire"))
  db_monitoring <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>% 
    mutate(
      type="tx_incoherence_intra",
      total_incoherence = 3 #*nombre incohérences faites par l'agent et vérifié par ce programme
    ) %>% 
    left_join(
      # Les incohérences identifiées dans le cas de cette collecte ne sont pas 
      # de nature à conduire à un rejet de questionnaire. Toutefois, nous conservons
      # ce script à toutes fins utiles....
      result %>% #select(caseid,type) %>%
        group_by(caseid,type) %>% slice_head(n=1) %>% ungroup() %>% #juste pour se rassurer que chaque type est compté au plus une fois pour chaque observation(cas) 
        filter(str_starts(type,"error_")) %>%
        select(caseid,type,message) %>% distinct() %>% group_by(caseid) %>% 
        mutate(nb_incoherence=sum(message!=config$MESSAGE_DEFAULT,na.rm = TRUE)) %>% select(caseid,nb_incoherence) %>% 
        distinct(),
      join_by(caseid)
    ) %>% # replace_na(list(nb_incoherence=0,total_incoherence=0)) %>% 
    filter(!is.na(nb_incoherence)) %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      value = ifelse(total_incoherence==0,0,
                     round(100*nb_incoherence/total_incoherence,1)),
      message=as.character(str_glue("Taux d'incoherences={value}%. Soit {nb_incoherence} incohérences sur {total_incoherence} vérifiés.")),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% 
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
  # On ajoute ce résultat au fichier de monitoring final
  result <- rbind(result,db_monitoring)
  
  #***************************************************************************************************************
  #* Évaluation des missing
  #***************************************************************************************************************
 
  varlist <- str_subset(names(db),pattern = "^(missing_)")
  varlabs <- unique(str_replace_all(str_replace_all(varlist,"^(missing_)",""),"(_[0-9]{1,2})$","")) %>% 
    str_replace_all("(_[0-9]{1,2})$","") %>% unique()
  
  donnees <- db %>% filter(
    is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
    end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
  )
  donnees$date <- floor_date(donnees$end,days(1))
  donnees$value <- -99
  donnees$idx <- 0
  
  cat("\n",glue::glue_col("{yellow Charles >} Identification des valeurs manquantes (Ne sait pas/Refus de répondre)"))
  # Chaque variable concernée par ce contrôle est examiné pour chacune des observations
  for(variable in varlabs){
    varnames <- str_subset(varlist,pattern = str_glue("{variable}"))
    tmp <- donnees %>% 
      mutate(
        type = as.character(str_glue("missing_{variable}")),
        message = as.character(str_glue("Formulaire {hhid} (caseid={caseid}) : valeur(s) manquante(s) à la question {variable}. Une valeur manquante correspond à un <Ne  sait pas> ou à un <Refus de répondre>.")),
        infos=as.character(str_glue("{hh_infos}."))
      ) %>% 
      rowwise() %>% 
      mutate(idx=as.numeric(any(pick({{varnames}}) %in% c(99,98)))) %>% 
      ungroup() %>% 
      mutate(message = ifelse(idx==1,message,config$MESSAGE_DEFAULT)) %>% 
      # filter(idx==1) %>% 
      select(
        date,
        zone=zone_name,
        # region=region_name,
        localite=localite_name,
        enumerator=enumerator_name,
        population=population_name,
        caseid,type,value,message,infos
      ) %>% distinct()
    if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  }
  
  # Calcul du taux de missing intra questionnaire (tx_missing_intra)
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de valeurs manquantes intra-questionnaire"))
  db_missing <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>% 
    mutate(type="tx_missing_intra") %>% 
    # (1) Calcul du total de missing évalué par questionnaire (observation)
    rowwise() %>% mutate(total_missing = sum(!is.na(pick({{varlist}})))) %>% ungroup() %>% 
    left_join(
      # (2) Calcul du numérateur
      result %>% filter(str_starts(type,"missing_")) %>% 
        group_by(caseid) %>% #mutate(nb_missing=n()) %>%
        mutate(nb_missing=sum(message!=config$MESSAGE_DEFAULT)) %>%
        select(caseid,nb_missing) %>% distinct(),
      join_by(caseid)
    ) %>% replace_na(list(nb_missing=0)) %>% 
    # (3) Calcul du taux
    mutate(
      date=floor_date(submission_time,days(1)),
      value = round(100*nb_missing/total_missing,1),
      txt = ifelse(value>=10,"[NON APPROUVE] ",""), #un formulaire avec un taux de valeurs manquantes >=10% est automatiquement rejecte
      message=as.character(str_glue("{txt}Taux de valeurs manquantes (Ne veut pas répondre/Ne sait pas) = {value}%. Soit {nb_missing} valeurs manquantes sur {total_missing} vérifiés.")),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% 
    # (4) Selection des variables d'intérêt pour le fichier de monitoring
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
  
  # On ajoute ce résultat au fichier de monitoring final
  result <- rbind(result,db_missing)
  
  #***************************************************************************************************************
  #* Évaluation du taux d'utilisation de la modalité autres à préciser
  #***************************************************************************************************************
  # # Autre à préciser [Taux d'utilisation des autres à préciser] 
  variables_other <- str_subset(names(db),pattern = "^(other_)")
  # values_other <- str_subset(names(db),pattern = "^(value_other_)")
  # varlabs <- unique(str_replace_all(str_replace_all(variables_other,"^(other_)",""),"(_[0-9]{1,2})$",""))
  varlabs <- str_replace_all(variables_other,"^(other_)","")
  
  donnees <- db %>% filter(
    is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
    end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
  )
  donnees$date <- floor_date(donnees$end,days(1))
  donnees$value <- -96
  donnees$idx <- 0
  
  
  cat("\n",glue::glue_col("{yellow Charles >} Identification des cas d'utilisation de la modalité << Autres à préciser>>"))
  for(varlab in varlabs){
    
    # Variables utilitaires...
    var_name <- as.character(str_glue("other_{varlab}"))
    var_value <- as.character(str_glue("value_other_{varlab}"))
    var_question <- unique(str_replace_all(varlab,"(_[0-9]{1,2})$",""))
    
    label_question <- config$varnames %>% filter(new_varname==var_question) %>% pluck("label_question") %>% head(n=1)
    oldvar_question <- config$varnames %>% filter(new_varname==var_question) %>% pluck("old_varname") %>% head(n=1)
    
    tmp_modalite <- config$form %>%
      mutate(answer=as.character(str_glue("({value}) {str_to_lower(label)}"))) %>%
      rowwise() %>%
      filter(any(oldvar_question==unlist(str_split(var_questions,"/"))) & !value %in%c(96,98,99)) %>%
      ungroup() %>% pluck('answer') %>%
      str_flatten_comma(last = " et ", na.rm = TRUE)
    
    # Application du contrôle qualité
    tmp <- donnees %>%
      mutate(
        type = as.character(str_glue("other_{varlab}")),
        varname = var_name, #as.character(str_glue("other_{varlab}")),
        valeur = var_value, #as.character(str_glue("value_other_{varlab}")),
        # message = as.character(str_glue("Question {var_question} : Autres à préciser = «{get(as.name(valeur))}». Bien vouloir reclasser cette réponse parmi les modalités suivantes : {tmp_modalite}.")),
        message = as.character(str_glue("<< Question => [{var_question}] {label_question} >> << Valeur autre précisée => {get(as.name(valeur))} >> << Instruction => Veuillez reclasser cette réponse parmi les modalités suivantes : {tmp_modalite}. Indiquez votre réponse dans la colonne <comment> et inscrivez ensuite la valeur TREATED dans la colonne <status>. >>")),
        infos=as.character(str_glue("{hh_infos}."))
      ) %>% 
      rowwise() %>% 
      # mutate(idx=as.numeric(any(!is.na(pick({{var_name}}))) & any(pick({{var_name}}) %in% c(96))) ) %>% 
      mutate(idx=as.numeric(any(pick({{var_name}}) %in% c(96)))) %>% 
      ungroup() %>% 
      mutate(message = ifelse(idx==1,message,config$MESSAGE_DEFAULT)) %>% 
      # filter(idx==1) %>% 
      select(
        date,
        zone=zone_name,
        # region=region_name,
        localite=localite_name,
        enumerator=enumerator_name,
        population=population_name,
        caseid,type,value,message,infos
      ) %>% distinct()
    if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  }
  
  # Calcul du taux d'imprécision intra questionnaire (tx_other_intra)
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux d'imprécision intra-questionnaire autrement appelé taux d'utilisation de la modalité << Autre à préciser>>."))
  db_other <- db %>%
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>% 
    mutate( type="tx_other_intra") %>% 
    # (1) Calcul du total d'imprécisions à vérifier par questionnaire (observation) [dénominateur]
    rowwise() %>% mutate(total_other = sum(!is.na(pick({{variables_other}})))) %>% ungroup() %>% 
    left_join(
      # (2) calcul du nombre effectif d'imprécisions (numérateur du taux d'imprécision)
      result %>% filter(str_starts(type,"other_")) %>% 
        group_by(caseid) %>% #mutate(nb_other=n()) %>%
        mutate(nb_other=sum(message!=config$MESSAGE_DEFAULT)) %>% 
        select(caseid,nb_other) %>% distinct(),
      join_by(caseid)
    ) %>% replace_na(list(nb_other=0)) %>% 
    # (3) Calcul du taux d'imprécision
    mutate(
      date=floor_date(submission_time,days(1)),
      value = round(100*nb_other/total_other,1),
      message=as.character(str_glue("Taux de recours à la modalité << autre à préciser >> = {value}%. Soit {nb_other} << Autres à préciser >> sur {total_other} questions vérifiées.")),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% 
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
  
  # On ajoute ce résultat au fichier de monitoring final
  result <- rbind(result,db_other)
  
  #***************************************************************************************************************
  #* Évaluation de la durée d'administration d'un questionnaire 
  #*  [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la durée d'interview pour chqaue questionnaire."))
  tmp <- db %>% 
    filter(
      is.na(validation_status)|validation_status!=config$STATUS_NOTAPPROVED,
      end_result_code %in% c(1,2) #agent a indiqué complet ou partiellement complet
    ) %>%
    mutate(
      date=floor_date(submission_time,days(1)),
      type="duree_collecte",
      value=round(as.numeric(as.duration(end - start),"minutes")),
      message = as.character(str_glue("Durée d'administration du questionnaire {hhid} (caseid={caseid}): {value} minute(s).")),
      infos=as.character(str_glue("{hh_infos}."))
    ) %>% 
    select(date,
           zone=zone_name,
           # region=region_name,
           localite=localite_name,
           enumerator=enumerator_name,
           population=population_name,
           caseid,type,value,message,infos) %>% 
    filter(!is.na(value)) %>% distinct()
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  # Retroune la base de données de monitoring.
  cat("\n",glue::glue_col("{green [Fin programme]}"))
  invisible(result)
}

#* [Calcul des indicateurs de performance] 
get_keyPerformanceIndicators <- function(survey,monitoring,byField="enumerator"){
  
  # Variables utilitaires 
  # var_inputs <- c("zone","pole","region","localite","enumerator","population")
  var_inputs <- c("zone","localite","enumerator","population")
  inputvar <- match.arg(byField,var_inputs)
  # macth.arg produira une erreur si il ne parvient pas à identifier le parametre
  inputvar_id <- as.character(str_glue("{inputvar}_id"))
  inputvar_name <- as.character(str_glue("{inputvar}_name"))
  
  cat("\n",glue::glue_col("{green [Calcul des indicateurs de performance par {inputvar}]}"))
  
  survey_data <- survey
  monitoring_data <- monitoring
  db_deploiement <- config$deploiement
  
  # Liste des données disponibles dans la base d'enquête mais non évalué dans la
  # base de monitoring, on les supprime automatiquement
  var_missing <- setdiff(unique(survey_data [[inputvar_name]]),unique(monitoring_data[[inputvar]]))
  if(length(var_missing))
    survey_data <-  survey_data %>% filter( !(!!sym(inputvar_name) %in% var_missing))
  
  # Variable utilitaire pour sauvegarder les résultats à retourner
  # Élément de monitoring.
  result <- tibble(date=ymd_hms(character()),
                   "{inputvar}":=character(0),
                   caseid=numeric(0),
                   type=character(0),
                   value=numeric(0),
                   message=character(0))
  
  #***************************************************************************************************************
  #* Évaluation du taux d'incohérences [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux d'incohérence par {inputvar}"))
  # tx_incoherence = moyenne des tx_incoherence_intra
  # Univers: tous les questionnaires sur le serveur doublon exclus 
  tmp <- survey_data %>%
    # # On s'assure de n'examiner que les questionnaires complets pour lequel on a bien eu un consentement 
    # # au préalable
    # filter(consent == 1 & end_result_code %in% c(1)) %>% 
    #  On ajoute les indicateurs calculés au niveau de chaque questionnaires
    left_join(
      monitoring_data %>% 
        filter(type=="tx_incoherence_intra") %>% 
        select(caseid,taux=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(taux)) %>%
    # on supprime les doublons de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(date=floor_date(submission_time,days(1))) %>% 
    group_by(!!sym(inputvar_name),date) %>% 
    mutate( #calcul au niveau du jour
      type="tx_incoherence",
      taux_jour = round(mean(taux,na.rm=TRUE),1), #Taux moyen d'incoherence à ce jour pour total questionnaire(s) évalué(s)
      total = n(), #Nombre de formulaire pour chaque inputvar par jour de travail
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>% 
    select(date,!!sym(inputvar),type,taux_jour,total) %>% distinct() %>%                          
    group_by(!!sym(inputvar)) %>% arrange(date) %>% 
    mutate(#cumul des indicateurs au fil des jours
      taux_cumul = round(cummean(taux_jour),1), #Taux moyen cumulé d’incohérences depuis le début de collecte à ce jour pour total_cumul questionnaire(s) évalué(s)
      total_cumul = cumsum(total),
    ) %>%  ungroup() %>% 
    mutate(
      value= taux_jour, #taux_cumul,
      message = as.character(str_glue("Taux moyen intra questionnaire d'incohérences au {as.character(date)} est de {taux_jour}% sur {total} questionnaire(s) évalué(s). Taux moyen cumulé intra questionnaire d'incohérences depuis le début de la collecte au {as.character(date)} est de {taux_cumul}% pour {total_cumul} questionnaire(s) évalué(s)."))
    ) %>% 
    select(date,!!sym(inputvar),type,value,message) %>%
    filter(!is.na(value)) %>% # On s'assure que pour les données d'un inputvar pour un jour specifique on a qu'une seule ligne..
    group_by(date,!!sym(inputvar)) %>% arrange(desc(value)) %>% slice_head(n=1) %>% ungroup() %>% 
    distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux de valeurs manquantes [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de valeurs manquantes par {inputvar}"))
  # tx_missing = moyenne des tx_missing_intra
  # Univers: tous les questionnaires sur le serveur doublon exclus 
  tmp <- survey_data %>%
    #  On ajoute les indicateurs calculés au niveau de chaque questionnaires
    left_join(
      monitoring_data %>% filter(type=="tx_missing_intra") %>% select(caseid,taux=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(taux)) %>% 
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(date=floor_date(submission_time,days(1))) %>% 
    group_by(!!sym(inputvar_name),date) %>%
    mutate( #calcul au niveau du jour
      type="tx_missing",
      taux_jour = round(mean(taux,na.rm=TRUE),1), #Taux moyen de valeurs manquantes à ce jour pour total questionnaire(s) évalué(s)
      total = n(), #Nombre de formulaire pour chaque agent, par jour de travail
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>%
    select(date,!!sym(inputvar),type,taux_jour,total) %>% distinct() %>%
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#cumul des indicateurs au fil des jours
      taux_cumul = round(cummean(taux_jour),1), #Taux moyen cumulé de valeurs manquantes depuis le début de collecte à ce jour pour total_cumul questionnaire(s) évalué(s)
      total_cumul = cumsum(total),
    ) %>%  ungroup() %>%
    mutate(
      value = taux_jour, #taux_cumul,
      message = as.character(str_glue("Taux moyen de valeurs manquantes intra questionnaires au {as.character(date)} est de {taux_jour}% sur {total} questionnaire(s) évalué(s). Taux moyen cumulé de valeurs manquantes intra questionnaires depuis le début de la collecte au {as.character(date)} est de {taux_cumul}% pour {total_cumul} questionnaire(s) évalué(s)."))
    ) %>%
    select(date,!!sym(inputvar),type,value,message) %>%
    filter(!is.na(value)) %>% # On s'assure que pour les données d'un agent de collecte pour un jour et un lieu spécifiques on a une seule ligne..
    group_by(date,!!sym(inputvar)) %>% arrange(desc(value)) %>% slice_head(n=1) %>% ungroup() %>%
    distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux d'utilisation de la modalité 'Autres à préciser' [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de recours à la modalité << autre à préciser >> par {inputvar}"))
  # tx_other = moyenne des tx_other_intra
  # Univers: tous les questionnaires sur le serveur doublon exclus 
  tmp <- survey_data %>% 
    #  On ajoute les indicateurs calculés au niveau de chaque questionnaires
    left_join(
      monitoring_data %>% filter(type=="tx_other_intra") %>% select(caseid,taux=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(taux)) %>%
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(date=floor_date(submission_time,days(1))) %>% 
    group_by(!!sym(inputvar_name),date) %>%
    mutate( #calcul au niveau du jour
      type="tx_other",
      taux_jour = round(mean(taux,na.rm=TRUE),1), #Taux moyen d'imprécisions à ce jour pour total questionnaire(s) évalué(s)
      total = n(), #Nombre de formulaire pour chaque agent par jour de travail
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>%
    select(date, !!sym(inputvar),type,taux_jour,total) %>% distinct() %>%
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#cumul des indicateurs au fil des jours
      taux_cumul = round(cummean(taux_jour),1), #Taux moyen cumulé d'imprécision depuis le début de collecte à ce jour pour total_cumul questionnaire(s) évalué(s)
      total_cumul = cumsum(total),
    ) %>%  ungroup() %>%
    mutate(
      value=taux_jour,#taux_cumul,
      message = as.character(str_glue("Taux moyen intra questionnaire de recours à la modalité << autre à préciser>> au {as.character(date)} est de {taux_jour}% sur {total} questionnaire(s) évalué(s). Taux moyen cumulé de recours à la modalité << autre à préciser >> depuis le début de la collecte au {as.character(date)} est de {taux_cumul}% pour {total_cumul} questionnaire(s) évalué(s)."))
    ) %>%
    select(date,!!sym(inputvar),type,value,message) %>%
    filter(!is.na(value)) %>% # On s'assure que pour les données d'un enumérateur pour un jour et un lieu specifique on a qu'une seule ligne..
    group_by(date,!!sym(inputvar)) %>% arrange(desc(value)) %>% slice_head(n=1) %>% ungroup() %>%
    distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation journalier de l'heure d'ouverture minimum des questionnaires (heure de début de travail)
  #* Univers : ensemble des questionnaires doublons exclus
  #* [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de l'heure de début du travail par {inputvar}"))
  
  if(inputvar!="enumerator"){
    # Si l'indicateur est autre que l'enumerateur alors c'est plutôt la médiane des valeurs minimales 
    # des agents de collecte que l'on doit retourner
    tmp <- survey_data %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(enumerator,date) %>% 
      mutate(
        type="heure_dbt_travail",
        value=decimal_date(min(start,na.rm = TRUE)),
        message =NA_character_,
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      select(date,enumerator,!!sym(inputvar),type,value,message) %>% distinct() %>% 
      group_by(!!sym(inputvar),date) %>% 
      mutate(
        value = round(median(value,na.rm = TRUE),2),
        message = as.character(str_glue("Pour la moitié des agents de {.data[[inputvar]]} l'heure de début du travail au {as.character(date)} est de {str_pad(as.character(hour(date_decimal(value))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value))),width=2,side = 'left',pad = '0')}")),
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, pour la moitié des agents de {.data[[inputvar]]} l'heure de début du travail est de {str_pad(as.character(hour(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct() 
  }else{
    # Si l'indicateur est par enumerator alors c'est la valeur minimale de l'heure d'ouverture du premier questionnaire
    # des agents de collecte que l'on doit retourner
    tmp <- survey_data %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(!!sym(inputvar_name),date) %>% 
      mutate(
        type="heure_dbt_travail",
        value=decimal_date(min(start,na.rm = TRUE)),
        message = as.character(str_glue("L'heure de début du travail le {as.character(date)} est de {str_pad(as.character(hour(date_decimal(value))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value))),width=2,side = 'left',pad = '0')}")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("Depuis le début de la collecte au {as.character(date)}, l'heure moyenne de début du travail est de {str_pad(as.character(hour(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct() 
  }
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation journalier de l'heure de clôture maximum des questionnaires (heure de fin de travail)
  #* Univers : ensemble des questionnaires doublons exclus
  #* [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de l'heure de fin du travail par {inputvar}"))
  if(inputvar!="enumerator"){
    # Si l'indicateur est autre que l'enumerateur alors c'est plutôt la médiane des valeurs minimales 
    # des agents de collecte que l'on doit retourner
    tmp <- survey_data %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(enumerator,date) %>% 
      mutate(
        type="heure_fin_travail",
        value=decimal_date(max(end,na.rm = TRUE)),
        message =NA_character_,
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      select(date,enumerator,!!sym(inputvar),type,value,message) %>% distinct() %>% 
      group_by(!!sym(inputvar),date) %>% 
      mutate(
        value = round(median(value,na.rm = TRUE),2),
        message = as.character(str_glue("Pour la moitié des agents de {.data[[inputvar]]} l'heure de fin du travail au {as.character(date)} est de {str_pad(as.character(hour(date_decimal(value))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value))),width=2,side = 'left',pad = '0')}")),
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, pour la moitié des agents de {.data[[inputvar]]} l'heure de fin du travail est de {str_pad(as.character(hour(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct() 
  }else{
    tmp <- survey_data %>%
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(!!sym(inputvar_name),date) %>% 
      mutate(
        type="heure_fin_travail",
        value=decimal_date(max(end,na.rm = TRUE)),
        message = as.character(str_glue("L'heure de fin du travail le {as.character(date)} est de {str_pad(as.character(hour(date_decimal(value))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value))),width=2,side = 'left',pad = '0')}")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>%  ungroup() %>%
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, l'heure de fin du travail est de {str_pad(as.character(hour(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}:{str_pad(as.character(minute(date_decimal(value_cumul))),width=2,side = 'left',pad = '0')}")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct()
  }
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation journalier de la durée de travail journalier
  #* [unite=agent par jour de collecte]
  #* Univers : ensemble des questionnaires doublons exclus
  #* différence entre l'heure de fermeture du dernier questionnaire de la journée et l'heure d'ouverture du 
  #* premier questionnaire de la journée
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la durée au travail par {inputvar}"))
  #* [Duree au travail]
  if(inputvar!="enumerator"){
    # Si l'indicateur est autre que l'enumerateur alors c'est plutôt la médiane des valeurs minimales 
    # des agents de collecte que l'on doit retourner
    tmp <- survey_data %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(enumerator,date) %>% 
      mutate(
        type="duree_au_travail",
        value=round(as.numeric(as.duration(max(end,na.rm = TRUE) - min(start,na.rm = TRUE)),"hours")),
        message =NA_character_,
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      select(date,enumerator,!!sym(inputvar),type,value,message) %>% distinct() %>% 
      group_by(!!sym(inputvar),date) %>% 
      mutate(
        value = round(mean(value,na.rm = TRUE),2),
        message = as.character(str_glue("Durée moyenne au travail pour {.data[[inputvar]]} au {as.character(date)} est de {value} heure(s)")),
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, la durée moyenne au travail pour {.data[[inputvar]]} est de {value_cumul} heure(s)")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% ungroup() %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct() 
  }else{
    tmp <- survey_data %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(!!sym(inputvar_name),date) %>% 
      mutate(
        type="duree_au_travail",
        value=round(as.numeric(as.duration(max(end,na.rm = TRUE) - min(start,na.rm = TRUE)),"hours")),
        message = as.character(str_glue("Duree au travail le {as.character(date)} : {value} heure(s)")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>%
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, la durée au travail est de {value_cumul} heure(s)")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% ungroup() %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% ungroup() %>% distinct()
  }
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #* [Durée de travail]
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la duréé de travail par {inputvar}"))
  if(inputvar!="enumerator"){
    # Si l'indicateur est autre que l'enumerateur alors c'est plutôt la médiane des valeurs minimales 
    # des agents de collecte que l'on doit retourner
    tmp <- survey_data %>% 
      #  On ajoute les indicateurs calculés au niveau de chaque questionnaire
      left_join(
        monitoring_data %>% filter(type=="duree_collecte") %>% select(caseid,duree2travail=value) %>% distinct(),
        by=join_by(caseid)
      ) %>% filter(!is.na(duree2travail)) %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(enumerator,date) %>% 
      mutate(
        type="duree_de_travail",
        value=round(as.numeric(duration(minutes=sum(duree2travail,na.rm=TRUE)),"hours")),
        message =NA_character_,
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      select(date,enumerator,!!sym(inputvar),type,value,message) %>% distinct() %>% 
      group_by(!!sym(inputvar),date) %>% 
      mutate(
        value = round(mean(value,na.rm = TRUE),2),
        message = as.character(str_glue("Duree moyenne de travail pour {.data[[inputvar]]} au {as.character(date)} est de {value} heure(s)")),
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, la durée moyenne de travail pour {.data[[inputvar]]} est de {value_cumul} heure(s)")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% ungroup() %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct() 
  }else{
    tmp <- survey_data %>% 
      #  On ajoute les indicateurs calculés au niveau de chaque questionnaire
      left_join(
        monitoring_data %>% filter(type=="duree_collecte") %>% select(caseid,duree2travail=value) %>% distinct(),
        by=join_by(caseid)
      ) %>% filter(!is.na(duree2travail)) %>% 
      # on supprime les doublons des donnees
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(!!sym(inputvar_name),date) %>%
      mutate(
        type="duree_de_travail",
        value=round(as.numeric(duration(minutes=sum(duree2travail,na.rm=TRUE)),"hours")),
        message = as.character(str_glue("Duree de travail le {as.character(date)} : {value} heure(s).")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt = as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, la duree de travail est de {value_cumul} heure(s).")),
        message = as.character(str_glue("{message} {txt}"))
      ) %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% ungroup() %>% distinct()
  }
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #* [Durée de collecte] 
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la durée d'administration d'un questionnaire par {inputvar}"))
  if(inputvar!="enumerator"){
    tmp <- survey_data %>% 
      #  On ajoute les indicateurs calculés au niveau de chaque questionnaire
      left_join(
        monitoring_data %>% filter(type=="duree_collecte") %>% select(caseid,duree2travail=value) %>% distinct(),
        by=join_by(caseid)
      ) %>% filter(!is.na(duree2travail)) %>% 
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(enumerator,date) %>% 
      mutate(
        type="duree_collecte",
        value=round(mean(duree2travail,na.rm=TRUE),2),
        message =NA_character_,
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      select(date,enumerator,!!sym(inputvar),type,value,message) %>% distinct() %>% 
      group_by(!!sym(inputvar),date) %>% 
      mutate(
        value = round(mean(value,na.rm = TRUE),2),
        message = as.character(str_glue("Duree moyenne d'administration d'un questionnaire pour {.data[[inputvar]]} au {as.character(date)} est de {value} minute(s)")),
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt=as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, la durée moyenne d'administration d'un questionnaire pour {.data[[inputvar]]} est de {value_cumul} minute(s)")),
        message=as.character(str_glue("{message}. {txt}."))
      ) %>% ungroup() %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% distinct() 
  }else{
    tmp <- survey_data %>% 
      #  On ajoute les indicateurs calculés au niveau de chaque questionnaire
      left_join(
        monitoring_data %>% filter(type=="duree_collecte") %>% select(caseid,duree2travail=value) %>% distinct(),
        by=join_by(caseid)
      ) %>% filter(!is.na(duree2travail)) %>% 
      # on supprime les doublons des donnees
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
      mutate(date=floor_date(submission_time,days(1))) %>% 
      group_by(!!sym(inputvar_name),date) %>%
      mutate(
        type="duree_collecte",
        value=round(mean(duree2travail,na.rm=TRUE),2),
        message = as.character(str_glue("Durée moyenne d'administration d'un questionnaire le {as.character(date)} : {value} minute(s).")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>% 
      group_by(!!sym(inputvar)) %>% arrange(date) %>% 
      mutate(
        value_cumul = round(cummean(value),2),
        txt = as.character(str_glue("En moyenne, depuis le début de la collecte au {as.character(date)}, la duree moyenne d'administration d'un questionnaire est de {value_cumul} minute(s)")),
        message = as.character(str_glue("{message} {txt}"))
      ) %>% 
      select(date,!!sym(inputvar),type,value,message) %>% 
      filter(!is.na(value)) %>% ungroup() %>% distinct()
  }
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux de participation [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de participation par {inputvar}"))
  # tx_participation
  # Numérateur: consent==1
  # Dénominateur: nombre total de questionnaire sur le serveur doublon exclus
  tmp <- survey_data %>%  
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      numerateur = as.numeric(consent==1 & end_result_code %in% c(1) & date <= config$aujourdhui),
      denominateur = as.numeric(date <= config$aujourdhui),
    ) %>% 
    group_by(!!sym(inputvar_name),date) %>% 
    mutate(#calcul des indicateurs par enumérateur et jour de collecte
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = sum(denominateur,na.rm = TRUE),
      type="tx_participation",
      value=ifelse(denominateur==0,0,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Taux de participation au {as.character(date)}: {value}%. Soit {numerateur} questionnaire(s) administré(s) sur {denominateur} interviews.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>% 
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>% 
    filter(!is.na(value))  %>% distinct() %>% 
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Taux cumulé de participation du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} questionnaire(s) administré(s) sur {denominateur} interviews.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>% 
    select(-c(numerateur,denominateur,txt)) %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux d'approbation [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux d'approbation par {inputvar}"))
  # tx_approbation
  # Les données sont automatiquement approuvées lorsque le questionnaire est complete ou partiellement complet
  # avec un taux de valeurs manquantes intra-questionnnaire est inférieur à 10%. 
  # Ici on ne prend pas le taux incohérence car dans cette collecte les incohérences identifiés ne sont pas 
  # réellement de nature à biaiser la qualité de données. Dans le cas contraire, les questionnaires automatiquement
  # approuvés devraient avoir un taux de valeurs manquantes ainsi qu'un taux d'incohérence inférieures tous les deux
  # à 10%. Le taux d'imprécision est plus associé à la qualité de la conception de l'outil ainsi qu'à la qualité de la
  # formation des agents de collecte et/ou de leur bonne compréhension de l'outil...
  # 
  # Dans le cas de cette étude, les incohérences identifiées ne sont pas de nature à créé des biais. Mais pour d'autres
  # étude, on pourrait être amené à inlcure la condition tx_incoherence<10% & tx_missing<10% avec  l'option de non complétude
  # 
  # Numérateur: end_result_code %in% c(1,2) & tx_missing<10
  # Dénominateur: nombre total de questionnaire sur le serveur doublons inclus
  tmp <- survey_data %>%
    # Ajoute du taux de missing à la base de données
    left_join(
      monitoring_data %>% 
        filter(type=="tx_missing_intra") %>% 
        select(caseid,tx_missing=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(tx_missing)) %>%
    mutate(
      date=floor_date(submission_time,days(1)),
      # numerateur = as.numeric(end_result_code %in% c(1,2) & (tx_missing<10 | validation_status==config$STATUS_APPROVED) & date <= config$aujourdhui),
      # Dans le cas de cette étude la validation est automatique on a pas d'action humaine d'où l'exclusion du contrôle sur validation_status
      #* [Ancien code : les questions non complets était prix en compte on ajuste]
      # numerateur = as.numeric(tx_missing<10 & date <= config$aujourdhui),#consent == 1 & end_result_code %in% c(1),
      numerateur = as.numeric(tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1)),
      denominateur = as.numeric(date <= config$aujourdhui),
    ) %>% 
    group_by(!!sym(inputvar_name),date) %>% 
    mutate(#calcul des indicateurs par enumérateur et jour de collecte
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = sum(denominateur,na.rm = TRUE),
      type="tx_approbation",
      value=ifelse(denominateur==0,0,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Taux d'approbation au {as.character(date)}: {value}%. Soit {numerateur} questionnaire(s) approuvés sur {denominateur} interviews.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>% 
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>%
    filter(!is.na(value))  %>% distinct() %>%
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Taux cumulé d'approbation du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} questionnaire(s) approuvés(s) sur {denominateur} interviews.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>%
    select(-c(numerateur,denominateur,txt))  %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux de rejet [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de rejet par {inputvar}"))
  # tx_reject
  # Numérateur: end_result_code %in% c(3) | tx_missing>=10 | validation_status==STATUS_NOTAPPROVED
  # Dénominateur: nombre total de questionnaire sur le serveur doublons inclus
  tmp <- survey_data %>%
    # Ajoute du taux de missing à la base de données
    left_join(
      monitoring_data %>% 
        filter(type=="tx_missing_intra") %>% 
        select(caseid,tx_missing=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(tx_missing)) %>%
    mutate(
      date=floor_date(submission_time,days(1)),
      # numerateur = as.numeric((end_result_code %in% c(3) | tx_missing>=10 | validation_status==config$STATUS_NOTAPPROVED) & date <= config$aujourdhui),
      # Dans le cas de cette étude la validation est automatique on a pas d'action humaine d'où l'exclusion du contrôle sur validation_status
      numerateur = as.numeric((end_result_code %in% c(2,3) | tx_missing>=10) & date <= config$aujourdhui),
      denominateur = as.numeric(date <= config$aujourdhui),
    ) %>% 
    group_by(!!sym(inputvar_name),date) %>%
    mutate(#calcul des indicateurs par enumérateur et jour de collecte
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = sum(denominateur,na.rm = TRUE),
      type="tx_reject",
      value=ifelse(denominateur==0,0,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Taux de rejet au {as.character(date)}: {value}%. Soit {numerateur} questionnaire(s) rejetés sur {denominateur} interviews.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>% 
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>%
    filter(!is.na(value))  %>% distinct() %>%
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Taux cumulé de rejet du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} questionnaire(s) rejeté(s) sur {denominateur} interviews.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>%
    select(-c(numerateur,denominateur,txt))  %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux de consentement [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de consentement par {inputvar}"))
  # tx_consentement
  # Numérateur = consent==1
  # Dénominateur = validation_status==STATUS_APPROVED, doublons exclus
  tmp <- survey_data %>% 
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      numerateur = as.numeric(consent==1  & date <= config$aujourdhui),
      denominateur = as.numeric(date <= config$aujourdhui)
    ) %>% 
    group_by(!!sym(inputvar_name),date) %>% arrange(date) %>% 
    mutate( #calcul des indicateurs par enumérateur et jour de collecte
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = sum(denominateur,na.rm = TRUE),
      type="tx_consentement",
      value=ifelse(denominateur==0,0,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Taux de consentement au {as.character(date)}: {value}%. Soit {numerateur} interviews consentis sur {denominateur} questionnaires effectués")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>%  
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>% 
    filter(!is.na(value)) %>% distinct() %>% 
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Taux cumulé de consentement du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} interviews consentis sur {denominateur} questionnaires effectués.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>% 
    select(-c(numerateur,denominateur,txt)) %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux de couverture  [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de couverture par {inputvar}"))
  # tx_couverture
  # Numérateur = Nombre de questionnaire consent==1 & validation_status==STATUS_APPROVED
  # Dénominateur = nombre de questionnaire attendu selon l'échantillon
  tmp <- survey_data %>% 
    left_join(
      monitoring_data %>% 
        filter(type=="tx_missing_intra") %>% 
        select(caseid,tx_missing=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(tx_missing)) %>%
    # on supprime les doublons systèmes des données
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>%
    # On va identifier le nombre d'agent par variable
    group_by(!!sym(inputvar_name)) %>% 
    mutate(enumerator_count = length(unique(enumerator))) %>% ungroup() %>% 
    # Exécution du programme
    mutate(
      date=floor_date(submission_time,days(1)),
      numerateur = as.numeric(consent==1 & end_result_code %in% c(1) & tx_missing<10),
    ) %>%
    group_by(!!sym(inputvar_name),date) %>% arrange(date) %>%   
    mutate(
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = config$seuil_journalier_agent * enumerator_count,
      type="tx_couverture",
      value=ifelse(denominateur==0,100,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Taux de couverture au {as.character(date)}: {value}%. Soit {numerateur} interviews consentis et approuvés sur {denominateur} questionnaires attendus à ce jour.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>%  ungroup() %>% 
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>% 
    filter(!is.na(value)) %>% distinct() %>%
    mutate(
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Taux cumulé de couverture du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} interviews consentis et approuvés sur {denominateur} questionnaires attendus à ce jour.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>% 
    select(-c(numerateur,denominateur,txt)) %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du taux de progression  [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du taux de progression par {inputvar}"))
  # tx_progression
  # Numérateur = Nombre de questionnaire consent==1 & validation_status==STATUS_APPROVED
  # Dénominateur = nombre de questionnaire attendu selon l'échantillon
  
  # Extraction des effectifs attendus tels que décrits dans l'échantillon
  if(inputvar!="population"){
    
    tmp_deploiement <- db_deploiement
    
    if(inputvar=="localite") #Ajustement au cas du RMS TCHAD
      tmp_deploiement <- config$sample %>% rename(number_waited_surveys=echantillon)
    
    tmp_deploiement <- tmp_deploiement %>%
      mutate("{inputvar}":=!!sym(inputvar_id)) %>% 
      group_by(!!sym(inputvar)) %>% 
      mutate(number_waited_surveys=sum(number_waited_surveys,na.rm=TRUE)) %>% ungroup() %>% 
      select(!!sym(inputvar),denominateur=number_waited_surveys) %>%
      distinct()
  }else{
    tmp_deploiement <- db_deploiement %>% 
      select(enumerator=enumerator_id,nbhh_idp,nbhh_host,nbhh_refugee) %>% 
      pivot_longer(cols=!enumerator,names_to = "population_code",values_to="denominateur") %>% 
      select(-enumerator) %>% 
      mutate(population_code=str_replace_all(population_code,"nbhh_","")) %>% 
      group_by(population_code) %>% 
      mutate(denominateur = sum(denominateur,na.rm = TRUE)) %>% 
      ungroup() %>% distinct() %>% 
      mutate(
        population = case_when(
          population_code=="refugee" ~ 1,
          population_code=="idp" ~ 2,
          population_code=="returnee" ~ 3,
          population_code=="host" ~ 4
        ) 
      ) %>% select(population,denominateur)
  }
  
  tmp <-  survey_data %>% 
    #  On ajoute les effectifs du plan d'échantillonnage
    left_join(tmp_deploiement,by=join_by(!!sym(inputvar))) %>% replace_na(list(denominateur=0)) %>%
    # Ajoute du taux de missing à la base de données
    left_join(
      monitoring_data %>% 
        filter(type=="tx_missing_intra") %>% 
        select(caseid,tx_missing=value) %>% distinct(),
      by=join_by(caseid)
    ) %>% filter(!is.na(tx_missing)) %>%
    # on supprime les doublons systèmes des données
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      numerateur = as.numeric(consent==1 & end_result_code %in% c(1) & tx_missing<10),
    ) %>%
    group_by(!!sym(inputvar_name),date) %>% arrange(date) %>%   
    mutate(
      numerateur = sum(numerateur,na.rm = TRUE),
      # Pour le taux de progression le denominateur est connu depuis le début
      # de la collecte
      # denominateur = config$seuil_journalier_agent,
      type="tx_progression",
      "{inputvar}":=!!sym(inputvar_name),
      value = NA_real_,message=""
      # Le taux de progression est toujours le cumul, donc le calcul ci-dessous n'est pas nécessaire..
      # Toutefois, on le laisse à toutes fins utiles
      # value=ifelse(denominateur==0,100,round(100*numerateur/denominateur,1)),
      # message = as.character(str_glue("Taux de progression au {as.character(date)}: {value}%. Soit {numerateur} interviews consentis et approuvés sur {denominateur} questionnaires attendus à la fin de la collecte."))
    ) %>%  ungroup() %>% 
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>% 
    distinct() %>%
    mutate(
      numerateur = cumsum(numerateur),
      value=ifelse(denominateur==0,100,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Taux de progression au {as.character(date)}: {value}%. Soit {numerateur} interviews consentis et approuvés sur {denominateur} questionnaires attendus à la fin de la collecte."))
      # pour le taux de progressiono le denominateur ne change pas, il est immuable tout au long de la collectee
      # denominateur = cumsum(denominateur),
      # txt=as.character(str_glue("Taux cumulé de progression du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} interviews consentis et approuvés sur {denominateur} questionnaires attendus à la fin de la collecte.")),
      # message = as.character(str_glue("{message} {txt}"))
    ) %>% 
    select(-c(numerateur,denominateur)) %>% ungroup() %>% distinct() 
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation du nombre total de questionnaire effectué [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation du nombre de questionnaires effectués par {inputvar}"))
  if(inputvar!="population"){
    
    tmp_deploiement <- db_deploiement
    if(inputvar=="localite") tmp_deploiement <- config$sample #Ajustement au cas du RMS TCHAD
    
    tmp_deploiement <- tmp_deploiement %>% 
      mutate("{inputvar}":=!!sym(inputvar_id)) %>% 
      select(!!sym(inputvar),nbhh_idp,nbhh_host,nbhh_refugee) %>%
      pivot_longer(cols=!as.name(inputvar),names_to = "population",values_to="denominateur") %>%
      group_by(!!sym(inputvar),population) %>% 
      mutate(denominateur=sum(denominateur,na.rm=TRUE)) %>% ungroup() %>% distinct() %>% 
      pivot_wider(names_from = "population",values_from = "denominateur")
    
    tmp <- survey_data %>%
      # ajout des  tailles des échantillons
      left_join(tmp_deploiement,by=join_by(!!sym(inputvar))) %>%
      # Ajoute du taux de missing à la base de données
      left_join(
        monitoring_data %>% 
          filter(type=="tx_missing_intra") %>% 
          select(caseid,tx_missing=value) %>% distinct(),
        by=join_by(caseid)
      ) %>% filter(!is.na(tx_missing)) %>%
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>%
      mutate(
        date = floor_date(submission_time,days(1)),
        #' [Ancien code, on a pris en compte ceux qui ont déclaré des questionnnaires non complet]
        # total = as.numeric(tx_missing<10 & date <= config$aujourdhui),
        # eff_idp = as.numeric(population_code=="idp" & tx_missing<10 & date <= config$aujourdhui),
        # eff_host = as.numeric(population_code=="host" & tx_missing<10 & date <= config$aujourdhui),
        # # eff_returnee = as.numeric(population_code=="returnee" & tx_missing<10 & date <= config$aujourdhui),
        # eff_refugee = as.numeric(population_code=="refugee" & tx_missing<10 & date <= config$aujourdhui)
        #' [Nouve code, on ajuste par rapport à l'observation plus haut]
        total = as.numeric(tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1)),
        eff_idp = as.numeric(population_code=="idp" & tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1)),
        eff_host = as.numeric(population_code=="host" & tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1)),
        # eff_returnee = as.numeric(population_code=="returnee" & tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1)),
        eff_refugee = as.numeric(population_code=="refugee" & tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1))
      ) %>%
      group_by(!!sym(inputvar_name),date) %>% arrange(date) %>%
      mutate( #calcul des indicateurs par inputvar_name et jour de collecte
        total = sum(total,na.rm = TRUE),
        eff_idp = sum(eff_idp,na.rm = TRUE),
        eff_host = sum(eff_host,na.rm = TRUE),
        # eff_returnee = sum(eff_returnee,na.rm = TRUE),
        eff_refugee = sum(eff_refugee,na.rm = TRUE),
        type="nb_interview",
        value=total,
        # message = as.character(str_glue("Nombre de questionnaires effectués pour {.data[[inputvar_name]]} au {as.character(date)} => {value}. Soit {eff_idp} ménage(s) de PDIs; {eff_host} ménage(s) de hôtes; {eff_refugee} ménage(s) de réfugiés et {eff_returnee} ménage(s) de retournés.")),
        message = as.character(str_glue("Nombre de questionnaires effectués au {as.character(date)} => {value}. Soit {eff_idp} ménage(s) de PDIs; {eff_host} ménage(s) de hôtes et {eff_refugee} ménage(s) de réfugiés.")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>%
      group_by(!!sym(inputvar)) %>% arrange(date) %>%
      # select(date,!!sym(inputvar),type,value,message,total,eff_idp,eff_host,eff_returnee,eff_refugee,
      #        nbhh_idp,nbhh_host,nbhh_returnee,nbhh_refugee,!!sym(inputvar_name)) %>%
      select(date,!!sym(inputvar),type,value,message,total,eff_idp,eff_host,eff_refugee,nbhh_idp,
             nbhh_host,nbhh_refugee,!!sym(inputvar_name)) %>%
      filter(!is.na(value)) %>% distinct() %>%
      group_by(!!sym(inputvar)) %>% arrange(date) %>%
      mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
        total = cumsum(total),
        eff_cumul_idp = cumsum(eff_idp),
        eff_cumul_host = cumsum(eff_host),
        # eff_cumul_returnee = cumsum(eff_returnee),
        eff_cumul_refugee = cumsum(eff_refugee),
        cover_num = ifelse(eff_cumul_idp>nbhh_idp,nbhh_idp,eff_cumul_idp) + 
          ifelse(eff_cumul_host>nbhh_host,nbhh_host,eff_cumul_host) + 
          ifelse(eff_cumul_refugee>nbhh_refugee,nbhh_refugee,eff_cumul_refugee),
        cover_denom = nbhh_idp+nbhh_host+nbhh_refugee,
        # cover_msg = as.character(str_glue("The coverage rate to date is {round(100*cover_num/cover_denom)}%.")),
        cover_msg = as.character(str_glue("Le taux de couverture à ce jour est de {round(100*cover_num/cover_denom)}%. Soit {cover_num} couverts / {cover_denom} attendus.")),
        # txt=as.character(str_glue("Nombre cumulé de questionnaires effectués pour {.data[[inputvar_name]]} du début de la collecte au {as.character(date)} => {total}. Soit {eff_cumul_idp}/{nbhh_idp} ménage(s) de PDIs; {eff_cumul_host}/{nbhh_host} ménage(s) de hôtes; {eff_cumul_refugee}/{nbhh_refugee} ménage(s) de réfugiés et {eff_cumul_returnee}/{nbhh_returnee} ménage(s) de retournés")),
        # txt=as.character(str_glue("Depuis le début de la collecte au {as.character(date)} => {total} questionnaires ont été effectués sur {nbhh_idp+nbhh_host+nbhh_refugee+nbhh_returnee} attendus. Soit {eff_cumul_idp} ménage(s) de PDIs sur {nbhh_idp} attendus; {eff_cumul_host} ménage(s) hôtes sur {nbhh_host} attendus; {eff_cumul_refugee} ménage(s) de réfugiés sur {nbhh_refugee} attendus et {eff_cumul_returnee} ménage(s) de retournés sur {nbhh_returnee} attendus.")),
        txt=as.character(str_glue("Depuis le début de la collecte au {as.character(date)} => {total} questionnaires ont été effectués sur {nbhh_idp+nbhh_host+nbhh_refugee} attendus. Soit {eff_cumul_idp} ménage(s) de PDIs sur {nbhh_idp} attendus; {eff_cumul_host} ménage(s) hôtes sur {nbhh_host} attendus et {eff_cumul_refugee} ménage(s) de réfugiés sur {nbhh_refugee} attendus.")),
        message = as.character(str_glue("{message} {txt} {cover_msg}"))
      ) %>%
      # select(-c(total,eff_idp,eff_host,eff_returnee,eff_refugee,txt,
      #           eff_cumul_idp,eff_cumul_host,eff_cumul_returnee,eff_cumul_refugee,
      #           nbhh_idp,nbhh_host,nbhh_returnee,nbhh_refugee,!!sym(inputvar_name))) %>% 
      select(-c(total,eff_idp,eff_host,eff_refugee,txt, eff_cumul_idp,eff_cumul_host,eff_cumul_refugee,
                cover_num,cover_denom,cover_msg,
                nbhh_idp,nbhh_host,nbhh_refugee,!!sym(inputvar_name))) %>% 
      ungroup() %>% distinct()
  }else{
    # Calcul des effectifs attendus par l'échantillonnage
    tmp_deploiement <- db_deploiement %>%
      select(enumerator=enumerator_id,nbhh_idp,nbhh_host,nbhh_refugee) %>% 
      pivot_longer(cols=!enumerator,names_to = "population",values_to="denominateur") %>% 
      select(-enumerator) %>% group_by(population) %>% 
      mutate(denominateur = sum(denominateur,na.rm = TRUE)) %>% ungroup() %>% distinct() %>% 
      mutate(
        population = case_when(population=="nbhh_refugee" ~ 1,population=="nbhh_idp" ~ 2,
                               population=="nbhh_host" ~ 4)
        # population = case_when(population=="nbhh_refugee" ~ 1,population=="nbhh_idp" ~ 2,
        #   population=="nbhh_returnee" ~ 3,population=="nbhh_host" ~ 4
      )
    # Calcul de l'indicateur d'intérêt pour le cas du champ population
    tmp <- survey_data %>%
      # Ajoute des effectifs attendus par type de population
      left_join(tmp_deploiement, by=join_by(population)) %>% 
      # Ajoute du taux de missing à la base de données
      left_join(
        monitoring_data %>% 
          filter(type=="tx_missing_intra") %>% 
          select(caseid,tx_missing=value) %>% distinct(),
        by=join_by(caseid)
      ) %>% filter(!is.na(tx_missing)) %>%
      # on supprime les doublons systèmes de la liste
      group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>%
      mutate(
        date = floor_date(submission_time,days(1)),
        #* [Ajustement du code en prenant en excluant les questionnaires non complet]
        # total = as.numeric(tx_missing<10 & date <= config$aujourdhui)
        total = as.numeric(tx_missing<10 & date <= config$aujourdhui & consent == 1 & end_result_code %in% c(1))
      ) %>%
      group_by(!!sym(inputvar_name),date) %>% arrange(date) %>%
      mutate( #calcul des indicateurs par inputvar_name et jour de collecte
        total = sum(total,na.rm = TRUE),
        type="nb_interview",
        value=total,
        message = as.character(str_glue("Nombre de questionnaires << {str_to_lower(.data[[inputvar_name]])} >> effectués au {as.character(date)} => {value}.")),
        "{inputvar}":=!!sym(inputvar_name)
      ) %>% ungroup() %>%
      group_by(!!sym(inputvar)) %>% arrange(date) %>%
      select(date,!!sym(inputvar),type,value,message,total,denominateur,!!sym(inputvar_name)) %>%
      filter(!is.na(value)) %>% distinct() %>%
      group_by(!!sym(inputvar)) %>% arrange(date) %>%
      mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
        total = cumsum(total),
        txt=as.character(str_glue("Nombre de questionnaires << {str_to_lower(.data[[inputvar_name]])} >> effectués depuis le début de la collecte jusqu'au {as.character(date)} => {total} questionnaires ménages << {str_to_lower(.data[[inputvar_name]])} >> effectués sur {denominateur} attendus.")),
        message = as.character(str_glue("{message} {txt}"))
      ) %>%
      select(-c(total,denominateur,txt,!!sym(inputvar_name))) %>% ungroup() %>% distinct() 
  }
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation de la taille moyenne de ménage par jour  [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la taille moyenne des ménages par {inputvar}"))
  tmp <- survey_data %>%
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>%
    mutate(
      date=floor_date(submission_time,days(1)),
      hhsize = hh_size,
    ) %>%
    group_by(!!sym(inputvar_name),date) %>% arrange(date) %>%
    mutate( #calcul des indicateurs par enumérateur et jour de collecte
      hhsize = round(mean(hhsize,na.rm = TRUE),0),
      type="nb_hhmembers",
      value=hhsize,
      message = as.character(str_glue("Taille moyenne de membres de ménage au {as.character(date)} => {value}.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>%
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    select(date,!!sym(inputvar),type,value,message,hhsize) %>%
    filter(!is.na(value)) %>% distinct() %>%
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      hhsize = round(cummean(hhsize),0),
      txt=as.character(str_glue("Taille moyenne de membres de ménage du début de la collecte au {as.character(date)} => {hhsize}.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>%
    select(-c(hhsize,txt)) %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation de la proportion des ménages d'au plus 1 personne par jour ainsi que la proportion cumulé  
  #* [unite=agent par jour de collecte]
  #***************************************************************************************************************
  #* Un agent qui a un nombre élevé de ménages de ménage d'une personne est susceptible de falsifier les données.. 
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la proportion de ménage d'une personne par {inputvar}"))
  tmp <- survey_data %>%
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      numerateur = as.numeric(hh_size==1  & consent==1 & end_result_code %in% c(1) & date <= config$aujourdhui),
      denominateur = as.numeric(consent==1 & end_result_code %in% c(1) & date <= config$aujourdhui)
    ) %>% 
    group_by(!!sym(inputvar_name),date) %>% arrange(date) %>% 
    mutate( #calcul des indicateurs par enumérateur et jour de collecte
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = sum(denominateur,na.rm = TRUE),
      type="prop_hhsize_one",
      value=ifelse(denominateur==0,0,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Proportion de ménage d'une personne au {as.character(date)}: {value}%. Soit {numerateur} ménage(s) d'une personne sur {denominateur} ménages interviewés.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>%  
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>% 
    filter(!is.na(value)) %>% distinct() %>% 
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Proportion cumulé de ménage d'une personne du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} ménage(s) d'une personne sur {denominateur} ménages interviewés.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>% 
    select(-c(numerateur,denominateur,txt)) %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
  
  #***************************************************************************************************************
  #* Évaluation de la proportion des ménages d'au plus 3 personne par jour ainsi que la proportion cumulé  
  #* [unite=agent par jour de collecte]
  #***************************************************************************************************************
  cat("\n",glue::glue_col("{yellow Charles >} Evaluation de la proportion des ménages d'au plus 3 personnes par {inputvar}"))
  #* Un agent qui a un nombre élevé de ménages de ménage d'une personne est susceptible de falsifier les données.. 
  tmp <- survey_data %>%
    # on supprime les doublons systèmes de la liste
    group_by(uuid) %>% arrange(desc(end)) %>% slice_head(n=1) %>% ungroup() %>% 
    mutate(
      date=floor_date(submission_time,days(1)),
      numerateur = as.numeric(hh_size>=3  & consent==1 & end_result_code %in% c(1) & date <= config$aujourdhui),
      denominateur = as.numeric(consent==1 & end_result_code %in% c(1) & date <= config$aujourdhui)
    ) %>% 
    group_by(!!sym(inputvar_name),date) %>% arrange(date) %>% 
    mutate( #calcul des indicateurs par enumérateur et jour de collecte
      numerateur = sum(numerateur,na.rm = TRUE),
      denominateur = sum(denominateur,na.rm = TRUE),
      type="prop_hhsize_atMost3",
      value=ifelse(denominateur==0,0,round(100*numerateur/denominateur,1)),
      message = as.character(str_glue("Proportion de ménage d'au plus trois (3) personnes au {as.character(date)}: {value}%. Soit {numerateur} ménage(s) d'au plus trois personnes sur {denominateur} ménages interviewés.")),
      "{inputvar}":=!!sym(inputvar_name)
    ) %>% ungroup() %>%  
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    select(date,!!sym(inputvar),type,value,message,numerateur,denominateur) %>% 
    filter(!is.na(value)) %>% distinct() %>% 
    group_by(!!sym(inputvar)) %>% arrange(date) %>%
    mutate(#calcul du cumul indicateurs par enumérateur depuis le début de la collecte
      numerateur = cumsum(numerateur),
      denominateur = cumsum(denominateur),
      txt=as.character(str_glue("Proportion cumulé de ménage d'au plus trois personnes du début de la collecte au {as.character(date)}: {ifelse(denominateur==0,0,round(100*numerateur/denominateur,1))}%. Soit {numerateur} ménage(s) d'au plus trois personnes sur {denominateur} ménages interviewés.")),
      message = as.character(str_glue("{message} {txt}"))
    ) %>% 
    select(-c(numerateur,denominateur,txt)) %>% ungroup() %>% distinct()
  
  if(!is.null(tmp) & nrow(tmp)>0) result <- rbind(result,tmp)
   
  # Retourne la base de données des KPIs (key Performance Indicator).
  cat("\n",glue::glue_col("{green [Fin programme]}"))
  return(invisible(result))
}

#* [Mesure de la similarité] 
#* Vérifier les anomalies à l'aide de la fonction silhouette. Nous supposons que l'ensemble de données est regroupé 
#* en utilisant les ID des recenseurs comme ID des grappes et nous calculons la silhouette pour ce scénario 
#* de regroupement. Une valeur de silhouette proche de 1 indique que les entrées de la grappe sont très similaires
#* les unes aux autres et très différentes des entrées des autres grappes. Nous devons donc lever un drapeau si 
#* la valeur de la silhouette est  proche de 1 pour l'une des grappes ou l'un des recenseurs.
calculateEnumeratorSimilarity <- function(data, tool.survey, col_enum, col_admin){
  # helper function
  convertColTypes <- function(data, tool.survey){
    # select_multiple: numeric or factor?
    col.types <- data.frame(column=colnames(data)) %>% 
      left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
      mutate(type.edited = case_when(
        # type %in% c("integer", "decimal", "calculate") ~ "numeric",
        type %in% c("integer", "decimal") ~ "numeric",
        str_starts(type, "select_") ~ "factor",
        str_detect(column, "/") ~ "factor",
        TRUE ~ "text")) #"calculate"
    
    cols <- col.types[col.types$type.edited=="numeric", "column"]
    data[,cols] <- lapply(data[,cols], as.numeric)
    cols <- col.types[col.types$type.edited=="text", "column"]
    data[,cols] <- lapply(data[,cols], as.character)
    cols <- col.types[col.types$type.edited=="factor", "column"]
    data[,cols] <- lapply(data[,cols], as.factor)
    
    return(data)
  }
  
  # convert columns using the tool
  data <- convertColTypes(data, tool.survey)
  # keep only relevant columns
  
  cols <- data.frame(column=colnames(data)) %>% 
    left_join(select(tool.survey, name, type), by=c("column"="name")) %>% 
    filter(!(type %in% c("date", "start", "end", "today", 
                         "audit", "note", "calculate", "deviceid", "geopoint")) &
             !str_starts(column, "_"))
  
  # convert character columns to factor and add enum.id
  data <- data[, all_of(cols$column)] %>% 
    mutate_if(is.character, factor) %>% 
    arrange(!!sym(col_enum)) %>%
    mutate(enum.id=as.numeric(!!sym(col_enum)), .after=!!sym(col_enum))
  
  # calculate similarity (for enumerators who completed at least 5 surveys)
  res <- data %>% split(data[[col_admin]]) %>% 
    lapply(function(gov){
      df <- gov %>% 
        group_by(enum.id) %>% mutate(n=n()) %>% filter(n>=5) %>% ungroup() %>% 
        select_if(function(x) any(!is.na(x)))
      if (length(unique(df$enum.id)) > 1){
        # calculate gower distance
        gower_dist <- cluster::daisy(select(df, -c(!!sym(col_enum), enum.id)),
                                     metric = "gower", warnBin = F, warnAsym = F,
                                     warnConst = F)
        gower_mat <- as.matrix(gower_dist)
        # calculate silhouette
        si <- cluster::silhouette(df$enum.id, gower_dist)
        res.si <- summary(si)
        # create output
        r <- data.frame(enum.id=as.numeric(names(res.si$clus.avg.widths)), si=res.si$clus.avg.widths) %>% 
          left_join(distinct(select(df, !!sym(col_admin), !!sym(col_enum), enum.id)), by="enum.id") %>% 
          left_join(group_by(df, enum.id) %>% summarise(num.surveys=n(), .groups="drop_last"), by="enum.id") %>% 
          select(!!sym(col_admin), !!sym(col_enum), num.surveys, si) %>% arrange(-si)
        return(r)}})
  return(do.call(rbind, res))
}

#* [Calcul de la silhouette des données]
#* # # Calcule de la silhouette des données des agents par localite
# # Hypothèse : si les agents falsifient des données, la silhouette de leurs données par
# # localité avoisinnerait les 1, donc une silhouette >= 0.5 indique des données 
# # fortement similaire
# silhouette <-  get_silhouette(export=TRUE)
# config$silhouette <- silhouette 
get_silhouette <- function(export=FALSE){
  
  # Calcul de la silhouette des données pour chaque énumérateur en considerant les localités d'enquête
  # comme des grappes...
  cat("\n",glue::glue_col("{green Charles >}  [Calcul de la silhoutte des données par agent et localite]"))
  suppressWarnings(
    result <- calculateEnumeratorSimilarity(data = config$db_raw$main %>% 
                                              filter( `_id` %in% unique(config$data$caseid)) %>% 
                                              mutate(
                                                # localite=parse_number(localite),
                                                # enumerator=parse_number(enumerator)
                                                localite=labelled::to_character(localite),
                                                enumerator=labelled::to_character(enumerator)),
                                            tool.survey = config$tool_survey ,
                                            col_enum = "enumerator" ,
                                            col_admin="localite")
  )
  
  # Traitement de la base de données afin de la rendre plus compréhensioble par un humain,
  # notamment pour un profane non exercé à l'interprétation de ce type de résultat
  if(nrow(result)>0){
    row.names(result) <- NULL
    silhouette <- result %>% 
      select(localite_name=localite,enumerator_name=enumerator,number_interviews=`num.surveys`,silhouette = si) %>% 
      mutate(
        comment = case_when(
          silhouette<0.45 ~ "Données de qualité acceptable",
          silhouette>=0.45 & silhouette<0.7 ~ "Warning : Risque de falsification des données",
          silhouette>=0.7 & silhouette<0.9 ~ "Attention : Falsification probable des données",
          silhouette>=0.9 ~ "Falsification des données",
        )
      ) %>% 
      left_join(
        config$db_raw$main %>% select(zone_name=zone,localite_name=localite) %>% labelled::to_character() %>% distinct(),
        by = join_by(localite_name)
      ) %>% 
      select(zone_name,everything())
    
    
    if(export){
      cat("\n",glue::glue_col("{yellow Charles >}  [Export de la silhoutte des données par agent et localite au format excel]"))
      # Export de la silhouette au format excel..
      xls_file <- file.path("data","output","silhouette_enumerator.xlsx")
      openxlsx::write.xlsx(silhouette %>% 
                             arrange(zone_name,localite_name,enumerator_name,desc(silhouette)),
                           file=xls_file,asTable = TRUE,
                           creator="Charles Mouté",sheetName="data",keepNA=FALSE,
                           colWidths="auto")
      cat("\n",glue::glue_col("{yellow Charles >} Le fichier {xls_file} aété créé."),"\n")
    }
    # Archivage des données pour exportation
    result <- silhouette
  }
  
  invisible(result)
}

#* [Export de la base de données]
#' @author Charles MOUTE
#' @description
#' Cette fonction permet d'exporter les données pour un monitoring externe.
#' @param target Indique la cible pour laquelle on souhaite exporter les données : les agents de collecte (enumerator)  ou les gestionnaires de l'enquête (manager) ou les deux (both)
#' @param monitoringDate les données antérieures ou égales à cette date seront exportés.
#' @param check_only_today pour la production des erreurs à traiter seulement de la date du jour seront exportées.
export_dataset <- function(target="all"){
  
  var_inputs <- c("all","enumerator","datamanager","manager")
  inputvar <- match.arg(target,var_inputs)
  
  # Variables utilitaires : noms des dossiers
  db_monitoring <- config$monitoring_data %>% select(date,zone,everything())
  pki_enumerator <- get_keyPerformanceIndicators(config$data, db_monitoring,byField = "enumerator")
  
  # Variables utilitaires : nom des indicateurs à exporter en fonction du type de traitement
  # à appliquer
  var_time <- c('heure_dbt_travail','heure_fin_travail')
  var_duree <- c('duree_collecte','duree_au_travail','duree_de_travail')
  
  var_performance_interne<- str_subset(unique(db_monitoring$type),
                                       pattern = "^(tx|nb)_[a-zA-Z0-9]+_(intra)$")
  var_performance_survey <- str_subset(unique(pki_enumerator$type),
                                       pattern = "^(tx_|nb_|prop_)")
    
  var_error <- str_subset(unique(db_monitoring$type),pattern = "^(erreur_|error_)")
  var_missing<- str_subset(unique(db_monitoring$type),pattern = "^(missing_)")
  var_other<- str_subset(unique(db_monitoring$type),pattern = "^(other_)")
  
  
  if(target %in% c("all","enumerator")){
    #****************************************************************************
    # Export pour chaque enumerator et stocke dans le dossier de son pole
    # Quatre onglet dans l'ordre : missing,error,other,performance
    #****************************************************************************
    
    cat("\n\n",glue::glue_col("Charles > {red [EXPORT DES DONNEES PAR AGENT DE COLLECTE]}"),"\n")
    
    var_sheets <- list(
      missing = var_missing,
      error=setdiff(var_error,c("erreur_doublon_uuid")),
      other=var_other, 
      evaluation= var_performance_interne,
      performance=c("nb_interview",str_subset(var_performance_survey,pattern = "^(tx_)"))
    )
    
    for(var_pole in unique(db_monitoring$zone)){
      
      #**************************************************************************************** 
      #' [AJUSTEMENT AU CAS DU RMS - CREATION DES DOSSIERS RESULTATS]
      #' On supprime l'existent afin de s'assurer d'avoir toujours des dossiers avec
      #' des fichiers à jour ...
      folder_path_pole <-  file.path("local","output",var_pole)
      if(fs::dir_exists(folder_path_pole)) fs::dir_delete(folder_path_pole)
      fs::dir_create(folder_path_pole,recurse = TRUE)
      #****************************************************************************************
      
      for(var_enumerator in unique(db_monitoring %>% filter(zone==var_pole) %>% pluck("enumerator"))){
        cat("\n",glue::glue_col("{green Charles >} [Export des données de suivi de l'agent {var_enumerator}]"))
        # Variable pour la manipulation du classeur Excel
        wb <- createWorkbook(
          creator = "Charles Mouté",
          title = "Données de suivi de l'enquête",
          subject = "Monitoring & Evaluation",
        )
        enumerator_filename <- as.character(str_glue("{var_enumerator}.xlsx"))
        xls_file_enumerator <- file.path("local","output",var_pole,enumerator_filename)
        for(sheetname in names(var_sheets)){
          
          db <- db_monitoring %>%
            filter(type %in% var_sheets[[sheetname]],enumerator == var_enumerator) %>% 
            mutate(date=date(date)) %>% 
            arrange(desc(date),desc(caseid),desc(type),zone,localite,enumerator,desc(value)) %>% 
            select(-c(zone,enumerator)) %>% distinct()
          
          if(!sheetname %in% c("evaluation","performance")){
            # On crée 2 colonnes à exploiter pour les corrections
            db <-  db %>% 
              mutate(comment="",status="NO_TREATED") %>% 
              filter(message!=config$MESSAGE_DEFAULT)
          }else{
            db <-  db %>% filter(value>0)
          }
          
          if(sheetname=="performance"){
            db <- pki_enumerator %>%
              filter(type %in% var_sheets[[sheetname]],enumerator == var_enumerator) %>% 
              mutate(date=date(date)) %>%
              group_by(enumerator,type) %>% 
              arrange(desc(date),desc(value)) %>% slice_head(n=1) %>% ungroup() %>% 
              distinct() %>% 
              # arrange(desc(date),desc(type),desc(value)) %>% 
              arrange(desc(enumerator),desc(type)) %>% 
              filter(value>0)
          } 
          openxlsx::addWorksheet(wb,sheetName = sheetname)
          openxlsx::writeDataTable(wb,sheet = sheetname,db)
          cat("\n",glue::glue_col("Charles > {yellow Base de données << {sheetname} >> exporté pour {var_enumerator}}"))
        }
        
        #Export des données au format excel
        res <- openxlsx::saveWorkbook(wb,file=xls_file_enumerator,overwrite=TRUE)
        if(res){
          cat("\n",glue::glue_col("{green Charles >}  Les données de suivi de l'agent {var_enumerator} ont été correctement exportées dans le fichier {xls_file_enumerator}"),"\n")
        }else{
          cat("\n",glue::glue_col("{red Charles >} Le fichier {xls_file_enumerator} n'a pas été créé."),"\n")
        }
      }
    }
  }
  
  if(target %in% c("all","datamanager")){
    
    #****************************************************************************
    # Export des données pour la firme 
    # Quatre onglet dans l'ordre : survey, zone, region, localite, enumerator
    #****************************************************************************
    # Pour ceci les indicateurs heure_ & dure ne sont pas applicables
    # zone, pole,region, localite, population, tous le reste des pki, sont 
    # applicables...
      
    cat("\n",glue::glue_col("Charles > {red [EXPORT DES DONNEES POUR LA GDI]}"),"\n")
      
    var_sheets <- list(
      missing = var_missing,
      error=var_error,
      other=var_other, 
      evaluation=sort(c("duree_collecte",var_performance_interne)),
      performance = unique(c(var_time,var_duree,var_performance_survey))
    )
    
    # Variables utilitaires : noms des fichiers à exporter
    xls_file <- file.path("local","output","rms_monitoring_datamanager.xlsx")
    
    # var_sheet_performance <- c("performance_zone","performance_pole","performance_region",
    #                            "performance_localite","performance_enumerator",
    #                            "performance_population")
    # names(var_sheet_performance) <- c("zone","pole","region","localite","enumerator",
    #                                   "population")
    var_sheet_performance <- c("performance_zone","performance_localite",
                               "performance_enumerator","performance_population")
    names(var_sheet_performance) <- c("zone","localite","enumerator","population")
    
    
    #Variable pour la manipulation du classeur Excel
    wb <- createWorkbook(
      creator = "Charles Mouté",
      title = "Données de suivi de l'enquête",
      subject = "Monitoring & Evaluation",
    )
    
    for(sheetname in names(var_sheets)){
      # Performance est decomposee en plusieurs feuilles
      if(sheetname=="performance"){
        for(nom_onglet in var_sheet_performance){
          pki_name <- str_replace_all(nom_onglet,"performance_","")
          db <- get_keyPerformanceIndicators(config$data, db_monitoring,byField = pki_name) %>%
            filter(type %in% var_sheets[[sheetname]],value>0) %>% 
            mutate(date=date(date)) %>%
            group_by(date,!!sym(pki_name),type) %>% 
            arrange(desc(value)) %>% slice_head(n=1) %>% ungroup() %>% 
            distinct() %>% 
            arrange(desc(date),desc(type),desc(value))
          
          if(nom_onglet=="performance_enumerator"){
            # On va ajouter quelques éléments de filtre pour faciliter la vie
            # au gestionnaire de données...
            db_display <- db_monitoring %>% select(zone,enumerator) %>% distinct()
            db <- db %>% left_join(db_display,by = join_by(enumerator)) %>% 
              select(zone,enumerator,everything())
          }else{
            if(nom_onglet=="performance_localite"){
              # On va ajouter quelques éléments de filtre pour faciliter la vie
              # au gestionnaire de données...
              db_display <- db_monitoring %>% select(zone,localite) %>% distinct()
              db <- db %>% left_join(db_display,by = join_by(localite)) %>% 
                select(zone,localite,everything())
            }
          }
          
          # Export des donnees correspondantes à la feuille...
          openxlsx::addWorksheet(wb,sheetName = nom_onglet)
          openxlsx::writeDataTable(wb,sheet = nom_onglet,db)
          cat("\n",glue::glue_col("Charles > {yellow Base de données << {nom_onglet} >> exporté.}"))
        }
      }else{
        # Les autres onglets sont surtout issu de la base de données de monitoring
        # on exporte toutes les donnees sans distinction par agent de collecte...
        db <- db_monitoring %>%
          filter(type %in% var_sheets[[sheetname]]) %>% 
          mutate(date=date(date)) %>% 
          arrange(desc(date),desc(caseid),desc(type),zone,localite,enumerator,desc(value)) %>% 
          distinct()
        
        if(!sheetname %in% c("evaluation")){
          # On crée 2 colonnes à exploiter pour les corrections
          db <-  db %>% 
            mutate(comment="",status="NO_TREATED") %>% 
            filter(message!=config$MESSAGE_DEFAULT)
        }else{
          db <-  db %>% filter(value>0)
        }
        # Export des donnees correspondantes à la feuille...
        openxlsx::addWorksheet(wb,sheetName = sheetname)
        openxlsx::writeDataTable(wb,sheet = sheetname,db)
        cat("\n",glue::glue_col("Charles > {yellow Base de données << {sheetname} >> exporté}"))
      } 
    }
    
    # Export des données relatives à la silhouette
    sheet_name <- "silhouette"
    silhouette <- get_silhouette()
    openxlsx::addWorksheet(wb,sheetName = sheet_name)
    openxlsx::writeDataTable(wb,sheet = sheet_name,
                             silhouette %>% arrange(localite_name,enumerator_name,desc(silhouette)))
    cat("\n",glue::glue_col("Charles > {yellow Base de données << {sheet_name} >> exporté}"))
    
    #Export des données au format excel
    res <- openxlsx::saveWorkbook(wb,file=xls_file,overwrite=TRUE)
    if(res){
      cat("\n",glue::glue_col("{green Charles >}  Les données de suivi de l'enquête ont été correctement exportées dans le fichier {xls_file}."),"\n")
    }else{
      cat("\n",glue::glue_col("{red Charles >} Le fichier {xls_file} n'a pas été créé."),"\n")
    }
    
    #* [Code ci-dessous peut-être supprimé si on pas rms_dashboard]
    #* Cette ligne est juste ajouté pour éviter de copier manuellement les données vers le tableau de bord
    data_dashboard <- file.path("../rms_dashboard","data","rms_monitoring_datamanager.xlsx")
    res <- openxlsx::saveWorkbook(wb,file=data_dashboard,overwrite=TRUE)
    if(res){
      cat("\n",glue::glue_col("{green Charles >}  Les données pour le dashboard ont été correctement exportées dans le fichier {data_dashboard}."),"\n")
    }else{
      cat("\n",glue::glue_col("{red Charles >} Le fichier {data_dashboard} n'a pas été créé."),"\n")
    }
    
  }
  
  if(target %in% c("all","manager")){
    
    #****************************************************************************
    # Export des données pour la firme 
    # Quatre onglet dans l'ordre : survey, zone, region, localite, enumerator
    #****************************************************************************
    # Pour ceci les indicateurs heure_ & dure ne sont pas applicables
    # zone, pole,region, localite, population, tous le reste des pki, sont 
    # applicables...
    
    cat("\n",glue::glue_col("Charles > {red [EXPORT DES DONNEES POUR LE M&E]}"),"\n")
    
    var_sheets <- list(
      missing = var_missing,
      error=var_error,
      other=var_other, 
      evaluation=sort(c("duree_collecte",var_performance_interne)),
      performance = unique(c(var_time,var_duree,var_performance_survey))
    )
    
    # Variables utilitaires : noms des fichiers à exporter
    xls_file <- file.path("local","output","rms_monitoring.xlsx")
    
    
    # var_sheet_performance <- c("performance_zone","performance_pole","performance_region",
    #                            "performance_localite","performance_enumerator",
    #                            "performance_population")
    # names(var_sheet_performance) <- c("zone","pole","region","localite","enumerator",
    #                                   "population")
    
    var_sheet_performance <- c("performance_zone","performance_localite",
                               "performance_enumerator","performance_population")
    names(var_sheet_performance) <- c("zone","localite","enumerator","population")
    
    
    #Variable pour la manipulation du classeur Excel
    wb <- createWorkbook(
      creator = "Charles Mouté",
      title = "Données de suivi de l'enquête",
      subject = "Monitoring & Evaluation",
    )
    
    for(sheetname in names(var_sheets)){
      # Performance est decomposee en plusieurs feuilles
      if(sheetname=="performance"){
        for(nom_onglet in var_sheet_performance){
          pki_name <- str_replace_all(nom_onglet,"performance_","")
          db <- get_keyPerformanceIndicators(config$data, db_monitoring,byField = pki_name) %>%
            filter(type %in% var_sheets[[sheetname]],value>0) %>% 
            mutate(date=date(date)) %>%
            group_by(!!sym(pki_name),type) %>% 
            arrange(desc(date),desc(value)) %>% slice_head(n=1) %>% ungroup() %>% 
            distinct() %>% 
            # arrange(desc(type),desc(date),desc(value))
            arrange(!!sym(pki_name),desc(type))
          
          if(nom_onglet=="performance_enumerator"){
            # On va ajouter quelques éléments de filtre pour faciliter la vie
            # au gestionnaire de données...
            db_display <- db_monitoring %>% select(zone,enumerator) %>% distinct()
            db <- db %>% left_join(db_display,by = join_by(enumerator)) %>% 
              select(zone,enumerator,everything())
          }else{
            if(nom_onglet=="performance_localite"){
              # On va ajouter quelques éléments de filtre pour faciliter la vie
              # au gestionnaire de données...
              db_display <- db_monitoring %>% select(zone,localite) %>% distinct()
              db <- db %>% left_join(db_display,by = join_by(localite)) %>% 
                select(zone,localite,everything())
            }
          }
          
          # Export des donnees correspondantes à la feuille...
          openxlsx::addWorksheet(wb,sheetName = nom_onglet)
          openxlsx::writeDataTable(wb,sheet = nom_onglet,db)
          cat("\n",glue::glue_col("Charles > {yellow Base de données << {nom_onglet} >> exporté.}"))
        }
      }else{
        # Les autres onglets sont surtout issu de la base de données de monitoring
        # on exporte toutes les donnees sans distinction par agent de collecte...
        db <- db_monitoring %>%
          filter(type %in% var_sheets[[sheetname]]) %>% 
          mutate(date=date(date)) %>% 
          arrange(desc(date),desc(caseid),desc(type),zone,localite,enumerator,desc(value)) %>% 
          distinct()
        
        if(!sheetname %in% c("evaluation")){
          # On crée 2 colonnes à exploiter pour les corrections
          db <-  db %>% 
            mutate(comment="",status="NO_TREATED") %>% 
            filter(message!=config$MESSAGE_DEFAULT)
        }else{
          db <-  db %>% filter(value>0)
        }
        # Export des donnees correspondantes à la feuille...
        openxlsx::addWorksheet(wb,sheetName = sheetname)
        openxlsx::writeDataTable(wb,sheet = sheetname,db)
        cat("\n",glue::glue_col("Charles > {yellow Base de données << {sheetname} >> exporté}"))
      } 
    }
    
    # Export des données relatives à la silhouette
    sheet_name <- "silhouette"
    silhouette <- get_silhouette()
    openxlsx::addWorksheet(wb,sheetName = sheet_name)
    openxlsx::writeDataTable(wb,sheet = sheet_name,
                             silhouette %>% arrange(localite_name,enumerator_name,desc(silhouette)))
    cat("\n",glue::glue_col("Charles > {yellow Base de données << {sheet_name} >> exporté}"))
    
    #Export des données au format excel
    res <- openxlsx::saveWorkbook(wb,file=xls_file,overwrite=TRUE)
    if(res){
      cat("\n",glue::glue_col("{green Charles >}  Les données de suivi de l'enquête ont été correctement exportées dans le fichier {xls_file}."),"\n")
    }else{
      cat("\n",glue::glue_col("{red Charles >} Le fichier {xls_file} n'a pas été créé."),"\n")
    }
  }
  
}

