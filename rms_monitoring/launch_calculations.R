# Dans le cas du RMS

##Remove past activities
rm(list = ls())

## Chargement des données 
load("rms_space.Rdata")

####Load libraries and data ----

#*****************************************************************************
####ETAPE 0 : CONFIGURATION DE L'ESPACE DE TRAVAIL -----
#*****************************************************************************

# Chargement de la base de données relatif aux calculs des pondérations
# db_sample <- rio::import(file.path("local","input","calc_ponderation.xlsx"),
#                          sheet="data_sample")

db_sample <- rio::import(file.path("local","input","calc_ponderation.xlsx"),
                         sheet="data_sample")

#  Ajout des variables de poids dans la base menage
main <-  main %>%
  mutate(
    zone_id = parse_number(zone),
    zone_lab = zone_name,
    region_id = parse_number(region),
    region_lab = region_name,
    localite_id = parse_number(localite),
    localite_lab = localite_name,
    population_id = population,
    population_lab = population_name,
    # zone ...
    cluster_zone_id = parse_number(str_glue("{zone_id}{population_id}")),
    cluster_zone_name = as.character(str_glue("{zone_lab} x {population_lab}")),
    #localite 
    cluster_localite_id = parse_number(str_glue("{localite_id}{population_id}")),
    cluster_localite_name = as.character(str_glue("{localite_lab} x {population_lab}")) #,
    # cluster_id = parse_number(str_glue("{population_id}")),
    # cluster_name = as.character(str_glue("{population_lab}"))
  ) %>% 
  #  Ajout de l'identifiant  poids sample depuiscluster_zone
  dplyr::group_by(cluster_zone_id,cluster_zone_name) %>% 
  tidyr::nest() %>% 
  arrange(cluster_zone_id) %>% 
  left_join(
    db_sample %>% 
      select(cluster_zone,all_of(names(db_sample)[str_ends(names(db_sample),"_reel")])) %>% 
      mutate(cluster_id=as.numeric(cluster_zone)) %>% distinct(),
    by = join_by(cluster_zone_id==cluster_id)
  ) %>% 
  tidyr::unnest(data) %>% ungroup() %>% 
  #  Ajout de l'identifiant  poids échantillonnage depuis cluster_localite 
  dplyr::group_by(cluster_localite_id,cluster_localite_name) %>% 
  tidyr::nest() %>% 
  arrange(cluster_localite_id) %>% 
  left_join(
    db_sample %>% select(cluster_localite,all_of(names(db_sample)[str_ends(names(db_sample),"_echantillon")])) %>% 
      mutate(cluster_id=as.numeric(cluster_localite)) %>% distinct(),
    by = join_by(cluster_localite_id==cluster_id)
  ) %>% 
  tidyr::unnest(data) %>% ungroup()

# Sauvegarde & changement des valeurs d'une variables d'intérêt
main$pop_groups_save <- main$pop_groups
main$pop_groups <-  main$population_name

# #* [On verifie que la taille et la distribution est bien celle du plan d’échantillon]
# inv_poids_eff_echantillon,
# main %>% ungroup() %>% count(localite_id, localite_name,pop_groups,wt = poids_eff_echantillon)
# main %>% ungroup() %>% count(localite_id, localite_name,wt = poids_eff_echantillon) %>% pluck("n") %>% sum() %>% round(0)
# # CORRECT

# #* [On verifie que la taille et la distribution est bien celle de l'effectif réelle]
# inv_poids_eff_reel
# main %>% ungroup() %>% count(zone_id, zone_name,pop_groups,wt = poids_eff_reel)
# main %>% ungroup() %>% count(zone_id, zone_name,pop_groups,wt = poids_eff_reel) %>% pluck("n") %>% sum() %>%  round(0)
# CORRECT

#  Ajout des variables de poids dans la base individu
ind <-  ind %>% 
  left_join(
    # main %>% select(`_parent_index`=`_index`,
    #                 cluster_id,cluster_name,
    #                 poids_reel:inv_poids_ajuste_echantillon) %>% 
    #   distinct(),
    main %>% select(`_parent_index`=`_index`,
                    cluster_zone,cluster_zone_name,cluster_localite,cluster_localite_name,
                    zone_id,zone_name,localite_id,localite_name,
                    poids_eff_reel:inv_poids_prop_reel,poids_eff_echantillon:inv_poids_prop_echantillon) %>% 
      distinct(),
    by=join_by(`_parent_index`)
  ) %>% 
  # Ajustement des coefficients au niveau individuel
  mutate(
    poids_eff_reel = poids_eff_reel / hh_size,
    poids_eff_echantillon = poids_eff_echantillon / hh_size,
    inv_poids_eff_reel = inv_poids_eff_reel / hh_size,
    inv_poids_eff_echantillon = inv_poids_eff_echantillon / hh_size
  )#poids_reel,poids_echantillon

# Sauvegarde & changement des valeurs d'une variables d'intérêt
ind$pop_groups_save <- ind$pop_groups
ind$pop_groups <-  ind$population_name

#* #* [On verifie que la taille et la distribution est bien celle du plan d’échantillon]
#* En principe on doit avoir le même résultat que les tailles d'écahntillon au niveau ménage
# ind %>% ungroup() %>% count(localite_id, localite_name,pop_groups,wt = poids_eff_echantillon)
# ind %>% ungroup() %>% count(localite_id, localite_name,pop_groups,wt = poids_eff_echantillon) %>% pluck("n") %>% sum()

# 
# #* #* [On verifie que la taille et la distribution est bien celle de l'effectif réelle]
# ind %>% ungroup() %>% count(zone_id, zone_name,pop_groups,wt = poids_eff_reel)
# ind %>% ungroup() %>% count(zone_id, zone_name,pop_groups,wt = poids_eff_reel) %>% pluck("n") %>% sum()


# ###Add labels for disaggregation variables
# ###Age - HH07_cat
# table(ind$HH07_cat) # 4 categories
# table(ind$HH07_cat2) # under 18 / above 18
# 
# ##Disability - disability
# table(ind$disability)
# 
# ###Gender - HH04 -- already labelled 
# table(ind$HH04)
# 
# 
# ###Population groups
# table(ind$pop_groups)
# table(ind$population_name)
# table(main$pop_groups)
# table(main$population_name)

# Nettoyage de l'espace de travail
rm(db_sample)

# main_save %>% select(zone,zone_name) %>% distinct()

# Sauvegarde des bases de données de départ
main_save <- main %>% 
  mutate(
    index = as.numeric(`_index`), #Identifiant unique du ménage .. genere par kobo
    # Dans le cas de la base ménage parent_index = index, en réalité on devrait -1 .. 
    # cependant, vu que l'on utilise cette variable  pour configurer le plan de sondage...
    parent_index = index 
  ) #%>% 
  # #Pour produire les résultats intra-régions particulière pour le LAC
  # #* [A SUPPRIMER - CAS PARTICULIER RMS Tchad 2025]
  # filter(parse_number(zone)==7)

ind_save <-  ind %>% 
  mutate(
    index = as.numeric(`_index.x`), #Identifiant unique de l'individu
    parent_index = as.numeric(`_parent_index`) #Identifiant du ménage auquel l'individu appartient
  ) #%>% 
  # #Pour produire les résultats intra-régions particulière pour le LAC
  # #* [A SUPPRIMER - CAS PARTICULIER RMS Tchad 2025]
  # filter(parent_index %in% unique(main_save$index))

# Sauvegarde partielle du travail à toutes fins utiles...
save.image()

#**************************************************************************** 
####ETAPE 1 : PRODUCTION DES RESULTATS SANS PONDERATION -----
#****************************************************************************

# Dans le cas de ceci on a pas besoin d'exporter les graphiques puisque
# l'on ne va pas les utiliser...

#* [Initialisation des variables utilitaires]


# Initialisation des bases de données
main <-  main_save
ind <-  ind_save

# inv_poids_echantillon => DENOMINATEUR : c(HOST,REFUGEES, PDI) 
# L'exécution avec ce poids implique de garder tous les fichiers et de préfixer
# les fichiers excel avec unweigt
main <-  main %>%
  mutate(
    UNHCR_WEIGHT = poids_eff_echantillon,#inv_poids_eff_echantillon
    cluster_id = cluster_localite,
    cluster_name = cluster_localite_name
  ) %>% 
  filter(pop_groups %in% c(var_HOST,var_REFUGEES,var_IDPs))
ind <-  ind  %>%
  mutate(
    UNHCR_WEIGHT = poids_eff_echantillon,#inv_poids_eff_echantillon
    cluster_id = cluster_localite,
    cluster_name = cluster_localite_name
  ) %>% 
  filter(pop_groups %in% c(var_HOST,var_REFUGEES,var_IDPs))

#* [Calcul des indicateurs sans recours aux ponderations]
cat(glue::glue_col(
  "{green [ CALCUL DES INDICATEURS SANS RECOURS AUX PONDERATIONS ] }"
),"\n")
source("indicator_calculations.R")

#* [Export de la table des indicators du RMS Standard avec méta-données]
export_dataset(
  result %>% filter(!is.na(status)), 
  file.path("local","database","analysis","Unweighted_Combined_Indicators.xlsx")
)

#* [Export de la table des indicateurs du Core Indicator Metadata]
export_dataset(
  combined_RBM_indicators %>% filter(!is.na(status)),
  file.path("local","database","analysis","Unweighted_raw_Combined_Indicators.xlsx")
)

#* [Export de la table des p-values associés aux tests statistiques]
export_dataset(
  rms_pvalues,
  file.path("local","database","analysis","Unweighted_pvalues.xlsx")
)


#* [Sauvegarde les results du calcul des indicateurs avec pondération]
main_unweighted <-  main
ind_unweighted <-  ind
combined_RBM_indicators_unweighted <-  combined_RBM_indicators
rms_pvalues_unweighted <- rms_pvalues
result_unweighted <-  result

#* [Export des données d'analyses]
#* Les données d'analyse sont constituées de toutes les observations avec
#*  les variables  indicateurs d'impact et d'outcome ainsi que de user_define
cat(glue::glue_col(
  "{green [ EXPORT DES BASES DE DONNEES DESTINEES AUX ANALYSES ] }"
),"\n")

#Enregistrement des bases de données d'analyse
suppressWarnings(saving_datasets("analysis"))

# Suppression des variables inutiles ......
rm(main,ind,combined_RBM_indicators,rms_pvalues,result)

# On va supprimer les resultats générer précédement
rm(list=str_subset(ls(),"^(impact|outcome|user|composite_)"))
rm(list=str_subset(ls(),"(_indicators|_percentages)$"))
rm(list=setdiff(str_subset(ls(),"(_pvalues)$"),c("get_pvalues")))

#****************************************************************************
####ETAPE 2 : PRODUCTION DES RESULTATS AVEC PONDERATION -----
#****************************************************************************
# UNHCR_SAMPLE_WEIGHTING <- FALSE #par défaut FAUX


#* [Initialisation des variables utilitaires]
main <-  main_save
ind <-  ind_save

#inv_poids_reel => DENOMINATEUR : c(REFUGEES, PDI)
# L'execution avec ce poids implique de conserver uniquement les fichiers Excel
# et de les prefixé avec weighted_
main <- main %>% 
  mutate(
    UNHCR_WEIGHT=poids_eff_reel,#inv_poids_eff_reel
    cluster_id = cluster_zone_id,
    cluster_name = cluster_zone_name
  ) %>% #* Dans le cas du [TCHAD], les effectifs des hotes ont été fourni...
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs))
  filter(pop_groups %in% c(var_HOST,var_REFUGEES,var_IDPs))

ind <-  ind %>% 
  mutate(
    UNHCR_WEIGHT = poids_eff_reel, #inv_poids_eff_reel
    cluster_id = cluster_zone,
    cluster_name = cluster_zone_name
  ) %>% #* Dans le cas du [TCHAD], les effectifs des hotes ont ete fourni...
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs))
  filter(pop_groups %in% c(var_HOST,var_REFUGEES,var_IDPs))

cat(glue::glue_col(
  "{green [ CALCUL DES INDICATEURS EN AYANT RECOURS A LA PONDERATION ] }"
),"\n")
source("indicator_calculations.R")

#* [Export the combined data frame to an Excel file]
cat(glue::glue_col(
  "{green [ EXPORT DE LA TABLE FINAL DES INDICATEURS ] }"
),"\n")

# Table des indicateurs standard du RMS avec méta données
export_dataset(
  result %>% filter(!is.na(status)), 
  file.path("local","database","analysis","Weighted_Combined_Indicators.xlsx")
)

#* [Table des indicateurs relevant du Core Indicator Metadata ]
export_dataset(
  combined_RBM_indicators %>% filter(!is.na(status)),
  file.path("local","database","analysis","Weighted_raw_Combined_Indicators.xlsx")
)

#* [Export de la table des p-values associés aux tests statistiques]
export_dataset(
  rms_pvalues,
  file.path("local","database","analysis","Weighted_pvalues.xlsx")
)

#* [Export des graphiques pour les analyses]
cat(glue::glue_col(
  "{green [ EXPORT DES GRAPHIQUES DESTINEES AUX ANALYSES DU RAPPORT ] }"
),"\n")

# Enregistrement des graphiques produits
saving_graphics()

#* [Sauvegarde les résultats du calcul des indicateurs avec pondération]
main_weighted <-  main
ind_weighted <-  ind
combined_RBM_indicators_weighted <-  combined_RBM_indicators
rms_pvalues_weighted <- rms_pvalues
result_weighted <-  result

# Suppression des variables inutiles ......
rm(main,ind,combined_RBM_indicators,rms_pvalues,result)

#* PAR DEFAUT CE SONT LES VARIABLES AVEC PONDERATION QUI RESTE DANS L'ENVIRONNEMENT
#* SELON LE CAS IL PEUT ETRE NECESSAIRE DE RELANCER L'ETAPE 1 ou L'ETAPE 2
#* SELON LE BESOIN DE GRAPHIQUE OU DE TABLES RMS
save.image()

#****************************************************************************
####ETAPE 3 : PRODUCTION DES GRAPHIQUES COMPLEMENTAIRES -----
#****************************************************************************

cat(glue::glue_col(
  "{green [ EXPORT DES GRAPHIQUES COMPLEMENTAIRES DESTINEES AUX ANALYSES DU RAPPORT ] }"
),"\n")

source("graphiques_complémentaires.R")


