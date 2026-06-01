###########RMS Qv3.2 Indicator Calculations############
##########UNHCR :: CAPI#######
#########Author: Ilgi Bozdag
#########Author: Charles Mouté (Rev 2026.03.06)

#*********************************************************************** 
#####Standard Scripts for RBM Indicators##############
#***********************************************************************

##Date: October 2024
##At this step, you should already have main and ind datasets 
###structured in a way that will allow you to calculate your indicators as you will be guided in this document
###It's not recommended to clear your work space if you have your main and ind datasets loaded


#Core Impact Indicators ----

#****************************************************************************************************************************************
### 1.2 Proportion of people who are able to move freely within the country of habitual residence -----
#' [Cet indicateur ne figure pas dans le RMS Standard]
#****************************************************************************************************************************************
#' 
#' ind <- ind %>%
#'   mutate(impact1_2=case_when(
#'     L3=="1" ~ 1,
#'     L3=="0" ~ 0,
#'     TRUE ~ NA #Les camerounais dont la population hôte e
#'   )) %>%
#'   mutate(impact1_2=labelled(impact1_2,
#'                             labels =c(
#'                               "Oui"=1,
#'                               "Non"=0
#'                             ),
#'                             label="Proportion de personnes pouvant circuler librement dans le pays de résidence habituelle"))
#' 
#' RMS_XXX_202X_ind <- ind %>%
#'   # filter(pop_groups %in% var_REFUGEES) %>% # Cet indicateur ne porte que sur les Réfugiés/Demandeurs d'asile & Apatrides.. pas d'apattrides dans cette base
#'   as_survey_design(
#'     ids = cluster_id,           # Specify the column with cluster IDs
#'     # inv_poids_reel,inv_poids_ajuste_reel
#'     weights = UNHCR_WEIGHT, # Specify the column with survey weights
#'     nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
#'   )
#' 
#' 
#' #' [Generation des tables standards]
#' # agd_config <- "disability_gender_age"
#' agd_config <- NULL
#' get_rmsTable(RMS_XXX_202X_ind,indicator = "impact1_2",agd=agd_config)
#' # get_rmsTable_AGD(RMS_XXX_202X_main,indicator = "impact2_2")
#' 
#' #' [Generation des tables standards]
#' get_rmsGraphics(indicator="impact1_2",indicator_name="Impact 1.2",
#'                 plt_subtitle="Proportion de personnes pouvant circuler librement dans le pays de résidence habituelle",
#'                 agd =agd_config, plt_caption = "")
#' rm(agd_config)

#****************************************************************************************************************************************
### 2.1 Proportion of people living below the national poverty line -----
# Aucune variable ne permet de mesurer ceci .... à ajouter pour la prochaine fois
#****************************************************************************************************************************************
#  impactIndicators_21 <- function(){
#   
#   indicator_name<- "2.1 Proportion de PoC vivant en dessous du seuil national de pauvreté"
#   indicator_type <- "Core impact indicator"
#   popcible_idx <- c(1,3:5)
#   popcible <- paste(target_population[popcible_idx],collapse = ", ")
#   
#   # seuil2pauvrete <- 339715/12 # anuuellement le seuil est de 339 715 fcfa
#   seuil2pauvrete <- 36270 # anuuellement le seuil est de 339 715 fcfa
#   
#   # c("poc","1-refugee","2-host","3-asylum_seeker","4-returnee","5-idp")
#   # c("réfugiés","hôtes","demandeurs d'asile","retournés","personnes déplacées internes")
#   
#   denominateur <- readr::parse_number(numberOf(c("refugee","asylum_seeker","returnee","idp"))[["value"]])
#   numerateur <- members %>% 
#     filter(q0304 %in% popcible_idx) %>% 
#     mutate(valid_answer=as.numeric(a1501==1)) %>% #1501<seuil2pauvrete
#     filter(valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   result <- 100*numerateur/denominateur
#   
#   denom <- readr::parse_number(numberOf(c("refugee","asylum_seeker"))[["value"]])
#   num <- members %>% 
#     filter(q0304 %in% c(1,3)) %>% 
#     mutate(valid_answer=as.numeric(a1501==1)) %>% #1501<seuil2pauvrete
#     filter(valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   res <- 100*num/denom
#   
#   msg <- sprintf("[%s] Le seuil de pauvreté au Cameroun est établi en 2014 à 339 715 FCFA (INS-Cameroun, Annuaire Statistique du Cameroun, édition 2019). Toutefois, le seuil ici considéré équivaut au SMIG qui est de 36 270 FCFA au Cameroun. %s",
#                  ifelse(result>=50,"Critical",ifelse(result<=35,"Acceptable","Unacceptable")),
#                  sprintf("La proportion des réfugiés/demandeurs d'asile vivant enn dessous du seuil de pauvreté est de %.0f%%",res))
#   
#   infos <- tibble(
#     indicator=indicator_name,
#     type=indicator_type,
#     population=popcible,
#     target="0%",
#     value=sprintf("%.1f%%",result),
#     source=src_result,
#     comment=msg
#   )
#   return(infos) 
# }

#****************************************************************************************************************************************
### 2.2 Proportion of people residing in physically safe and secure settlements with access to basic facilities -----
##Module :	LIGHT01-LIGHT03 (9.2) + HEA01-HEA03 (health) +  DWA01-DWA04 (12.1) + SHEL01-SHEL06 and RISK01-RISK02 and DWE05 (9.1)
#****************************************************************************************************************************************

###All variables for this indicators are in the main dataset both for CAPI and CATI

##This indicator aims to measure the proportion of forcibly displaced and stateless people that 
##reside in safe and secure settlements with access to basic facilities such as 
##shelter, WASH, energy and security from natural hazards


###Step.1. Electricity
##Here we will create a binary variable if they use anything for lighting (LIGHT01), 
##and the light source for most of the time is electricity (LIGHT02) - 
##exclude cases if they selected LIGHT03 - 0 ( no electricity)

table(main$LIGHT01)
table(main$LIGHT02)

main <- main %>%
  mutate(electricity = ifelse(LIGHT01 == "1" & LIGHT02 == "1" & LIGHT03 != "0", 1, 0)
  ) %>%
  mutate( electricity = labelled(electricity,
                                 labels = c(
                                   "Oui" = 1,
                                   "Non" = 0
                                 ),
                                 label = "Accès à l'électricité"))
table(main$electricity)

###Step2. Healthcare
###Access to healthcare if household has any facility available excluding 'don't know' and 'other' 
#within one hour distance (cannot be > 60) (walking or any other type of transport)
main <- main %>%
  mutate(healthcare = ifelse(HEA01 != "96" & HEA01 != "98" & HEA03 <= 60, 1, 0)
  ) %>%
  mutate( healthcare = labelled(healthcare,
                                labels = c(
                                  "Oui" = 1,
                                  "Non" = 0
                                ),
                                label = "Accès à un établissement de soins de santé"))
table(main$healthcare)


###Step3. Drinking water
###Access to drinking water is calculated as below
##use improved sources of drinking water in their housing or within 30 minutes round trip collection time

###Convert time variable to minutes only
main <- main %>%
  mutate(time_DWA=case_when(
    DWA03a=="1"~ "1", DWA03a=="2"~"60" #convert hour into minutes
  ))
main$time_DWA <- as.numeric(main$time_DWA)
table(main$time_DWA)

###Compute variable with above conditions
main <- main %>%
  mutate(time_tot=time_DWA*DWA03b
  ) %>% 
  mutate(dwa_cond1=case_when( time_tot > 30 ~ 0, 
                              TRUE ~ 1) # reachable under 30 minutes or NA
  ) %>% 
  # mutate( 
  #   # improved source only
  #   #  Le code ci-dessous est incorrecte.. et conduit à une valeur distincte avec l'outcome 12.1
  #   # dwa_cond2=case_when(DWA01!="7" |DWA01 !="9" |DWA01 != "13" | DWA01 != "96" |DWA01 !="98" ~ 1,
  #   #                     TRUE ~ 0)
  #   # Pour avoir des valeurs correctes ... le code ci-dessous serait meilleur
  #   dwa_cond2=case_when(DWA01!="7" & DWA01 !="9" & DWA01 != "13" & DWA01 != "96" & DWA01 !="98" ~ 1,
  #                       TRUE ~ 0)
  # ) %>%
  # Au vu de ce qui précéde le plus simple est de copier-coller ce aui est fait  l'outcome 12.1
  mutate(dwa_cond2 = case_when(
    !DWA01 %in% c("7", "9", "13", "96", "98") ~ 1,  # Improved source (all except these codes)
    TRUE ~ 0
  )) %>%
  mutate(dwa_cond3=case_when(DWA02 == "3" ~ 0, 
                             TRUE ~ 1) # in the dwelling/yard/plot
  ) %>% 
  mutate(drinkingwater=case_when(
    ((dwa_cond1==1 | dwa_cond3==1) & dwa_cond2==1 ) ~ 1, TRUE ~ 0)
  ) %>%
  mutate(drinkingwater = labelled(drinkingwater,
                                  labels = c(
                                    "Oui" = 1,
                                    "Non" = 0
                                  ),
                                  label = "Accès à l'eau potable"))
table(main$drinkingwater)


###Step 4. Habitable housing
##Condition 1
##Classify as habitable for below conditions - if 98 selected, put into missing

##First check the variables
table(main$SHEL01)
table(main$SHEL02)
table(main$SHEL03)
table(main$SHEL04)
table(main$SHEL05)
table(main$SHEL06)


main <- main %>%
  mutate(across(starts_with("SHEL"), ~if_else(. == "98", NA, .))) %>%
  mutate(housing = case_when(
    (SHEL01 == "1") & (SHEL02 == "1") & (SHEL05 == "1") & 
      (SHEL03 == "0" ) & (SHEL04 == "0" ) & (SHEL06 == "0" ) ~ 1,
    (SHEL01 == "0") | (SHEL02 == "0" ) | (SHEL05 == "0") |
      (SHEL03 == "1") | (SHEL04 == "1") | (SHEL06 == "1" ) ~ 0,
    TRUE ~ NA_integer_
  ))

table(main$housing)


##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus not considered as physically safe

# Indice de surpeuplement - surpeuplement lorsque plus de 3 personnes partagent la même chambre pour dormir
table(main$hh_size_001)
table(main$DWE05)

main <- main %>%
  mutate(crowding=hh_size_001/DWE05
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 0)
  )


table(main$crowding)
table(main$dwe05_cat)


###Combine both conditions for habitable housing -- exclude DWE01
main <- main %>%
  mutate(shelter=case_when(
    dwe05_cat==1 & housing==1 ~ 1,
    TRUE ~ 0
  ))

table(main$shelter)



##Step 5. Safe and secure settlements are those with no risks and hazards like flooding, landslides, 
###landmines, and close proximity to military installations and hazardous zones
table(main$RISK01)
table(main$RISK02)

main <- main %>%
  mutate(secure=case_when(
    RISK01=="1" |  RISK02=="1" ~ 0,
    TRUE ~ 1
  ))

table(main$secure)


##Step 6. Combine all services
###Calculate impact indicator based on electricity, healthcare, drinkingwater, shelter and secure
##Impact 2.2 is "1" if all services above are accessible

main <-main %>%
  mutate(impact2_2=case_when(
     
    shelter==0 | electricity==0 | drinkingwater==0 | healthcare==0 | secure==0 ~ 0,
    shelter==1 & electricity==1 & drinkingwater==1 & healthcare==1 & secure==1 ~ 1)
    
  ) %>% 
  mutate(impact2_2=labelled(impact2_2,
                            labels =c(
                              "Oui"=1,
                              "Non"=0
                            ),
                            label="Proportion de personnes résidant dans des zones d'habitation physiquement sûres et sécurisées et ayant accès à des installations de base"))
table(main$impact2_2)

####Rerun to have the dataset with indicators 
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>% #Core indicator metadata - les Hôtes ne sont pas concernés
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )


####Standard tables 
# composite_impact2_2 <- main %>%
#   # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>% #Core indicator metadata - les Hôtes ne sont pas concernés
#   select(pop_groups, shelter, electricity, drinkingwater, secure, healthcare, impact2_2) %>%
#   pivot_longer(cols = shelter:healthcare, names_to = "facility", values_to = "access") %>%
#   group_by(pop_groups, facility) %>%
#   mutate(
#     facility = case_when(
#       facility == "drinkingwater" ~ "Eau potable",
#       facility == "electricity" ~ "Electricité",
#       facility == "healthcare" ~ "Etablissement de soins de santé",
#       facility == "secure" ~ "Zone d'habitation sûre et sécurisée",
#       facility == "shelter" ~ "Logement habitable",
#       TRUE ~ facility
#     )
#   ) %>% 
#   summarise(percentage = mean(access, na.rm = TRUE) * 100,.groups = "drop")

composite_impact2_2 <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>% #Core indicator metadata - les Hôtes ne sont pas concernés
  select(pop_groups, shelter, electricity, drinkingwater, secure, healthcare, impact2_2, UNHCR_WEIGHT) %>%
  pivot_longer(cols = shelter:healthcare, names_to = "facility", values_to = "access") %>%
  mutate(
    facility = case_when(
      facility == "drinkingwater" ~ "Eau", #Eau de boisson
      facility == "electricity" ~ "Électricité",
      facility == "healthcare" ~ "Établissement de soins de santé",
      facility == "secure" ~ "Zone d'habitation sûre et sécurisée",
      facility == "shelter" ~ "Logement habitable",
      TRUE ~ facility
    )
  ) %>% 
  group_by(pop_groups, facility) %>%
  summarise(
    percentage = weighted.mean(access, w = UNHCR_WEIGHT, na.rm = TRUE) * 100,
    .groups = "drop"
  )

###Chart for above with all dimensions 
impact2_2_graph03 <- composite_impact2_2 %>%
  ggplot(aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.9),vjust = -0.5, hjust=0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +
  scale_y_continuous(limits = c(0, 100), expand = c(0.02, 0.02)) +
  labs(
    title = "Accès aux équipements par groupe de population",
    caption = unhcr_caption,
    x = "Facilité",
    y = "Pourcentage d'accès (%)",
    fill = "Groupes de population"
  ) +
  theme_unhcr()

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,indicator = "impact2_2",agd=agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,indicator = "impact2_2")

#' [Generation des tables standards]
get_rmsGraphics(indicator="impact2_2",indicator_name="Impact 2.2",
                plt_subtitle="Proportion de personnes résidant dans des zones d'habitation physiquement sûres et sécurisées et ayant accès à des installations de base",
                agd =agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###2.3 Proportion of people with access to health services -----
##Module :HACC01 - HACC04
#****************************************************************************************************************************************

table(ind$HACC01) ## Needed to see a health professional for any reason
table(ind$HACC02) ## the reason for seeking care
table(ind$HACC03) ## did receive the needed health care
table(ind$HACC04) ## if not, what are the reasons for not receiving the health care


##Calculate those who needed and accessed health services

ind <- ind %>%
  mutate(impact2_3=case_when(
    HACC01=="1" & HACC03=="1" ~ 1,
    HACC01=="0" ~ NA,
    HACC01=="1" & HACC03=="0" & (HACC04_1 == 1 | HACC04_2 == 1 | HACC04_4 == 1 | HACC04_7 == 1 | HACC04_10 == 1 | HACC04_11 == 1 | 
                                   HACC04_12 == 1 | HACC04_13 == 1) ~ 0 ,
    HACC01=="1" & HACC03=="0" & (HACC04_3 == 1 | HACC04_5 == 1 | HACC04_6 == 1 | HACC04_8 == 1 | 
                                   HACC04_9 == 1 | HACC04_96 == 1) ~ 1)
  ) %>%
  mutate(impact2_3=labelled(impact2_3,
                            labels =c(
                              "Oui"=1,
                              "Non"=0
                            ),
                            label="Proportion de personnes ayant accès aux services de santé"))

###Descriptives
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES)) %>% # Indicateur portant uniquement sur les réfugiés
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_ind,"impact2_3",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_ind,"impact2_3")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="impact2_3",indicator_name="Impact 2.3",
                plt_subtitle="Proportion de personnes ayant accès aux services de santé",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#### The reasons for not being able to access to health services 
##add labels for not accessing the health services
reasons_mapping <- c(
  "HACC04_1" = "Centre de santé trop éloigné",
  "HACC04_2" = "Médicaments ou établissement de santé trop chers",
  "HACC04_3" = "Il n'existe pas de traitement/pas nécessaire",
  "HACC04_4" = "Ne sait pas où aller",
  "HACC04_5" = "Pas le temps",
  "HACC04_6" = "Préférer d'autres options",
  "HACC04_7" = "L'établissement de santé n'accepte pas de nouveaux patients",
  "HACC04_8" = "Ne fait pas confiance à la médecine moderne",
  "HACC04_9" = "Ne fait pas confiance aux médecins",
  "HACC04_10" = "Problèmes administratifs/documentaires (certificats, cartes de service, etc.)",
  "HACC04_11" = "Longs temps d'attente",
  "HACC04_12" = "Manque de fournitures médicales",
  "HACC04_13" = "Établissement de santé endommagé/détruit"
)

hacc_percentages <- ind %>%
  summarise(
    HACC04_1 = weighted.mean(HACC04_1 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_2 = weighted.mean(HACC04_2 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_3 = weighted.mean(HACC04_3 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_4 = weighted.mean(HACC04_4 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_5 = weighted.mean(HACC04_5 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_6 = weighted.mean(HACC04_6 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_7 = weighted.mean(HACC04_7 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_8 = weighted.mean(HACC04_8 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_9 = weighted.mean(HACC04_9 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_10 = weighted.mean(HACC04_10 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_11 = weighted.mean(HACC04_11 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_12 = weighted.mean(HACC04_12 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,
    HACC04_13 = weighted.mean(HACC04_13 == 1, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = everything(), 
               names_to = "Reason", 
               values_to = "Percentage") %>%
  mutate(Reason = reasons_mapping[Reason])  # Map column names to descriptive labels


##Chart creation
impact2_3_graph03 <- ggplot(hacc_percentages, aes(x = reorder(Reason, Percentage), y = Percentage, fill = Reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Raisons du non-accès aux services de santé",
    x = "Raison",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Les pourcentages sont calculés indépendamment pour chaque raison.")
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

#****************************************************************************************************************************************
###3.1 Proportion of people who have the right to decent work -----
#' [Indicator non disponible dans le questionnaire standard]
#' Ceci est un ajout on le reajustera après... 
#****************************************************************************************************************************************
#' main <- main %>%
#'   mutate(
#'     decent_work_age = case_when(
#'       HH07 >= 18 ~ 1,  # If 18 or older, mark as working age (numeric)
#'       TRUE ~ NA_real_  # Set to NA if not 18+
#'     )
#'   ) %>%
#'   mutate(
#'     
#'     #* [Code d'origine]
#'     # impact3_1=case_when(
#'     #   (BQ301=="1" | BQ302=="1") & (BQ304=="1" &  (BQ305=="1" | BQ305=="2") ) & decent_work_age==1 & pop_groups %in% c(var_REFUGEES) ~ 1,
#'     #   decent_work_age==1  & pop_groups %in% c(var_REFUGEES) ~ 0,
#'     #   TRUE ~ NA_real_  # Set to NA for those not in the working age group
#'     # )
#'     
#'     #* [Code adapté au cas du RMS Tchad]
#'     impact3_1=case_when(
#'       (BQ301=="1" | BQ302=="1") & (BQ304=="1" &  (BQ305=="1" | BQ305=="2") ) & decent_work_age==1 ~ 1,
#'       decent_work_age==1 ~ 0,
#'       TRUE ~ NA_real_  # Set to NA for those not in the working age group
#'     )
#'   ) %>% 
#'   mutate(
#'     impact3_1=labelled(impact3_1,labels =c("Oui"=1,"Non"=0),label="Proportion de personnes ayant droit à un travail décent")
#'   )
#' 
#' ###Descriptives
#' RMS_XXX_202X_main <- main %>%
#'   # filter(pop_groups %in% c(var_REFUGEES)) %>% 
#'   filter(decent_work_age==1) %>% 
#'   as_survey_design(
#'     ids = cluster_id,           # Specify the column with cluster IDs
#'     # inv_poids_reel,inv_poids_ajuste_reel
#'     weights = UNHCR_WEIGHT, # Specify the column with survey weights
#'     nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
#'   )
#' 
#' #' [Generation des tables standards]
#' # get_rmsTable(RMS_XXX_202X_main,indicator = "impact3_1")
#' # agd_config <- "disability_gender"
#' agd_config <- NULL
#' get_rmsTable(RMS_XXX_202X_main,"impact3_1",agd = agd_config)
#' # get_rmsTable_AGD(RMS_XXX_202X_main,"impact3_1",gender=TRUE, disability=TRUE, age = FALSE)
#' 
#' #' [Generation des graphiques standards]
#' get_rmsGraphics(indicator="impact3_1",indicator_name="Impact 3.1",
#'                 plt_subtitle="Proportion de personnes ayant droit à un travail décent",
#'                 agd = agd_config, plt_caption = "Âge de travail décent : 18 ans et plus")
#' rm(agd_config)


#****************************************************************************************************************************************
###3.2a: Proportion of children and young people enrolled in primary education -----
##Module :EDU01-EDU04
#****************************************************************************************************************************************
ind <- ind %>% 
  mutate(
    edu_primary = case_when(
      EDU01 == "1" & EDU02 == "1" & EDU03 == "2" ~ 1,  
      EDU01 == "0" | EDU02 == "0" ~ 0,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    age_primary = case_when(
      HH07 >= 6 & HH07 <= 11 ~ 1,  ###ADJUST AGE GROUPS FOR PRIMARY LEVEL ( 6- 11 in this ex)
      TRUE ~ 0
    )
  )

# # # Verification des denominateur et numérateurs..
# table(ind$impact3_2a) %>% addmargins()
# table(with(ind,edu_primary)) %>% addmargins()
# table(ind$age_primary) %>% addmargins()
# table(with(ind,edu_primary[age_primary==1])) %>% addmargins()

###Results of the indicator table
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%  
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE             # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender"
get_rmsTable(RMS_XXX_202X_ind,"impact3_2a",agd = agd_config,get_ratio = TRUE,
               var_numerator = "edu_primary",var_denominator = "age_primary")

get_rmsTable_AGD(RMS_XXX_202X_ind,"impact3_2a",
                 disability=TRUE, gender=TRUE, age = FALSE, get_ratio = TRUE,
                 var_numerator = "edu_primary",var_denominator = "age_primary")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="impact3_2a",indicator_name="Impact 3.2a",
                plt_subtitle="Proportion d'enfants et de jeunes inscrits dans l'enseignement primaire",
                agd = agd_config,plt_caption = "Groupe d'âge pour le primaire : 6-11 ans.")
rm(agd_config)

#****************************************************************************************************************************************
###3.2b: Proportion of children and young people enrolled in secondary education -----
##Module :EDU01-EDU04
#****************************************************************************************************************************************

#Turn character variables into vector
###This indicator comes from the individual dataset
###Include if they are attending secondary or secondary -technical and vocational
ind <- ind %>%
  mutate(
    edu_secondary = case_when(
      EDU01 == "1" & EDU02 == "1" & (EDU03 == "3" | EDU03 == "4") ~ 1,
      EDU01 == "0" | EDU02 == "0" ~ 0,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    age_secondary = case_when(
      HH07 >= 12 & HH07 <= 18 ~ 1,  # ADJUST THE SCHOOL AGE FOR SECONDARY ( 12 to 18 in this ex)
      TRUE ~ NA_real_  # NA_real_ for missing numeric values
    )
  )

###Results of the indicator table
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE    
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender"
get_rmsTable(RMS_XXX_202X_ind,"impact3_2b",agd = agd_config,get_ratio = TRUE,
             var_numerator = "edu_secondary",var_denominator = "age_secondary")

get_rmsTable_AGD(RMS_XXX_202X_ind,"impact3_2b",
                 gender=TRUE, disability=TRUE, age = FALSE, get_ratio = TRUE,
                 var_numerator = "edu_secondary",var_denominator = "age_secondary")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="impact3_2b",indicator_name="Impact 3.2b",
                plt_subtitle="Proportion d'enfants et de jeunes (12-18) inscrits dans l'enseignement secondaire",
                agd = agd_config, plt_caption = "Groupe d'âge pour le secondaire : 12-18 ans")
rm(agd_config)

#****************************************************************************************************************************************
###3.3 Proportion of people that feel safe walking alone in their neighbourhood after dark ----
##Module :SAF01
#****************************************************************************************************************************************

##This indicator comes from main dataset based on the respondent randomly selected for individual level
###Indicator calculations
main <- main %>%
  mutate(
    impact3_3 = case_when(
      SAF01 == "1" | SAF01 == "2" ~ 1,  # Assign 1 for Yes (feels safe)
      SAF01 == "3" | SAF01 == "4" ~ 0,  # Assign 0 for No (does not feel safe)
      SAF01 == "98" | SAF01 == "99" ~ NA_real_  # Handle missing/unknown values with NA_real_
    )
  ) %>%
  mutate(
    impact3_3 = labelled(
      impact3_3,
      labels = c(
        "Oui" = 1,  # Label for Yes (1)
        "Non" = 0    # Label for No (0)
      ),
      label = "Proportion de personnes qui se sentent en sécurité lorsqu'elles se promènent seules dans leur quartier après la tombée de la nuit"  # Assign the variable label
    )
  )

###Table standard 
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # poids_reel,poids_ajuste_reel,inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"impact3_3",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"impact3_3")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="impact3_3",indicator_name="Impact 3.3",
                plt_subtitle="Proportion de personnes qui se sentent en sécurité lorsqu'elles se promènent seules dans leur quartier après la tombée de la nuit",
                agd = agd_config, plt_caption = "")
rm(agd_config)


###Outcome Indicators
#Core Outcome Indicators ----

#****************************************************************************************************************************************
###1.2 Proportion of children under 5 years of age whose births have been registered with a civil authority -----
##Module :REG03 - REG04
# ind$REG03 - birth certificate
# ind$REG04 - birth has been registered
#****************************************************************************************************************************************

##Calculate children who has a birth certificate
ind <- ind %>%
  mutate(birthCertificate = case_when(
    REG03 == "0" | REG03 == "98" ~ 0,
    REG03 == "1" ~ 1,
    TRUE ~ NA_real_  # Handle any missing or unknown values
  )) %>%
  mutate(birthCertificate = labelled(
    birthCertificate,
    labels = c(
      'Oui' = 1,
      'Non' = 0
    ),
    label = "Enfants de moins de 5 ans munis d'un certificat de naissance"
  ))


ind <- ind %>%
  mutate(birthRegistered = case_when(
    REG04 == "0" | REG04 == "98" ~ 0,
    REG04 == "1" ~ 1,
    REG04 == "99" ~ NA_real_,  # Handle unknown/missing values
    TRUE ~ NA_real_  # Catch-all for any unexpected values
  )) %>%
  mutate(birthRegistered = labelled(
    birthRegistered,
    labels = c(
      'Oui' = 1,
      'Non' = 0
    ),
    label = "Enfants de moins de 5 ans enregistrés auprès des autorités civiles"
  ))

# Mutate outcome1_2 variable 
ind <- ind %>%
  mutate(outcome1_2 = case_when(
    (birthRegistered == 1 | birthCertificate == 1) & HH07 < 5 ~ 1,  # Child has been registered or has birth certificate
    (birthRegistered == 0 & birthCertificate == 0) & HH07 < 5 ~ 0,  # Child not registered and has no birth certificate
    TRUE ~ NA_real_  # Handle cases where data is missing or unknown
  )) %>%
  mutate(outcome1_2 = labelled(
    outcome1_2,
    labels = c(
      'Oui' = 1,
      'Non' = 0
    ),
    label = "Proportion d'enfants de moins de 5 ans dont la naissance a été enregistrée auprès d'une autorité civile"
  ))

##Table for srvyr
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>% #Ne concerne que les moins de 5 ans ...
  filter(HH07<5) %>% #Ne concerne que les moins de 5 ans ...
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE                # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
# agd_config <- NULL
agd_config <- "gender_age" #"disability_gender_age"
get_rmsTable(RMS_XXX_202X_ind,"outcome1_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome1_2",disability = FALSE)

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome1_2",indicator_name="Outcome 1.2",
                plt_subtitle="Proportion d'enfants de moins de 5 ans dont la naissance a été enregistrée auprès d'une autorité civile",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###1.3 Proportion of people with legally recognized identity documents or credentials ----
##Module :REG01 - REG02 - REG05 - REG06
#****************************************************************************************************************************************

##This indicator comes from the individual dataset
###Calculate valid identity documents for under 5 with REG05 and REG06 variables
#ind$REG05a - passport
#ind$REG05b - civil/government issued ID
#ind$REG05c - residency permit
#ind$REG05d - statelessness documentation
#ind$REG05e - household card of address/family book
#ind$REG05f - social security card
#ind$REG06 - any other document establishes identity
#add birth certificate as additional document from REG03

#ind$REG01a # passport
#ind$REG01b # birth certificate
#ind$REG01c # civil/ government issued ID
#ind$REG01d # residency permit
#ind$REG01e # statelessness documentation
#ind$REG01f # household card of address/family book
#ind$REG01g # social security card
#ind$REG02 # any other document establishes identity

#Make sure to delete REG05e below from the script if you don't have any stateless 
ind <- ind %>%
  mutate(document_under5 = case_when(
    #* [Ancien script]
    REG05a == "1" | REG05b == "1" | REG05c == "1" | REG05d == "1" | REG05e == "1" | REG05f == "1" | REG06 == "1" | REG03 == "1" ~ 1,
    REG05a != "1" & REG05b != "1" & REG05c != "1" & REG05d != "1" & REG05e != "1" & REG05f != "1" & REG06 != "1" & REG03 != "1" ~ 0,
    # #* [Ajustement à la demande du commanditaire]
    # REG05a == "1" | REG05b == "1" | REG05c == "1" | REG05d == "1" | REG05e == "1" | REG05f == "1" | REG03 == "1" ~ 1,
    # REG05a != "1" & REG05b != "1" & REG05c != "1" & REG05d != "1" & REG05e != "1" & REG05f != "1" & REG03 != "1" ~ 0,
    TRUE ~ NA_real_
  ))

# Mutate document_above5 using character values
ind <- ind %>%
  mutate(document_above5 = case_when(
    REG01a == "1" | REG01b == "1" | REG01c == "1" | REG01d == "1" | REG01e == "1" | REG01f == "1" | REG01g == "1" | REG02 == "1" ~ 1,
    REG01a != "1" & REG01b != "1" & REG01c != "1" & REG01d != "1" & REG01e != "1" & REG01f != "1" & REG01g != "1" & REG02 != "1" ~ 0,
    TRUE ~ NA_real_
  ))

# Combine both age groups
ind <- ind %>%
  mutate(outcome1_3 = case_when(
    (document_above5 == 1 | document_under5 == 1) ~ 1,  
    (document_above5 == 0 | document_under5 == 0) ~ 0
  )) %>%
  mutate(outcome1_3 = labelled(outcome1_3,
                               labels = c(
                                 'Oui' = 1,
                                 'Non' = 0
                               ),
                               label = "Proportion de personnes possédant des documents d'identité ou des titres légalement reconnus"))


RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE 
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_ind,"outcome1_3",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome1_3")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome1_3",indicator_name="Outcome 1.3",
                plt_subtitle="Proportion de personnes possédant des documents d'identité ou des titres légalement reconnus",
                agd = agd_config, plt_caption = "")
rm(agd_config)

###define labels
identity_documents <- c(
  "REG01a" = "Passeport",
  "REG01b" = "Acte de naissance",
  "REG01c" = "Carte d'identité civile/délivrée par le gouvernement",
  "REG01d" = "Permis de séjour",
  "REG01e" = "Documentation sur l'apatridie",
  "REG01f" = "Carte d'adresse du ménage/carnet de famille",
  "REG01g" = "Carte de sécurité sociale",
  "REG02" = "Tout autre document"
)

# Calculate the percentage of '1's for each identity document
identity_document_percentages <- RMS_XXX_202X_ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs)) %>%
  summarise(across(c(REG01a, REG01b, REG01c, REG01d, REG01e, REG01f, REG01g, REG02),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = everything(), 
               names_to = "Document", 
               values_to = "Percentage") %>%
  mutate(Document = identity_documents[Document])  # Map column names to descriptive labels

# Create the bar chart
outcome1_3_graph03 <- ggplot(identity_document_percentages %>% filter(!is.na(Percentage)), 
                             aes(x = reorder(Document, Percentage), y = Percentage, fill = Document)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),color = "orangered",, #, #orangered
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Pourcentage de personnes détenant des documents d'identité",
    x = "Document d'identité",
    y = "Pourcentage",
    caption = "Note : Les pourcentages sont calculés indépendamment pour chaque document."#pour les individus de 5 ans et plus
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

#****************************************************************************************************************************************
###4.1 Proportion of people who know where to access available GBV service ----
##Module :GBV01
#****************************************************************************************************************************************
#*
##indicator calculation
main <- main %>%
  mutate(outcome4_1 = case_when(
    GBV01a == "1" | GBV01b == "1" ~ 1,  # If GBV01a or GBV01b is "1", set to 1 (numeric)
    across(c(GBV01a, GBV01b, GBV01c, GBV01d), ~ . == "98") %>% rowSums() == 4 ~ NA_real_,  # If all selected columns are "98", set to NA_real_
    TRUE ~ 0  # For all other cases, set to 0 (numeric)
  )) %>%
  mutate(outcome4_1 = labelled(outcome4_1,
                               labels = c(
                                 'Oui' = 1,
                                 'Non' = 0
                               ),
                               label = "Proportion de personnes sachant où accéder à un service de lutte contre la violence liée au sexe"
  ))

######Table standard 
RMS_XXX_202X_main <- main %>% 
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  filter(ind_age>=18) %>% #* [Indicateur calculé uniquement pour les 18 ans et plus]
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE 
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome4_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome4_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome4_1",indicator_name="Outcome 4.1",
                plt_subtitle="Proportion de personnes sachant où accéder à un service de lutte contre la violence liée au sexe",
                agd = agd_config, plt_caption = "")
rm(agd_config)

####Simple bar chart of GBV01
###define labels 
gbv_services <- c(
  "GBV01a" = "Services de santé",
  "GBV01b" = "Services psychosociaux",
  "GBV01c" = "Services de sûreté et de sécurité",
  "GBV01d" = "Assistance juridique"#,
  # # A supprimer - Proprement au Tchad
  # "GBV01e" = "Association des femmes juristes du Tchad (AFJT)",
  # "GBV01f" = "Association de promotion des Libertés fondamentales au Tchad (APLFT)",
  # "GBV01g" = "Arrangements communautaires"
)

# Calculate the percentage of '1's for each identity document
gbv01_percentages <- RMS_XXX_202X_main %>%
  ungroup() %>% 
  summarise(across(c(GBV01a, GBV01b, GBV01c, GBV01d),#,GBV01e,GBV01f,GBV01g),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups="drop") %>% 
  pivot_longer(cols = everything(), 
               names_to = "Services", 
               values_to = "Percentage") %>%
  mutate(Services = gbv_services[Services])   # Map column names to descriptive labels

# Create the bar chart
outcome4_1_graph03 <- ggplot(gbv01_percentages, aes(x = reorder(Services, Percentage), y = Percentage, fill = Services)) +
  geom_bar(stat = "identity", width = 0.7) +  # Use "identity" for stat
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = "white", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels inside bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Connaissance des lieux d'accès aux services de lutte contre la violence liée au sexe",
    x = "Services de lutte contre la violence sexiste",
    y = "Pourcentage",
    caption = "Note : Les pourcentages sont calculés indépendamment pour chaque service pour les personnes de 18 ans et plus."
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust y-axis text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

##Table with education level for randomly selected adult
outcome4_1_EDU <- RMS_XXX_202X_main %>%
  filter(!is.na(EDU01_random) ) %>%  # Exclude EDU01_random
  group_by(EDU01_random) %>%
  summarise(
    var_name = "outcome4_1",                                      # Name of the variable
    num_obs_uw = survey_total(),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome4_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


#****************************************************************************************************************************************
###4.2 Proportion of people who do not accept violence against women ----
##Module :VAW01
#****************************************************************************************************************************************

##This indicator comes from main dataset based on the respondent randomly selected for individual level
#If randomly selected adult who believes that a  husband is justified in beating his wife in various circumstances
##If yes selected for any of the circumstances
###Prefer not to respond will be put into missing

main <- main %>%
  mutate(
    outcome4_2 = case_when(
      VAW01a == "1" | VAW01b == "1" | VAW01c == "1" | VAW01d == "1" | VAW01e == "1" ~ 0,  # Any "1" results in 0
      VAW01a == "0" & VAW01b == "0" & VAW01c == "0" & VAW01d == "0" & VAW01e == "0" ~ 1,  # All "0"s result in 1
      TRUE ~ NA_real_  # Missing or other cases result in NA
    )
  ) %>%
  mutate(
    outcome4_2 = labelled(
      outcome4_2,
      labels = c(
        'Oui' = 1,  # Numeric label for "Oui"
        'Non' = 0    # Numeric label for "Non"
      ),
      label = "Proportion de personnes qui n'acceptent pas la violence à l'égard des femmes"
    )
  )

######Table standard 
RMS_XXX_202X_main <- main %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  filter(ind_age>=18) %>% #* [Indicateur calculé uniquement pour les 18 ans et plus]
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome4_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome4_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome4_2",indicator_name="Outcome 4.2",
                plt_subtitle="Proportion de personnes qui n'acceptent pas la violence à l'égard des femmes",
                agd = agd_config, plt_caption = "")
rm(agd_config)

## Gender-based Violence
vaw_options <- c(
  VAW01a = "Si elle sort sans le lui dire",
  VAW01b = "Si elle néglige les enfants",
  VAW01c = "Si elle se dispute avec lui",
  VAW01d = "Si elle refuse d'avoir des relations sexuelles avec lui",
  VAW01e = "Si elle brûle la nourriture"
)

# Summarize the percentages for each question
vaw01_percentages <- RMS_XXX_202X_main %>% #main %>%
  ungroup() %>%
  summarise(across(c(VAW01a, VAW01b, VAW01c, VAW01d, VAW01e),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups="drop") %>% 
  pivot_longer(cols = everything(), 
               names_to = "Question", 
               values_to = "Percentage") %>%
  mutate(Question = vaw_options[Question])  # Map column names to descriptive labels

###Chart
outcome4_2_graph03 <- ggplot(vaw01_percentages, aes(x = reorder(Question, Percentage), y = Percentage, fill = Question)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = "white", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  labs(
    title = "Justification de la violence à l'égard des femmes",
    x = "Justification",
    y = "Pourcentage",
    caption = "Note : Les pourcentages représentent la proportion de personnes interrogées qui sont d'accord avec la justification."
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

#****************************************************************************************************************************************
###4.3 Proportion of survivors who are satisfied with GBV case management services -----
#' [Indicateur non défini dans le RMS]
#****************************************************************************************************************************************
# outcomeIndicators_43 <- function(){
#   
#   indicator_name<- "4.3 Proportion de PoC qui sont satisfaits des services de gestion des cas de VBG"
#   indicator_type <- "Core outcome indicator"
#   indicator_target <- "100%"
#   popcible_idx <- c(1:5)
#   popcible <- paste(target_population[popcible_idx],collapse = ", ")
#   
#   # c("poc","1-refugee","2-host","3-asylum_seeker","4-returnee","5-idp")
#   # c("réfugiés","hôtes","demandeurs d'asile","retournés","personnes déplacées internes")
#   
#   dbase <- members %>% 
#     filter(q0304 %in% popcible_idx,c1202==1 | c1203==1 | d1205==1)
#   
#   # Calcul de l'indicateur dans l'ensemble...
#   denominateur <-  dbase %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   numerateur <- dbase %>% 
#     mutate(valid_answer=as.numeric(q1207==1)) %>% 
#     filter(valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   result <- 100*numerateur/denominateur
#   
#   denom_man <- dbase %>% filter(q0306==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   denom_woman <- dbase %>% filter(q0306==2) %>% 
#     summarize(total=n()) %>% as.numeric()
#   #pwd=people with disabilities
#   denom_pwd <- dbase %>% filter(q0310_6==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   #pwd=people without disabilities
#   denom_pwod <- dbase %>% filter(q0310_6==0) %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   #....................................................................
#   # Desagregation des resultats par genre et handicap
#   #...................................................................
#   
#   dbtmp <- dbase %>% 
#     mutate(valid_answer=as.numeric(q1207==1))
#   
#   num_man <- dbtmp %>% 
#     filter(q0306==1,valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   num_woman <- dbtmp %>% 
#     filter(q0306==2,valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   num_pwd <- dbtmp %>% 
#     filter(q0310_6==1,valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   num_pwod <- dbtmp %>% 
#     filter(q0310_6==0,valid_answer==1) %>% 
#     summarize(total=n()) %>% as.numeric()
#   
#   res_man <- 100*num_man/denom_man
#   res_woman <- 100*num_woman/denom_woman
#   res_pwd <- 100*num_pwd/denom_pwd
#   res_pwod <- 100*num_pwod/denom_pwod
#   
#   msg <- sprintf("[%s] %s",
#                  ifelse(result<=40,"Critical",ifelse(result>=71,"Acceptable","Unacceptable")),
#                  sprintf("Femme = %.1f%%; Homme = %.1f%%; Personnes handicapées = %.1f%%; Personnes sans handicap = %.1f%%.",
#                          res_woman,res_man,res_pwd,res_pwod)) 
#   
#   infos <- tibble(
#     indicator=indicator_name,
#     type=indicator_type,
#     population=popcible,
#     target=indicator_target,
#     value=sprintf("%.1f%%",result),
#     source=src_result,
#     comment=msg
#   )
#   return(infos) 
# }


#****************************************************************************************************************************************
###5.2 Proportion of children who participate in community-based child protection programmes ----
##Module :COMM01-COMM04
#****************************************************************************************************************************************

## Child Protection
ind$COMM01 <- labelled_chr2dbl(ind$COMM01)
ind$COMM02 <- labelled_chr2dbl(ind$COMM02)
ind$COMM03 <- labelled_chr2dbl(ind$COMM03)
ind$COMM04 <- labelled_chr2dbl(ind$COMM04)


###This indicator comes from the individual level dataset
#Children who participate in community-based programmes at least once 
##under adult supervision in a physically safe area
ind <- ind %>%
  mutate(outcome5_2 = case_when(
    (COMM01 == 1 & (COMM02 >= 1 & COMM02 != 98) & COMM03 == 1 & COMM04 == 1) ~ 1,  # All conditions are character comparisons
    (COMM01 == 0 | 
       (COMM02 == 0 | COMM02 == 98) | 
       (COMM03 == 0 | COMM03 == 98) |
       (COMM04 == 0 | COMM04 == 98)) ~ 0,  # Comparison for "0" and "98" as characters
    TRUE ~ NA_real_  # Catch-all for any missing data
  )) %>%
  mutate(outcome5_2 = labelled(outcome5_2,
                               labels = c(
                                 'Oui' = 1,  # Label for "Oui"
                                 'Non' = 0    # Label for "Non"
                               ),
                               label = "Proportion of children who participate in community-based child protection programmes"
  )) #%>% 
  # mutate(
  #   age_cb_child_protection = case_when(
  #     # !is.na(HH07_cat) & HH07 < 18 ~ 1,
  #     HH07 >= 4 & HH07 <= 17 ~ 1,
  #     TRUE ~ 0
  #   )
  # )

###Table 
RMS_XXX_202X_ind <- ind %>%
  filter(HH07>=5 & HH07 <= 17) %>% # Indicateur calculé uniquement pour les enfants de 5-17 ans
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender"
get_rmsTable(RMS_XXX_202X_ind,"outcome5_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome5_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome5_2",indicator_name="Outcome 5.2",
                plt_subtitle="Proportion d'enfants participant à des programmes communautaires de protection de l'enfance",
                agd = agd_config, plt_caption = "Enfants de 5 à 17 ans.")
rm(agd_config)

##Table with disability and gender
outcome5_2_AGD_tmp <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(pop_groups) & HH07 < 18) %>%  # Missing pipe added here
  group_by(HH04, pop_groups, disability) %>%
  summarise(
    var_name = "outcome5_2",                                      # Name of the variable
    # num_obs_uw = survey_total(!is.na(outcome5_2), vartype = NULL),  # Unweighted total count
    num_obs_uw = unweighted(n()),
    denominator = survey_total(),                                  # Weighted total count
    mean_value = survey_mean(outcome5_2, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )

##Chart with pop groups
gender_colors <- c("Homme" = "#8395B9", "Femme" = "#E0E9FE")
outcome5_2_graph03 <- ggplot(outcome5_2_AGD_tmp, aes(x = HH04, y = mean_value, fill = disability)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)),
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +
  scale_fill_unhcr_d() +  # UNHCR color palette
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 5.2 par groupe de population et par sexe",
    subtitle = "Proportion d'enfants participant à des programmes communautaires de protection de l'enfance",
    x = "Sexe",
    y = "Proportion d'enfants",
    caption = paste(unhcr_caption,"Note : Seulement les enfants de 5 à 17 ans",sep = "; ")
  ) +
  # scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


###7.3 Proportion of women participating in leadership/management structures -----
#'  [Indicateur non définie dans le questionnaire standard du RMS]
#' main <- main %>%
#'   rowwise() %>% 
#'   mutate(
#'     unhcr_condition = sum(c(as.numeric(BQ903=="1"), as.numeric(BQ904=="1"), as.numeric(BQ905=="1"),
#'       as.numeric(BQ906=="1"),as.numeric(BQ907=="1"),as.numeric(BQ908=="1")),na.rm=TRUE),
#'     outcome7_3 = case_when(
#'       # All conditions are character comparisons
#'       HH04=="Femme" & HH07>= 18 & unhcr_condition>=6 ~ 1,  
#'       HH04=="Homme" & HH07>= 18 & unhcr_condition>=6 ~ 0,  
#'       TRUE ~ NA_real_  # Catch-all for any missing data
#'     )
#'   ) %>%
#'   select(-unhcr_condition) %>% ungroup() %>% 
#'   mutate(outcome7_3 = labelled(outcome7_3,
#'                                labels = c(
#'                                  'Oui' = 1,  # Label for "Oui"
#'                                  'Non' = 0    # Label for "Non"
#'                                ),
#'                                label = "Proportion de femmes participant aux structures de direction/gestion"
#'   ))
#' 
#' # # Check data 
#' # table(main$outcome7_3,useNA = "ifany")
#' 
#' ###Table 
#' RMS_XXX_202X_main <- main %>%
#'   filter(
#'     # pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED),
#'     !is.na(outcome7_3)
#'   ) %>% 
#'   as_survey_design(
#'     ids = cluster_id,           # Specify the column with cluster IDs
#'     # inv_poids_reel,inv_poids_ajuste_reel
#'     weights = UNHCR_WEIGHT, # Specify the column with survey weights
#'     nest = FALSE              # Use TRUE if PSUs are nested within clusters
#'   )
#' 
#' #' [Generation des tables standards]
#' agd_config <- "disability_age"
#' get_rmsTable(RMS_XXX_202X_main,"outcome7_3",agd = agd_config)
#' get_rmsTable_AGD(RMS_XXX_202X_main,"outcome7_3")
#' 
#' #' [Generation des graphiques standards]
#' get_rmsGraphics(indicator="outcome7_3",indicator_name="Outcome 7.3",
#'                 plt_subtitle="Proportion de femmes participant aux structures de direction/gestion",
#'                 agd = agd_config, plt_caption = "")
#' rm(agd_config)
#' 
#' # Engagement et participation communautaires
#' bq900_mapping <- c(
#'   "BQ903" = "Les femmes ont accès aux informations sur les structures de direction", #leadership and management 
#'   "BQ904" = "Organisation de réunions de dirigeants pour encourager la participation des femmes",
#'   "BQ905" = "Les femmes sont impliquées dans les structures de direction",
#'   "BQ906" = "Les femmes peuvent s'exprimer librement dans les structures de direction",
#'   "BQ907" = "Les femmes impliquées dans la prise de décision",
#'   "BQ908" = "Les femmes ont réussi à mettre des questions à l'ordre du jour au sein des structures de direction." 
#' )
#' 
#' # Step 2: Calculate the percentage of individuals receiving each service
#' bq900_percentages <- main %>%
#'   filter(
#'     # pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED),
#'     HH07>= 18
#'     # !is.na(outcome7_3)
#'   ) %>%
#'   group_by(pop_groups) %>%  # Group by population group (e.g., gender, age group)
#'   summarise(across(c(BQ903, BQ904, BQ905, BQ906, BQ907, BQ908), 
#'                    # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
#'                    ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
#'            .groups="drop") %>% 
#'   pivot_longer(cols = -pop_groups,  # Exclude the PopGroup column from pivoting
#'                names_to = "community_involvement", 
#'                values_to = "Percentage") %>%
#'   mutate(community_involvement = bq900_mapping[community_involvement])  # Map column names to descriptive labels
#' 
#' 
#' # Step 3: Create the bar chart
#' outcome7_3_graph03 <- ggplot(bq900_percentages, aes(x = reorder(community_involvement, Percentage), 
#'                                                      y = Percentage, fill = community_involvement)) +
#'   geom_bar(stat = "identity", width = 0.7, color = "white") +
#'   # Add percentage labels on bars
#'   geom_text(aes(label = sprintf("%.1f%%", Percentage),colour = 'orangered', ),
#'             position = position_stack(vjust = 0.5), size = 3.5) +
#'   coord_flip() +  # Flip the axes for better readability
#'   # Labels and title
#'   labs(
#'     title = "Proportion de femmes par engagement communautaire",
#'     x = "Participation communautaire",
#'     y = "Pourcentage",
#'     caption = "Note : Chaque participation communautaire est calculée indépendamment."
#'   ) +
#'   scale_fill_unhcr_d() +  # Use UNHCR color palette
#'   theme_unhcr() +  # Apply UNHCR theme
#'   # Customize theme elements
#'   theme(
#'     axis.text.y = element_text(size = 9),  # Adjust text size for readability
#'     legend.position = "none"  # Remove the legend for simplicity
#'   ) +
#'   # Separate plots for each population group (e.g., gender, age group)
#'   facet_wrap(~pop_groups, scales = "free_y", ncol = 1)  # Create separate plots per PopGroup

#****************************************************************************************************************************************
###8.2 Proportion of people with primary reliance on clean (cooking) fuels and technology -----
##Module :COOK01-COOK03
#****************************************************************************************************************************************

###indicator calculation
main$COOK01 <- labelled_chr2dbl(main$COOK01)
main$COOK02 <- labelled_chr2dbl(main$COOK02)
main$COOK03 <- labelled_chr2dbl(main$COOK03)


###Based on MICS calculation : TC4.1
main <- main %>%
  mutate(
    outcome8_2 = case_when(
      (COOK01 == 1 & (COOK02 %in% c(1, 2, 3, 4, 5)) | (COOK02 %in% c(10) & COOK03 %in% c(1))
      ) ~ 1,  # First condition for clean cooking
      (COOK01 == 1 & (COOK02 %in% c(7, 8, 9, 10, 96)) | (COOK02 %in% c(10) & !(COOK03 %in% c(1))
      )) ~ 0,  # Condition for unclean cooking or incomplete clean technology
      COOK01 == 0 ~ 0,  # COOK01 is "0"
      TRUE ~ NA_real_  # Handle missing or other cases
    )
  ) %>%
  mutate(
    outcome8_2 = labelled(outcome8_2,
                          labels = c(
                            "Non" = 0,
                            "Oui" = 1
                          ),
                          label = "Proportion of people with primary reliance on clean (cooking) fuels and technology"
    )
  )


##Table by population groups
RMS_XXX_202X_main <- main %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (            # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome8_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome8_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome8_2",indicator_name="Outcome 8.2",
                plt_subtitle="Proportion de personnes utilisant principalement des combustibles et des technologies propres",
                agd = agd_config, plt_caption = "")
rm(agd_config)

###Show the bar chart for COOK02
table(main$COOK02)

# Define stove categories based on the provided list
stove_labels <- c(
  "1" = "Cuiseur solaire (énergie thermique du soleil)",
  "2" = "Cuisinière électrique",
  "3" = "Cuisinière à gaz naturel",
  "4" = "Four à biogaz",
  "5" = "Réchaud à gaz de pétrole liquéfié (GPL)/gaz de cuisine",
  "6" = "Poêle à combustible solide fabriqué",
  "7" = "Fourneau traditionnel à combustible solide (non fabriqué)",
  "8" = "Bac à feu mobile",
  "9" = "Trois poêles en pierre/feu ouvert",
  "10" = "Réchaud à combustible liquide",
  "96" = "Autre, précisez"
)

# Summarize the counts and percentages for each category
cook02_percentages <- main %>%
  ungroup() %>% 
  filter(!is.na(COOK02)) %>%  # Exclude missing values
  count(COOK02, wt=UNHCR_WEIGHT) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(COOK02 = factor(COOK02, levels = names(stove_labels), labels = stove_labels))

# RMS_XXX_202X_main %>%  
#   mutate(
#     unhcr_var = labelled::to_factor(COOK02)
#   ) %>% 
#   filter(!is.na(unhcr_var)) %>%
#   gtsummary::tbl_svysummary(include=c(unhcr_var))

# Create the chart
outcome8_2_graph03 <- ggplot(cook02_percentages, aes(x = reorder(COOK02, Percentage), y = Percentage, fill = COOK02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Répartition des types de fourneaux",
    x = "Type de fourneau",
    y = "Pourcentage",
    caption = unhcr_caption #"Source: RMS Cameroon 202"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

#****************************************************************************************************************************************
###9.1 Proportion of people living in habitable and affordable housing -----
##Module :DWE01 – SHEL01-SHEL06 – DWE05 – DWE08-DWE09
#****************************************************************************************************************************************

###Indicator calculation
##Module :DWE01 – SHEL01-SHEL06 – DWE05 – DWE08-DWE09
##This indicator is calculated from the main dataset

##Condition 1
##Classify as habitable for below conditions - if 98 selected, put into missing
##First check the variables
table(main$SHEL01)
table(main$SHEL02)
table(main$SHEL03)
table(main$SHEL04)
table(main$SHEL05)
table(main$SHEL06)

main <- main %>%
  mutate(across(starts_with("SHEL"), ~if_else(. == "98", NA, .))) %>%
  mutate(habitablehousing = case_when(
    (SHEL01 == "1") & (SHEL02 == "1") & (SHEL05 == "1") & 
      (SHEL03 == "0" ) & (SHEL04 == "0" ) & (SHEL06 == "0" ) ~ 1,
    (SHEL01 == "0") | (SHEL02 == "0" ) | (SHEL05 == "0") |
      (SHEL03 == "1") | (SHEL04 == "1") | (SHEL06 == "1" ) ~ 0,
    TRUE ~ NA_integer_
  ))

table(main$habitablehousing)

##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus not considered as physically safe
table(main$hh_size_001)
table(main$DWE05)

main <- main %>%
  mutate(crowding=hh_size_001/DWE05
  ) %>%
  mutate(dwe05_cat=case_when( ##if crowding <= 3, not overcrowded 
    crowding <= 3 ~ 1, TRUE ~ 0)
  )

table(main$crowding)
table(main$dwe05_cat)

##Condition 3
main <- main %>%
  mutate(dwe09_cat = case_when( 
    # Affordable if household pays rent and without financial distress
    (DWE08 == "1" & (DWE09 == "1" | DWE09 == "2")) ~ 1,  # No financial distress
    (DWE08 == "1" & (DWE09 == "3" | DWE09 == "4")) ~ 0,  # Financial distress
    DWE08 == "0" ~ 1,  # If not paying rent, it's considered affordable (set to 1)
    TRUE ~ NA_real_  # Catch-all for missing or unexpected values
  ))

table(main$dwe09_cat)

###Combine all three conditions for habitable housing
main <- main %>%
  mutate(
    outcome9_1 = case_when(
      dwe05_cat == 1 & habitablehousing == 1 & dwe09_cat == 1  ~ 1,
      TRUE ~ 0
    ),
    outcome9_1 = labelled(outcome9_1,
                          labels = c("Oui" = 1, "Non" = 0),
                          label = "Proportion of people living in habitable and affordable housing")
  )

####Standard tables 
composite_outcome9_1 <- main %>%
  select(pop_groups, dwe05_cat, habitablehousing, dwe09_cat,UNHCR_WEIGHT) %>%
  pivot_longer(cols = c(dwe05_cat, habitablehousing, dwe09_cat),  # Pivot the three variables
               names_to = "facility", 
               values_to = "access") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = weighted.mean(access, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,.groups = "drop")

###Chart for above with all dimensions 
outcome9_1_graph03 <- ggplot(composite_outcome9_1, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "dwe05_cat" = "Logement non surpeuplé",
    "habitablehousing" = "Logement en bon état",
    "dwe09_cat" = "Logement abordable"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Accès aux équipements de logement par groupe de population",
    x = "",#"Facilité",
    y = "Pourcentage accès",
    fill = "Groupes de population",
    caption = unhcr_caption
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    axis.text.x = element_text(hjust = 0.5, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  )

##Table by population groups
RMS_XXX_202X_main <- main %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (             # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome9_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome9_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome9_1",indicator_name="Outcome 9.1",
                plt_subtitle="Proportion de personnes vivant dans un logement habitable et abordable",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###9.2 Proportion of people that have energy to ensure lighting -----
##Module :LIGHT01-LIGHT03
#****************************************************************************************************************************************

###This basic service is calculated from the main dataset
### The below Calculates percentage of PoC having access to clean fuel for lighting and / or basic connectivity (9.1 Outcome Indicator)
main <- main %>%
  mutate(outcome9_2 = case_when(
    # LIGHT01 == "1" & LIGHT02 %in% c("1", "2", "3", "4", "5", "6", "7") ~ 1,  # Conditions for "Oui" (1)
    # Dans le cas du RMS NIGER - la revision est de considérer uniquement l'energie électrique et solaire
    LIGHT01 == "1" & LIGHT02 %in% c("1", "2") ~ 1,  # Conditions for "Oui" (1)
    TRUE ~ 0  # Default to "Non" (0)
  )) %>%
  mutate(outcome9_2 = labelled(outcome9_2,
                               labels = c(
                                 "Oui" = 1,
                                 "Non" = 0
                               ),
                               label = "Proportion of people that have energy to ensure lighting"
  ))

##Table by population groups
RMS_XXX_202X_main <- main %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (          # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome9_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome9_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome9_2",indicator_name="Outcome 9.2",
                plt_subtitle="Proportion de personnes disposant d'énergie pour assurer l'éclairage",
                agd = agd_config, plt_caption = "")
rm(agd_config)

###Show the bar chart for LIGHT02
table(main$LIGHT02)

# Define stove categories based on the provided list
lighting_labels <- c(
  "1" =	"Electricité",
  "2" =	"Système solaire domestique",
  "3"	= "Lanterne ou lampe de poche à énergie solaire",
  "4" = "Lampe de poche rechargeable, mobile, torche ou lanterne",
  "5" = "Lampe de poche, torche ou lanterne à piles",
  "6" =	"Lampe à biogaz",
  "7" =	"Lampe GPL",
  "8" = "Lampe à essence",
  "9"	= "Lampe à pétrole ou à paraffine",
  "10" = "Lampe à huile",
  "11" = "Bougie",
  "12" ="Feu ouvert",
  "96" ="Autre, précisez"
)

# Summarize the counts and percentages for each category
light02_percentages <- main %>%
  ungroup() %>% 
  filter(!is.na(LIGHT02)) %>%  # Exclude missing values
  count(LIGHT02, wt=UNHCR_WEIGHT) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(LIGHT02 = factor(LIGHT02, levels = names(lighting_labels), labels = lighting_labels))

# Create the chart
outcome9_2_graph03 <- ggplot(light02_percentages, aes(x = reorder(LIGHT02, Percentage), y = Percentage, fill = LIGHT02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5,color="orangered") +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Distribution de l'énergie d'éclairage (LIGHT02)",
    x = "Type d'éclairage",
    y = "Pourcentage",
    caption = unhcr_caption#"Source: RMS XXX 202X"
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

#****************************************************************************************************************************************
###10.1 Proportion of children aged 9 months to five years who have received measles vaccination ----
##Module :MMR01-MMR04 ##  MICS TC.1.1 UNICEF calculates on the first dose received##
#****************************************************************************************************************************************
ind <- ind %>%
  mutate(outcome10_1=case_when(
    MMR03=="1" ~ 1, MMR03=="0"  | MMR03=="98" ~ 0)
  ) %>%
  mutate( outcome10_1 = labelled(outcome10_1,
                                 labels = c(
                                   "Oui" = 1,
                                   "Non" = 0
                                 ),
                                 label = "Proportion of children aged 9 months to five years who have received measles vaccination"))

###Table 
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES)) %>%
  # filter(ind_age_month>9 | ind_age_year<5) %>% 
  filter(HH07_months>9 & HH07<5) %>% 
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (             # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "gender_age" #"disability_gender"
get_rmsTable(RMS_XXX_202X_ind,"outcome10_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome10_1",disability = FALSE)

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome10_1",indicator_name="Outcome 10.1",
                plt_subtitle="Proportion d'enfants âgés de 9 mois à 5 ans ayant été vaccinés contre la rougeole",
                agd = agd_config, plt_caption = "")
rm(agd_config)

##Table with disability and gender
outcome10_1_AGD_tmp <- RMS_XXX_202X_ind %>%
  filter(!is.na(HH04) & !is.na(pop_groups) & (HH07_months>9 & HH07<5) ) %>%  # Exclude HH07_cat categories 1, 2, and 5
  group_by(HH07_cat, HH04, pop_groups) %>%
  summarise(
    var_name = "outcome10_1",                                      # Name of the variable
    num_obs_uw = survey_total(!is.na(outcome10_1), vartype = NULL),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome10_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  )


##Chart with pop groups
gender_colors <- c("Homme" = "#8395B9", "Femme" = "#E0E9FE")
outcome10_1_graph03 <- ggplot(outcome10_1_AGD_tmp, aes(x = HH04, y = mean_value, fill = HH04)) +  # Fill mapped to HH04 for gender
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", mean_value)), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3) +  # Add values on bars
  facet_wrap(~ pop_groups) +  # Create separate plots for each population group
  labs(
    title = "Outcome 10.1 by Population Groups and Gender",
    x = "Sexe",
    y = "Proportion d'enfants",
    caption = paste(unhcr_caption,"Remarque : seuls les enfants de moins de 5 ans",sep = " ")
  ) +
  scale_fill_manual(values = gender_colors) +  # Apply custom colors for male and female
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )

#****************************************************************************************************************************************
###10.2 Proportion of births attended by skilled health personnel ----
##Module :BIR01-BIR04
#****************************************************************************************************************************************

###indicator calculation
main <- main %>%
  mutate(outcome10_2 = case_when( 
    (BIR01 == "1" | BIR02 == "1") & (BIR03 %in% c("1", "2", "3")) ~ 1,  # Skilled personnel attended
    (BIR01 == "1" | BIR02 == "1") & (BIR03 %in% c("4", "5", "6")) ~ 0,  # Unskilled personnel attended
    BIR03 %in% c("96", "98") ~ NA_real_,  # Missing or not applicable
    TRUE ~ NA_real_  # Catch-all for any other cases
  )) %>%
  mutate(outcome10_2 = labelled(outcome10_2,
                                labels = c(
                                  "Oui" = 1,  # Yes for skilled personnel
                                  "Non" = 0    # No for unskilled personnel
                                ),
                                label = "Proportion of births attended by skilled health personnel"
  ))


###Table 
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_HOST)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE 
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome10_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome10_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome10_2",indicator_name="Outcome 10.2",
                plt_subtitle="Proportion d'accouchements assistés par du personnel de santé qualifié",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###11.1 Proportion of young people enrolled in tertiary and higher education  ----
#' [Indicateur non défini dans le RMS standard] ... 
#****************************************************************************************************************************************
ind <- ind %>% 
  mutate(
    edu_tertiary = case_when(
      EDU01 == "1" & EDU02 == "1" & (EDU03 == "5" | EDU03 == "6") ~ 1,  
      EDU01 == "0" | EDU02 == "0" ~ 0,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    age_tertiary = case_when(
      HH07 >= 19 & HH07 <= 23  ~ 1,  ###ADJUST AGE GROUPS FOR PRIMARY LEVEL ( 6- 11 in this ex)
      TRUE ~ 0
    )
  )

###Results of the indicator table
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%  
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE             # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender"
get_rmsTable(RMS_XXX_202X_ind,"outcome11_1",agd = agd_config,get_ratio = TRUE,
             var_numerator = "edu_tertiary",var_denominator = "age_tertiary")

get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome11_1",
                 disability=TRUE, gender=TRUE, age = FALSE, get_ratio = TRUE,
                 var_numerator = "edu_tertiary",var_denominator = "age_tertiary")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome11_1",indicator_name="Outcome 11.1",
                plt_subtitle="Proportion de jeunes inscrits dans l'enseignement tertiaire et supérieur",
                agd = agd_config,plt_caption = "Tranche d'âge pour l'enseignement supérieur : 19-23 ans.")
rm(agd_config)

#****************************************************************************************************************************************
###11.2 Proportion of children and young people enrolled in the national education system  ----
#' [Indicateur non défini dans questionnaire standard du RMS]
#****************************************************************************************************************************************
  ind <- ind %>% 
    mutate(
      edu_nes = case_when(
        EDU01 == "1" & EDU02 == "1" & (!is.na(EDU03) & EDU03 != "98") ~ 1,  
        EDU01 == "0" | EDU02 == "0" ~ 0,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      age_nes = case_when(
        HH07 >= 6 & HH07 <= 18  ~ 1,
        TRUE ~ 0
      )
    )

###Results of the indicator table
RMS_XXX_202X_ind <- ind %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%  
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE             # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender"
get_rmsTable(RMS_XXX_202X_ind,"outcome11_2",agd = agd_config,get_ratio = TRUE,
             var_numerator = "edu_nes",var_denominator = "age_nes")

get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome11_2",
                 disability=TRUE, gender=TRUE, age = FALSE, get_ratio = TRUE,
                 var_numerator = "edu_nes",var_denominator = "age_nes")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome11_2",indicator_name="Outcome 11.2",
                plt_subtitle="Proportion d'enfants et de jeunes inscrits dans le système éducatif national",
                agd = agd_config,plt_caption = "Groupe d'âge pour le système d'éducation nationale : 6-18 ans.")
rm(agd_config)

#****************************************************************************************************************************************
###12.1 Proportion of people using at least basic drinking water services ----
##Module :DWA01-DWA03
#****************************************************************************************************************************************

## Indicator calculation
main <- main %>%
  mutate(time_DWA = case_when(
    DWA03a == "1" ~ 1,  # If it's in hours, convert to 1 minute
    DWA03a == "2" ~ 60  # Convert 1 hour to 60 minutes
  )) %>%
  mutate(time_tot = time_DWA * as.numeric(DWA03b)  # Convert DWA03b to numeric if it's not already
  ) %>%
  mutate(dwa_cond1 = case_when(
    time_tot > 30 ~ 0,  # Not reachable under 30 minutes
    TRUE ~ 1  # Reachable under 30 minutes
  )) %>%
  mutate(dwa_cond2 = case_when(
    !DWA01 %in% c("7", "9", "13", "96", "98") ~ 1,  # Improved source (all except these codes)
    TRUE ~ 0
  )) %>%
  mutate(dwa_cond3 = case_when(
    DWA02 == "3" ~ 0,  # Not in the dwelling/yard/plot
    TRUE ~ 1  # In the dwelling/yard/plot
  )) %>%
  mutate(outcome12_1 = case_when(
    ((dwa_cond1 == 1 | dwa_cond3 == 1) & dwa_cond2 == 1) ~ 1,  # Basic drinking water service
    TRUE ~ 0
  )) %>%
  mutate(outcome12_1 = labelled(outcome12_1,
                                labels = c(
                                  "Oui" = 1,
                                  "Non" = 0
                                ),
                                label = "Proportion of people using at least basic drinking water services"
  ))

####Standard tables 
composite_outcome12_1 <- main %>%
  select(pop_groups, dwa_cond1, dwa_cond2, dwa_cond3,UNHCR_WEIGHT) %>%
  pivot_longer(cols = c(dwa_cond1, dwa_cond2, dwa_cond3),  # Pivot the three variables
               names_to = "facility", 
               values_to = "conditions") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = weighted.mean(conditions,wt=UNHCR_WEIGHT,na.rm = TRUE) * 100,.groups = 'drop') %>%
  ungroup()

###Chart for above with all dimensions 
outcome12_1_graph03 <- ggplot(composite_outcome12_1, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "dwa_cond1" = "Accessible en moins de 30 minutes",
    "dwa_cond2" = "A partir d'une source améliorée",
    "dwa_cond3" = "Dans l'habitation/la cour/le terrain"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Accès aux services de base d'eau potable",
    x = "", #"Au moins un des services d'eau potable de base",
    y = "Accès en pourcentage",
    fill = "Groupes de population",
    caption = unhcr_caption
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    axis.text.x = element_text(hjust = 0.5, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  )

##Table by population groups
RMS_XXX_202X_main <- main %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE           # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome12_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome12_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome12_1",indicator_name="Outcome 12.1",
                plt_subtitle="Proportion de la population utilisant au moins les services de base en matière d'eau potable",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###12.2 Proportion of people with access to a safe household toilet -----
##Module :TOI01-TOI05 ##MICS calculation WS3.1/WS3.4
#****************************************************************************************************************************************

##This indicator measures the proportion of people with access to at 
##least basic sanitation services.
##MICS calculation WS3.1/WS3.4

###Indicator calculations
main <- main %>%
  mutate(toi_cond1 = case_when(
    TOI01 %in% c("1", "2", "3", "4", "5", "6", "7", "9") ~ 1,  # Improved sanitation facility
    TOI01 %in% c("8", "10", "11", "12", "96") ~ 0,  # Unimproved sanitation facility
    TRUE ~ NA_real_  # Missing or invalid
  )) %>%
  mutate(toi_cond2 = case_when(
    TOI02 == "1" & TOI03 %in% c("5", "96", "98") ~ 0,  # Unsafe disposal
    TOI02 == "1" & TOI03 %in% c("1", "2", "3", "4") ~ 1,  # Safe disposal
    TOI02 == "2" ~ 0,  # Unsafe if no toilet
    TOI02 == "98" ~ 0,  # Unavailable information
    TRUE ~ NA_real_  # Missing or invalid
  )) %>%
  mutate(toi_cond3 = case_when(
    TOI05 == "1" ~ 0,  # Shared toilet
    TOI05 == "0" ~ 1   # Not shared
  )) %>%
  
  ### Combine all three conditions
  mutate(outcome12_2 = case_when(
    toi_cond1 == 1 & toi_cond2 == 1 & toi_cond3 == 1 ~ 1,  # Basic sanitation service
    TRUE ~ 0  # If any condition fails
  )) %>%
  mutate(outcome12_2 = labelled(outcome12_2,
                                labels = c(
                                  "Oui" = 1,
                                  "Non" = 0
                                ),
                                label = "Proportion of people with access to at least basic sanitation services."
  ))



####Standard tables 
composite_outcome12_2 <- main %>%
  select(pop_groups, toi_cond1, toi_cond2, toi_cond3, UNHCR_WEIGHT) %>%
  pivot_longer(cols = c(toi_cond1, toi_cond2, toi_cond3),  # Pivot the three variables
               names_to = "facility", 
               values_to = "conditions") %>%
  group_by(pop_groups, facility) %>%
  summarise(percentage = weighted.mean(conditions, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,.groups = "drop") %>%
  ungroup()


###Chart for above with all dimensions 
outcome12_2_graph03 <- ggplot(composite_outcome12_2, aes(x = facility, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "toi_cond1" = "Installation sanitaire améliorée",
    "toi_cond2" = "Élimination sûre des excréments\nsur place",
    "toi_cond3" = "Toilettes non partagées avec\nd'autres ménages"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Accès aux services de base d’assainissement et d'hygiène",
    x = "",#"Toilettes améliorées",
    y = "Accès en pourcentage",
    fill = "Groupes de population",
    caption = unhcr_caption
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    axis.text.x = element_text(hjust = 0.5, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  ) + 
  scale_y_continuous(limits=c(0,100), expand = c(0,0)) ### limit if needed

##Table by population groups
RMS_XXX_202X_main <- main %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE     
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome12_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome12_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome12_2",indicator_name="Outcome 12.2",
                plt_subtitle="Proportion de personnes ayant accès à des toilettes domestiques sûres",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###13.1 Proportion of people with an account at a bank or other financial institution or with a mobile-money-service provider ----
##This indicator comes from main dataset based on the respondent randomly selected for individual level
#****************************************************************************************************************************************

main <- main %>%
  mutate(
    outcome13_1 = case_when(
      BANK01 == "1" | BANK02 == "1" | BANK03 == "1" | BANK05 == "1" ~ 1,  # If any is "1", set to 1
      BANK01 == "0" & BANK02 == "0" & BANK03 == "0" & BANK05 == "0" ~ 0,  # If all are "0", set to 0
      TRUE ~ 0  # Default to 0 for all other cases
    )
  ) %>%
  mutate(outcome13_1 = labelled(outcome13_1,
                                labels = c(
                                  "Oui" = 1,  # Label for Yes
                                  "Non" = 0    # Label for No
                                ),
                                label = "Proportion of people with an account at a bank or other financial institution or with a mobile-money-service provider"
  ))

###Show all options separately
composite_outcome13_1 <- main %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>% #Population cible core indicator metadata
  filter(HH07 >= 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  select(pop_groups, BANK01, BANK02,  BANK04, BANK05, UNHCR_WEIGHT) %>%
  pivot_longer(cols = c(BANK01, BANK02, BANK04, BANK05),  # Pivot the three variables
               names_to = "access", 
               values_to = "conditions") %>%
  # ungroup() %>% select(-c(cluster_id,cluster_name)) %>%
  mutate(conditions=labelled_chr2dbl(conditions)) %>% 
  group_by(pop_groups, access) %>%
  summarise(percentage = weighted.mean(conditions, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,.groups = "drop")

###Chart for above with all dimensions 
outcome13_1_graph03 <- ggplot(composite_outcome13_1, aes(x = access, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Correct stat to "identity"
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "BANK01" = "Dispose d'un compte dans une\ninstitution financière",
    "BANK02" = "Dispose d'une carte bancaire\nou d'une carte de débit",
    "BANK04" = "A utilisé l'argent mobile\nau cours des 12 derniers mois",
    "BANK05" = "A utilisé personnellement\nl'argent mobile au cours des\n12 derniers mois"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Pourcentage de personnes financièrement intégrées",
    x = "",#"Inclusion financière",
    y = "Pourcentage",
    fill = "Groupes de population",
    caption = unhcr_caption
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels for readability
    axis.text.x = element_text(hjust = 0.5, size = 10),  # Rotate x-axis labels for readability
    strip.text = element_text(size = 10)  # Adjust label size
  ) #+
  # scale_y_continuous(limits = c(0, 50), expand = c(0, 0))  # Limit y-axis from 0 to 100%


###Table standard 
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
    filter(HH07 >= 18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE  
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome13_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome13_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome13_1",indicator_name="Outcome 13.1",
                plt_subtitle="Proportion de personnes ayant un compte dans une banque ou une autre institution financière ou auprès d'un fournisseur de services d'argent mobile",
                agd = agd_config, plt_caption = "(Personnes de 18 ans et plus)")
rm(agd_config)

# Graphique utile
outcome13_1_graph04 <- RMS_XXX_202X_main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  filter(!is.na(HH04) & !is.na(disability) & !is.na(HH07_cat) & HH07 >=18) %>%  # Exclude HH07_cat categories 1, 2, and 5
  mutate(
    grpage = case_when(
      HH07>=18 & HH07<=25 ~ "18-25",
      HH07>25 ~ "26+"
    )
  ) %>% 
  group_by(grpage, HH04, disability) %>%
  summarise(
    num_obs_uw = unweighted(n()),  # Unweighted total count
    denominator = survey_total(),                                # Weighted total count
    mean_value = survey_mean(outcome13_1, vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with CI and SE
  ) %>% 
  ggplot(aes(x = HH04, y = mean_value, fill = disability)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, position = position_dodge(0.7)) +  
  scale_fill_unhcr_d() +  # UNHCR color palette
  facet_wrap(~ grpage) +  # Create facets for each age group
  labs(
    title = "Outcome 13.1 par sexe, âge et handicap",
    subtitle = "Proportion de personnes ayant un compte dans une banque ou une autre institution financière ou auprès d'un fournisseur de services d'argent mobile",
    x = "Sexe",
    y = "Pourcentage",
    fill = "Handicap",
    caption = paste(unhcr_caption,"Note : Le module sur le handicap n'inclut pas les enfants de moins de 5 ans.")
  ) +
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
  )


#****************************************************************************************************************************************
###13.2 Proportion of people who self-report positive changes in their income compared to previous year ----
##Module :INC01 - INC02
#****************************************************************************************************************************************

###To calculate the indicator value, the standard survey methodology considers 
###for the numerator only individuals who state their income increased and 
##who can also afford more goods and services, or those whose income 
##remained the same or decreased but who can still afford more goods 
##and services (to account for possible inflation).
###To calculate the indicator value, the standard survey methodology considers 
###for the numerator only individuals who state their income increased and 
##who can also afford more goods and services, or those whose income 
##remained the same or decreased but who can still afford more goods 
##and services (to account for possible inflation).

main <- main %>%
  mutate(outcome13_2 = case_when(
    INC01 == "1" & INC02 == "1" ~ 1,  # Income increased and can afford more
    (INC01 == "2" | INC01 == "3") & INC02 == "1" ~ 1,  # Income decreased/same and can afford more
    TRUE ~ 0  # Default to 0
  )) %>%
  mutate(outcome13_2 = labelled(outcome13_2,
                                labels = c(
                                  "Oui" = 1,  # Label for Yes
                                  "Non" = 0    # Label for No
                                ),
                                label = "Proportion of people who self-report positive changes in their income compared to previous year"
  ))


###Check INC01 and INC02 
# Define stove categories based on the provided list
inc01_labels <- c(
  "1" = "Augmentation par rapport à l'année précédente",
  "2" = "Identique à l'année précédente",
  "3" = "Diminution par rapport à l'année précédente"  
)

# Summarize the counts and percentages for each category
INC01_percentages <- main %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  filter(!is.na(INC01) & INC01!='98') %>%  # Exclude missing values
  ungroup() %>% select(-c(cluster_id,cluster_name)) %>% 
  count(INC01, wt=UNHCR_WEIGHT) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC01 = factor(INC01, levels = names(inc01_labels), labels = inc01_labels))

# Create the chart
outcome13_2_graph03 <- ggplot(INC01_percentages, aes(x = reorder(INC01, Percentage), y = Percentage, fill = INC01)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5,color="orangered") +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Changements de revenus au cours des 12 derniers mois (INC01)",
    x = "Changements",
    y = "Pourcentage",
    caption = unhcr_caption
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

###INC02
# Define stove categories based on the provided list
inc02_labels <- c(
  "1" = "Plus d'informations",
  "2" = "Le même",
  "3" = "Moins"  
)

# Summarize the counts and percentages for each category
INC02_percentages <- main %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  filter(!is.na(INC02) & INC02!="98") %>%  # Exclude missing values
  ungroup() %>% select(-c(cluster_id,cluster_name)) %>% 
  count(INC02, wt=UNHCR_WEIGHT) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC02 = factor(INC02, levels = names(inc02_labels), labels = inc02_labels))


# Create the chart
outcome13_2_graph04 <- ggplot(INC02_percentages, aes(x = reorder(INC02, Percentage), y = Percentage, fill = INC02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Changements en terme de biens et services au cours des 12 derniers mois (INC02)",
    x = "Abordabilité",
    y = "Pourcentage",
    caption = unhcr_caption
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

###Table standard 
RMS_XXX_202X_main <- main %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE 
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome13_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome13_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome13_2",indicator_name="Outcome 13.2",
                plt_subtitle="Proportion de personnes déclarant une évolution positive de leurs revenus par rapport à l'année précédente",
                agd = agd_config, plt_caption = "(Personnes de 18 ans et plus)")
rm(agd_config)


#****************************************************************************************************************************************
###13.3 Proportion of people (working age) who are unemployed -----
#****************************************************************************************************************************************

##Indicator calculations
### Include only the labour force ( those who are in labour force)
### International standard is 15+ 
### Eurostat calculates for 18+ as in standard RMS Q V3.2


main <- main %>%
  # Create a working_age flag for those who are 18+ years old (numeric)
  mutate(working_age = case_when(
    HH07 >= 18 ~ 1,  # If 18 or older, mark as working age (numeric)
    TRUE ~ NA_real_  # Set to NA if not 18+
  )) %>%
  # Determine who is employed based on various conditions (character comparisons, numeric outcome)
  mutate(employed = case_when(
    UNEM01 == "1" & working_age == 1 ~ 1,  # Paid employment - employees
    (UNEM02 == "1" & UNEM07 == "3") & working_age == 1 ~ 1,  # Self-employment
    (UNEM03 == "1" & UNEM07 == "3") & working_age == 1 ~ 1,  # Unpaid contributing family workers
    UNEM04 == "1" & working_age == 1 ~ 1,  # Absent but still employed
    (UNEM05 == "1" & UNEM06 == "3") & working_age == 1 ~ 1,  # Absent but self-employed
    (UNEM02 == "1" & UNEM07 %in% c("1", "2") & UNEM08 %in% c("1", "2")) & working_age == 1 ~ 1,  # Farming/rearing/fishing for sale
    (UNEM05 == "1" & UNEM06 %in% c("1", "2") & UNEM08 %in% c("1", "2")) & working_age == 1 ~ 1,  # Absent but farming for sale
    working_age == 1 ~ 0,  # If working age but none of the above, mark as not employed
    TRUE ~ NA_real_  # Set to NA for those not in the working age group
  )) %>%
  # Define unemployed: those not employed but actively looking for work
  mutate(unemployed = case_when(
    (employed == 0 & UNEM09 == "1" & UNEM10 == "1") & working_age == 1 ~ 1,  # Actively looking for work
    working_age == 1 ~ 0,  # Not unemployed if not actively looking or employed
    TRUE ~ NA_real_  # Set to NA for those not in the working age group
  )) %>%
  # Define labour force: anyone employed or unemployed
  mutate(labour_force = case_when(
    (employed == 1 | unemployed == 1) & working_age == 1 ~ 1,  # Employed or actively seeking work
    working_age == 1 ~ 0,  # Not in labour force but 18+
    TRUE ~ NA_real_  # Set to NA for those not in the working age group
  )) %>%
  # Outcome: unemployment status among those in the labor force (numeric outcome)
  mutate(outcome13_3 = case_when(
    employed == 1 & labour_force == 1 ~ 0,  # If employed, outcome is 0 (not unemployed)
    unemployed == 1 & labour_force == 1 ~ 1,  # If unemployed, outcome is 1
    TRUE ~ NA_real_  # Catch-all for cases not in the labor force or with missing data
  ))

######Table standard 
RMS_XXX_202X_main <- main %>%
  filter(!is.na(outcome13_3)) %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome13_3",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome13_3")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome13_3",indicator_name="Outcome 13.3",
                plt_subtitle="Proportion de personnes en âge de travailler qui sont au chômage",
                agd = agd_config, plt_caption = "(Personnes de 18 ans et plus)")
rm(agd_config)

#****************************************************************************************************************************************
###14.1 Proportion of returnees with legally recognized identity documents or credentials -----
##Module :REG01 - REG02 - REG03 / REG05 - REG06 
#****************************************************************************************************************************************

##DISAGGREGATE FOR REFUGEE RETURNEES ONLY 
###Calculate valid identity documents for under 5 with REG05 and REG06 variables
#ind$REG05a - passport
#ind$REG05b - civil/government issued ID
#ind$REG05c - residency permit
#ind$REG05d - statelessness documentation
#ind$REG05e - household card of address/family book
#ind$REG05f - social security card
#ind$REG06 - any other document establishes identity
#add birth certificate as additional document from REG03

#ind$REG01a # passport
#ind$REG01b # birth certificate
#ind$REG01c # civil/ government issued ID
#ind$REG01d # residency permit
#ind$REG01e # statelessness documentation
#ind$REG01f # household card of address/family book
#ind$REG01g # social security card
#ind$REG02 # any other document establishes identity

ind <- ind %>%
  # Calculate identity documents for children under 5
  mutate(document_under5 = case_when(
    REG05a == "1" | REG05b == "1" | REG05c == "1" | REG05d == "1" | REG05e == "1" | REG05f == "1" | REG06 == "1" | REG03 == "1" ~ 1,  # Document present
    REG05a == "0" & REG05b == "0" & REG05c == "0" & REG05d == "0" & REG05e == "0" & REG05f == "0" & REG06 == "0" & REG03 == "0" ~ 0,  # No document
    TRUE ~ NA_real_  # Missing or invalid data
  )) %>%
  
  # Calculate valid identity documents for people over 5
  mutate(document_above5 = case_when(
    REG01a == "1" | REG01b == "1" | REG01c == "1" | REG01d == "1" | REG01e == "1" | REG01f == "1" | REG01g == "1" | REG02 == "1" ~ 1,  # Document present
    REG01a == "0" & REG01b == "0" & REG01c == "0" & REG01d == "0" & REG01e == "0" & REG01f == "0" & REG01g == "0" & REG02 == "0" ~ 0,  # No document
    TRUE ~ NA_real_  # Missing or invalid data
  )) %>%
  
  # Combine both age groups (under 5 and above 5)
  mutate(outcome14_1 = case_when(
    (document_above5 == 1 | document_under5 == 1) ~ 1,  # If either age group has a document, mark as "Oui"
    (document_above5 == 0 & document_under5 == 0) ~ 0,  # If neither age group has a document, mark as "Non"
    TRUE ~ NA_real_  # Missing or invalid data
  )) %>%
  
  # Label the outcome variable
  mutate(outcome14_1 = labelled(outcome14_1,
                                labels = c(
                                  'Oui' = 1,
                                  'Non' = 0
                                ),
                                label = "Proportion of returnees with legally recognized identity documents or credentials"
  ))


#### Table for indicator
###Table by population groups
RMS_XXX_202X_ind <- ind %>%
  #* [PAS POSSIBLE DE CALCULER DANS LE CAS DU TCHAD - RETOURNES PAR INTERVIEWES]
  # filter(pop_groups %in% c(var_RETURNED)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_ind,"outcome14_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_ind,"outcome14_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome14_1",indicator_name="Outcome 14.1",
                plt_subtitle="Proportion de rapatriés disposant de documents d'identité ou de titres légalement reconnus",
                agd = agd_config, plt_caption = "")
rm(agd_config)


#****************************************************************************************************************************************         
###16.1 Proportion of people with secure tenure rights to housing and/or land ----
##Module :DWE06_land – DWE06a_land – DWE07_land - DWE06_housing – DWE06a_housing – DWE07_housing – DWE10
#****************************************************************************************************************************************

# likelihood of losing right for housing is unlikely
# Have the documentation both for land and housing
main <- main %>%
  mutate(outcome16_1 = case_when(
    (DWE10 %in% c("1", "2")) & (DWE06a_land == "1" & DWE06a_housing == "1") ~ 1,  # Likelihood of losing right is unlikely and has documentation
    DWE06a_land == "0" | DWE06a_housing == "0" | DWE10 %in% c("3", "4") ~ 0,  # No documentation or likely to lose the right
    TRUE ~ NA_real_  # Handle missing values
  )) %>%
  mutate(outcome16_1 = labelled(outcome16_1,
                                labels = c("Non" = 0, "Oui" = 1),
                                label = "Proportion of people with secure tenure rights to housing and/or land"))


##Table by population groups
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE 
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome16_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome16_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome16_1",indicator_name="Outcome 16.1",
                plt_subtitle="Proportion de personnes bénéficiant de droits d'occupation sécurisés pour le logement et/ou la terre",
                agd = agd_config, plt_caption = "")
rm(agd_config)

###Check on DWE10
table(main$DWE10)
# Define DWE10 categories based on the provided list
DWE10_labels <- c(
  "1" = "Très peu probable",
  "2" = "Peu probable",
  "3" = "Assez probable",
  "4" = "Très probable",
  "99" = "Ne sait pas"
)

# Convert DWE10 to a factor with correct levels and labels BEFORE summarizing
main <- main %>%
  mutate(DWE10 = factor(DWE10, levels = names(DWE10_labels), labels = DWE10_labels))

# Summarize the counts and percentages for each category
DWE10_percentages <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  ungroup() %>% select(-c(cluster_id,cluster_name)) %>% 
  filter(!is.na(DWE10)) %>%  # Exclude missing values
  count(DWE10, wt=UNHCR_WEIGHT) %>%
  mutate(Percentage = n / sum(n) * 100)  # Calculate percentage based on total valid responses

# Create the chart
outcome16_1_graph03 <- ggplot(DWE10_percentages, aes(x = reorder(DWE10, Percentage), y = Percentage, fill = DWE10)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = 'orangered', 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Probabilité de perdre son logement/terre dans les 12 prochains mois",
    x = "Probabilité",
    y = "Pourcentage",
    caption = unhcr_caption
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

 
#****************************************************************************************************************************************        
###16.2 Proportion of people covered by national social protection systems ----
##Module :UNHCR Core Indicator Metadata	SPF01
#****************************************************************************************************************************************
         
#Module :UNHCR Core Indicator Metadata SPF01
main <- main %>%
  # Convert labelled/factor SPF01 columns to numeric
  # mutate(across(starts_with("SPF01"), ~ as.numeric(as.character(.)))) %>%
  mutate(across(starts_with("SPF01"), labelled_chr2dbl)) %>%
  
  rowwise() %>%
  mutate(outcome16_2 = case_when(
    any(c_across(starts_with("SPF01")) == 1) ~ 1,  # If any SPF01 column has 1
    all(c_across(starts_with("SPF01")) == 0) ~ 0,  # If all SPF01 columns are 0
    TRUE ~ 0                                      # Default case
  )) %>%
  
  # Add labels for outcome16_2
  mutate(outcome16_2 = labelled(outcome16_2,
                                labels = c(
                                  'Oui' = 1,
                                  'Non' = 0
                                ),
                                label = "Proportion of people covered by national social protection systems"))

##Table by population groups
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   # inv_poids_reel,inv_poids_ajuste_reel
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"outcome16_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"outcome16_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="outcome16_2",indicator_name="Outcome 16.2",
                plt_subtitle="Proportion of people covered by national social protection systems",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#### social protection services
###Please delete/adjust response options accordingly for the chart
# Define the mapping for SPF01 variables (social protection services)
spf01_mapping <- c(
  "SPF01a" = "Transferts en espèces / en nature",
  "SPF01b" = "Alimentation scolaire",
  "SPF01c" = "Travaux publics",
  "SPF01d" = "Subventions / dispenses de frais",
  "SPF01e" = "Indemnités de chômage",
  "SPF01f" = "Assurance maladie",
  "SPF01g" = "Pension de vieillesse",
  "SPF01h" = "Assurance récolte / bétail",
  "SPF01j" = "Travail social", # (protection de l'enfance, handicap, personnes âgées, violence liée au sexe)
  "SPF01k" = "Soutien à la famille",
  "SPF01l" = "Soutien psychosocial",
  "SPF01m" = "Formation professionnelle",
  "SPF01n" = "Services de recherche d'emploi",
  "SPF01o" = "Subventions salariales",
  "SPF01p" = "Amélioration de la sécurité foncière"
)

# Step 2: Calculate the percentage of individuals receiving each service
spf01_percentages <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  group_by(pop_groups) %>%  # Group by population group (e.g., gender, age group)
  summarise(across(c(SPF01a, SPF01b, SPF01c, SPF01d, SPF01e, SPF01f, SPF01g, SPF01h, 
                     SPF01j, SPF01k, SPF01l, SPF01m, SPF01n, SPF01o, SPF01p),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   # ~ mean(. == 1, na.rm = TRUE) * 100)) %>% # non prise en compte de la pondération..
                   ~ weighted.mean(. == 1, w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = -pop_groups,  # Exclude the PopGroup column from pivoting
               names_to = "Service", 
               values_to = "Percentage") %>%
  mutate(Service = spf01_mapping[Service])  # Map column names to descriptive labels


# Step 3: Create the bar chart
outcome16_2_graph03 <- ggplot(spf01_percentages, aes(x = reorder(Service, Percentage), y = Percentage, fill = Service)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),#colour = 'orangered', 
            position = position_stack(vjust = 0.5), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Proportion de personnes couvertes par les systèmes nationaux de protection sociale",
    x = "Service de protection sociale",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Chaque service est calculé indépendamment.")
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  ) +
  # Separate plots for each population group (e.g., gender, age group)
  facet_wrap(~pop_groups, scales = "free_y", ncol = 1)  # Create separate plots per PopGroup

###USER Indicators
#User define & Good pratics impact indicators ----

#****************************************************************************************************************************************
###1.1  Proportion des personnes envisageant un retour dans leur pays/localité d'origine  -----
#****************************************************************************************************************************************

main <- main %>%
  # Convert labelled/factor SPF01 columns to numeric
  rowwise() %>%
  mutate(
    user1_1 = case_when(
      INT01 %in% c("1","2") | INT02 %in% c("1","2") | INT03 %in% c("1","2") ~ 1,
      INT03 %in% c("3","4","98") & INT02 %in% c("3","4","98") & INT03 %in% c("3","4","98") ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>% 
  # Add labels for user1_1
  mutate(user1_1 = labelled(user1_1,
                                labels = c(
                                  'Oui' = 1,
                                  'Non' = 0
                                ),
                                label = "Proportion of people considering returning to their country/location of origin"))

# # Verification de l'indicateur
# main %>%  count(INT01,INT02,INT03,user1_1,wt=UNHCR_WEIGHT)
# main %>%  count(INT01,INT02,INT03,user1_1,wt=UNHCR_WEIGHT) %>% filter(user1_1==1) %>% tail(n=30) %>% print(n=30)
# main %>%  count(INT01,INT02,INT03,user1_1,wt=UNHCR_WEIGHT) %>% filter(user1_1==0) %>% print(n=30)
# main %>%  count(INT01,INT02,INT03,user1_1,wt=UNHCR_WEIGHT) %>%  filter(is.na(user1_1))

##Table by population groups
RMS_XXX_202X_main <- main %>%
  filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"user1_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"user1_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="user1_1",indicator_name="User indicator 1.1",
                plt_subtitle="Proportion des personnes envisageant un retour dans leur pays/localité d'origine",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###1.2  Proportion des personnes envisageant de déménager dans un pays/localité autre que leur pays/localité d'origine  -----
#****************************************************************************************************************************************

main <- main %>%
  rowwise() %>%
  mutate(
    user1_2 = case_when(
      INT03 %in% c("3","4") & INT04 %in% c("2","3") ~ 1,
      INT03 %in% c("3","4")  ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>% 
  # Add labels for user1_2
  mutate(user1_2 = labelled(user1_2,
                            labels = c(
                              'Oui' = 1,
                              'Non' = 0
                            ),
                            label = "Proportion of people considering moving to a country/location other than their country/location of origin"))

# # Verification de l'indicateur
# main %>%  count(INT03,INT04,user1_2,wt=UNHCR_WEIGHT)
# main %>%  count(INT03,INT04,user1_2,wt=UNHCR_WEIGHT) %>% filter(user1_2==1) %>% tail(n=30) %>% print(n=30)
# main %>%  count(INT03,INT04,user1_2,wt=UNHCR_WEIGHT) %>% filter(user1_2==0) %>% print(n=30)
# main %>%  count(INT03,INT04,user1_2,wt=UNHCR_WEIGHT) %>%  filter(is.na(user1_2))


##Table by population groups
RMS_XXX_202X_main <- main %>%
  filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # Cet indicateur a trait au répondant pas au chef de ménage.
  # Aussi, on écrase les variables du chef de ménage avec celui du répondant au questionnaire individuel
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07,HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual =p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age =p_ind_age
  ) %>% 
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"user1_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"user1_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="user1_2",indicator_name="User indicator 1.2",
                plt_subtitle="Proportion des personnes envisageant de déménager dans un pays/localité autre que leur pays/localité d'origine",
                agd = agd_config, plt_caption = "")
rm(agd_config)

#****************************************************************************************************************************************
###2.1  Proportion des personnes ayant connu un choc et déclarant être satisfait de la réponse humanitaire  -----
#****************************************************************************************************************************************

main <- main %>%
  rowwise() %>%
  mutate(
    user2_1 = case_when(
      !is.na(CONEX03) & CONEX03!="0" & CONEX06=="1" ~ 1,
      !is.na(CONEX03) & CONEX03!="0" & CONEX06=="0" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>%
  # Add labels for user2_1
  mutate(user2_1 = labelled(user2_1,
                            labels = c(
                              'Oui' = 1,
                              'Non' = 0
                            ),
                            label = "Proportion of people who experienced a shock and said they were satisfied with the humanitarian response"))


# # # Verification de l'indicateur
# main %>%  count(CONEX03,CONEX06,user2_1,wt=UNHCR_WEIGHT)
# main %>%  count(CONEX03,CONEX06,user2_1,wt=UNHCR_WEIGHT) %>% filter(user2_1==1) %>% tail(n=30) %>% print(n=30)
# main %>%  count(CONEX03,CONEX06,user2_1,wt=UNHCR_WEIGHT) %>% filter(user2_1==0) %>% print(n=30)
# main %>%  count(CONEX03,CONEX06,user2_1,wt=UNHCR_WEIGHT) %>%  filter(is.na(user2_1))


##Table by population groups
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  as_survey_design(
    ids = cluster_id,           # Specify the column with cluster IDs
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"user2_1",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"user2_1")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="user2_1",indicator_name="User indicator 2.1",
                plt_subtitle="Proportion des personnes ayant connu un choc et déclarant être satisfait de la réponse humanitaire",
                agd = agd_config, plt_caption = "")
rm(agd_config)

# Pricnipaux domaines ou des lacunes sont ressentis dans la réponse humanitaire
gaps_mapping <- c(
  "CONEX07_1" = "Accès à la nourriture",
  "CONEX07_2" = "Hébergement/logement",
  "CONEX07_3" = "Soins de santé",
  "CONEX07_4" = "Éducation",
  "CONEX07_5" = "Sécurité/protection",
  "CONEX07_6" = "Soutien psychologique",
  "CONEX07_7" = "Intégration sociale/emplois"
)

# Step 2: Calculate the percentage of individuals receiving each gap
gaps_percentages <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  group_by(pop_groups) %>%  # Group by population group (e.g., gender, age group)
  summarise(across(c(CONEX07_1, CONEX07_2, CONEX07_3, CONEX07_4, CONEX07_5,
                     CONEX07_6, CONEX07_7),
                   #~ mean(. == 1, na.rm = TRUE) * 100)) %>% #pondération non prise en compte
                   ~ weighted.mean(. == 1, w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
             .groups="drop") %>%
  pivot_longer(cols = -pop_groups,  # Exclude the PopGroup column from pivoting
               names_to = "Gaps",
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>%
  mutate(Gaps = gaps_mapping[Gaps])  # Map column names to descriptive labels


# Step 3: Create the bar chart
user2_1_graph03 <- ggplot(gaps_percentages, aes(x = reorder(Gaps, Percentage), y = Percentage, fill = Gaps)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = 'orangered',
            position = position_stack(vjust = 0.5), size = 3.5) +
            # position = position_stack(vjust = 1.05), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Proportion de personnes par lacunes ressenties dans la réponse humantire",
    x = "Lacunes dans la réponse humanitaire",
    y = "Pourcentage",
    caption = "Note : Chaque lacune est mesuré indépendamment."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  ) +
  # Separate plots for each population group (e.g., gender, age group)
  facet_wrap(~pop_groups, scales = "free_y", ncol = 1)  # Create separate plots per PopGroup


#****************************************************************************************************************************************
###2.2  Proportion des personnes déclarant utiliser un mécanisme de gestion de plainte et le trouvant efficace  -----
#****************************************************************************************************************************************

main <- main %>%
  rowwise() %>%
  mutate(
    user2_2 = case_when(
      !is.na(BQ901a) & BQ901a!="-1" & BQ901a!="0" & BQ902=="1" ~ 1,
      !is.na(BQ901a) & BQ901a!="-1" & BQ901a!="0" & BQ902=="2" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup() %>% 
  # Add labels for user2_2
  mutate(user2_2 = labelled(user2_2,
                            labels = c(
                              'Oui' = 1,
                              'Non' = 0
                            ),
                            label = "Proportion of people who say they use a complaint management mechanism and find it effective"))

##Table by population groups
RMS_XXX_202X_main <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  # as_survey_design(
  #   ids = cluster_id,           # Specify the column with cluster IDs
  #   weights = UNHCR_WEIGHT, # Specify the column with survey weights
  #   nest = FALSE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  # )
  as_survey_design(
    strata = c(zone_id),
    ids = c(localite_id,parent_index),#cluster_id,           # Specify the column with cluster IDs
    # inv_poids_reel,inv_poids_ajuste_reel
    weights = UNHCR_WEIGHT, # Specify the column with survey weights
    nest = TRUE              # Use TRUE if PSUs are nested within clusters (optional, based on your survey design)
  )

#' [Generation des tables standards]
agd_config <- "disability_gender_age"
get_rmsTable(RMS_XXX_202X_main,"user2_2",agd = agd_config)
get_rmsTable_AGD(RMS_XXX_202X_main,"user2_2")

#' [Generation des graphiques standards]
get_rmsGraphics(indicator="user2_2",indicator_name="User indicator 2.2",
                plt_subtitle="Proportion des personnes déclarant utiliser un mécanisme de gestion de plainte et le trouvant efficace",
                agd = agd_config, plt_caption = "")
rm(agd_config)


# Utiliser BQ901C_1:BQ901C_6 difficultés rencontrées pour l'utilisation du MGP
mgp_diff <- c(
  "BQ901c_1" = "Difficulté à trouver les informations pour déposer la plainte",
  "BQ901c_2" = "Complexité du processus",
  "BQ901c_3" = "Temps d'attente trop long pour une réponse",
  "BQ901c_4" = "Manque de suivi sur l'état de la plainte",
  "BQ901c_5" = "Sentiment que la plainte n'a pas été prise au sérieux",
  "BQ901c_6" = "Résolution insatisfaisante"
)

# Step 2: Calculate the percentage of individuals receiving each difficulty
diff_percentages <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  group_by(pop_groups) %>%  # Group by population group (e.g., gender, age group)
  summarise(across(c(BQ901c_1, BQ901c_2, BQ901c_3, BQ901c_4, BQ901c_5, 
                     BQ901c_6), 
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = -pop_groups,  # Exclude the PopGroup column from pivoting
               names_to = "Difficulties", 
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>% 
  mutate(Difficulties = mgp_diff[Difficulties])  # Map column names to descriptive labels


# Step 3: Create the bar chart
user2_2_graph03 <- ggplot(diff_percentages, aes(x = reorder(Difficulties, Percentage), y = Percentage, fill = Difficulties)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = 'orangered', 
            position = position_stack(vjust = 0.5), size = 3.5) +
            # position = position_stack(vjust = 1.05), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Proportion de personnes par difficultés rencontrées lors de l'utilisation d'un MGP",
    x = "Difficultés rencontrées",
    y = "Pourcentage",
    caption = "Note : Chaque difficulté est mesurée indépendamment."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  ) +
  # Separate plots for each population group (e.g., gender, age group)
  facet_wrap(~pop_groups, scales = "free_y", ncol = 1)  # Create separate plots per PopGroup


# Raison de la non-utilisation de mécanisme mgp : BQ902b_2 à BQ902b_7
# Utiliser BQ901C_1:BQ901C_6 difficultés rencontrées pour l'utilisation du MGP
mgp_nouse <- c(
  "BQ902b_2" = "La peur d'être victime",
  "BQ902b_3" = "C'est coûteux",
  "BQ902b_4" = "Cela prend du temps",
  "BQ902b_5" = "Il n'y a pas eu de retour d'information",
  "BQ902b_6" = "Il n'est pas facile à utiliser",
  "BQ902b_7" = "Je ne m'attendais pas à recevoir de l'aide (assistance)"
)

# Step 2: Calculate the percentage of individuals receiving each reason
mgp_nouse_percentages <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  group_by(pop_groups) %>%  # Group by population group (e.g., gender, age group)
  summarise(across(c(BQ902b_2, BQ902b_3, BQ902b_4, BQ902b_5, BQ902b_6, BQ902b_7), 
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   ~ weighted.mean(. == 1, w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = -pop_groups,  # Exclude the PopGroup column from pivoting
               names_to = "Reasons", 
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>% 
  mutate(Reasons = mgp_nouse[Reasons])  # Map column names to descriptive labels


# Step 3: Create the bar chart
user2_2_graph04 <- ggplot(mgp_nouse_percentages, aes(x = reorder(Reasons, Percentage), y = Percentage, fill = Reasons)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),#colour = 'orangered'), 
            position = position_stack(vjust = 1.09), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Proportion de personnes par motifs pour la non utilisation d'un MGP",
    x = "Motifs",
    y = "Pourcentage",
    caption = "Note : Chaque raison est mesuré indépendamment."
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  ) +
  # Separate plots for each population group (e.g., gender, age group)
  facet_wrap(~pop_groups, scales = "free_y", ncol = 1)  # Create separate plots per PopGroup



#*****************************************************************************
####FINAL TABLE -----
#*****************************************************************************

# cat(glue::glue_col(
#   "{green [ PRODUCTION DE LA TABLE FINAL DES INDICATEURS ] }"
# ),"\n")

##Delete if you don't have some of the indicators
# Combine all indicators into one data frame
# Ajute les data_frame de sorte que l'on puisse avoir les variables pop_groups, disability et autre
# dans une colonne disaggregation_variable et leur modalites dans une variable disaggregation_value
# pour ce faire utiliser pivot_longer
combined_RBM_indicators <- bind_rows(
  
  #' # # Export des résultats de l'impact 1.2
  #' # # [Cet indicateur ne figure pas dans le RMS Standard]
  #' rebuild_table(impact1_2_total,critical_value=39,acceptable_value=70),
  #' rebuild_table(impact1_2,critical_value=39,acceptable_value=70),
  #' 
  # Export des résultats de l'impact 2.1
  # [Ajout au RMS ne figure pas parmi les questions - pas disponible]
  # rebuild_table(impact2_1_total,critical_value = 35,acceptable_value = 50,inverse=TRUE), #à insérer plus tard....
  # rebuild_table(impact2_1,critical_value = 35,acceptable_value = 50,inverse=TRUE), #à insérer plus tard....
  
  # Export des résultats de l'impact 2.2
  rebuild_table(impact2_2_total,critical_value = 69,acceptable_value = 90),
  rebuild_table(impact2_2,critical_value = 69,acceptable_value = 90),
  rebuild_table(impact2_2_AGD,critical_value = 69,acceptable_value = 90,
                agd_type = "disability_gender_age"),
  # rebuild_table(impact2_2_age,critical_value = 69,acceptable_value = 90),
  rebuild_table(impact2_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value = 69,acceptable_value = 90),
  rebuild_table(impact2_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value = 69,acceptable_value = 90),
  rebuild_table(impact2_2_disability,critical_value = 69,acceptable_value = 90),
  rebuild_table(impact2_2_gender,critical_value = 69,acceptable_value = 90),
  
  # Export des résultats de l'impact 2.3
  rebuild_table(impact2_3_total,critical_value = 59 ,acceptable_value = 90),
  rebuild_table(impact2_3,critical_value = 59 ,acceptable_value = 90),
  rebuild_table(impact2_3_AGD,critical_value = 59 ,acceptable_value = 90,
                agd_type="disability_gender_age"),
  # rebuild_table(impact2_3_age,critical_value = 59 ,acceptable_value = 90),
  rebuild_table(impact2_3_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value = 59 ,acceptable_value = 90),
  rebuild_table(impact2_3_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value = 59 ,acceptable_value = 90),
  rebuild_table(impact2_3_disability,critical_value = 59 ,acceptable_value = 90),
  rebuild_table(impact2_3_gender,critical_value = 59 ,acceptable_value = 90),
  
  # # Export des résultats de l'impact 3.1
  ##' [Cet indicateur ne figure pas dans le RMS Standard]
  # rebuild_table(impact3_1_total,critical_value = 39,acceptable_value = 70), 
  # rebuild_table(impact3_1,critical_value = 39,acceptable_value = 70),
  
  # Export des résultats de l'impact 3.2a
  rebuild_table(impact3_2a_total,critical_value=59,acceptable_value=80),
  rebuild_table(impact3_2a,critical_value=59,acceptable_value=80),
  rebuild_table(impact3_2a_AGD,critical_value=59,acceptable_value=80,
                agd_type="disability_gender"),
  rebuild_table(impact3_2a_disability,critical_value=59,acceptable_value=80),
  rebuild_table(impact3_2a_gender,critical_value=59,acceptable_value=80),
  
  # Export des résultats de l'impact 3.2b
  rebuild_table(impact3_2b_total,critical_value=49,acceptable_value=70),
  rebuild_table(impact3_2b,critical_value=49,acceptable_value=70),
  rebuild_table(impact3_2b_AGD,critical_value=49,acceptable_value=70,
                agd_type="disability_gender"),
  rebuild_table(impact3_2b_disability,critical_value=49,acceptable_value=70),
  rebuild_table(impact3_2b_gender,critical_value=49,acceptable_value=70),
  
  # Export des résultats de l'impact 3.3
  rebuild_table(impact3_3_total,critical_value =0 ,acceptable_value = 65),
  rebuild_table(impact3_3,critical_value = 0,acceptable_value = 65),
  rebuild_table(impact3_3_AGD,
                critical_value = 0,
                acceptable_value = 65,
                agd_type="disability_gender_age"),
  # rebuild_table(impact3_3_age,critical_value = 0 ,acceptable_value = 65),
  rebuild_table(impact3_3_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value = 0 ,acceptable_value = 65),
  rebuild_table(impact3_3_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value = 0 ,acceptable_value = 65),
  rebuild_table(impact3_3_disability,critical_value = 0 ,acceptable_value = 65 ),
  rebuild_table(impact3_3_gender,critical_value = 0 ,acceptable_value = 65 ),
  
  # Export des résultats de l'outcome 1.2
  rebuild_table(outcome1_2_total,critical_value=39,acceptable_value=80),
  rebuild_table(outcome1_2,critical_value=39,acceptable_value=80),
  rebuild_table(outcome1_2_AGD,
                critical_value = 39,
                acceptable_value =80,
                agd_type="gender_age"),
                # agd_type="disability_gender"),
  # rebuild_table(outcome1_2_age,critical_value=39,acceptable_value=80),
  rebuild_table(outcome1_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=39,acceptable_value=80),
  rebuild_table(outcome1_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=39,acceptable_value=80),
  # rebuild_table(outcome1_2_disability,critical_value=39,acceptable_value=80),
  rebuild_table(outcome1_2_gender,critical_value=39,acceptable_value=80), 
  
  # Export des résultats de l'outcome 1.3
  rebuild_table(outcome1_3_total,critical_value=79,acceptable_value=90),
  rebuild_table(outcome1_3,critical_value=79,acceptable_value=90),
  rebuild_table(outcome1_3_AGD,
                critical_value=79,
                acceptable_value=90,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome1_3_age,critical_value=79,acceptable_value=90),
  rebuild_table(outcome1_3_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=79,acceptable_value=90),
  rebuild_table(outcome1_3_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=79,acceptable_value=90),
  rebuild_table(outcome1_3_disability,critical_value=79,acceptable_value=90),
  rebuild_table(outcome1_3_gender,critical_value=79,acceptable_value=90),
  
  # Export des résultats de l'outcome 4.1
  rebuild_table(outcome4_1_total,critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1,critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1_AGD,
                critical_value=40,
                acceptable_value=71,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome4_1_age,critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1_disability,critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1_gender,critical_value=40,acceptable_value=71),
  rebuild_table(outcome4_1_EDU,critical_value=40,acceptable_value=71),
  
  # Export des résultats de l'outcome 4.2
  rebuild_table(outcome4_2_total,critical_value=20,acceptable_value=60),
  rebuild_table(outcome4_2,critical_value=20,acceptable_value=60),
  rebuild_table(outcome4_2_AGD,
                critical_value=20,
                acceptable_value=60,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome4_2_age,critical_value=20,acceptable_value=60),
  rebuild_table(outcome4_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat1),
                critical_value=20,acceptable_value=60),
  rebuild_table(outcome4_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat2),
                critical_value=20,acceptable_value=60),
  rebuild_table(outcome4_2_disability,critical_value=20,acceptable_value=60),
  rebuild_table(outcome4_2_gender,critical_value=20,acceptable_value=60),
  
  # # Export des résultats de l'outcome 4.3
  ##' [Cet indicateur ne figure pas dans le RMS Standard]
  ##' LES VARIABLES PERMETTANT DE LE MESURER DEVRAIENT ETRE AJOUTEES DANS
  ##' LE PROCHAIN EXCERCICE
  # rebuild_table(outcome4_3_total,critical_value=40,acceptable_value=71),
  # rebuild_table(outcome4_3,critical_value=40,acceptable_value=71),
  # rebuild_table(outcome4_3_AGD,
  #               critical_value=40,
  #               acceptable_value=71,
  #               agd_type="disability_gender_age"),
  # rebuild_table(outcome4_3_age,critical_value=40,acceptable_value=71),
  # rebuild_table(outcome4_3_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
  #                 rename(HH07_cat=HH07_cat1),
  #               critical_value=40,acceptable_value=71),
  # rebuild_table(outcome4_3_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
  #                 rename(HH07_cat=HH07_cat2),
  #               critical_value=40,acceptable_value=71),
  # rebuild_table(outcome4_3_disability,critical_value=40,acceptable_value=71),
  # rebuild_table(outcome4_3_gender,critical_value=40,acceptable_value=71),
  
  # Export des résultats de l'outcome 5.2
  rebuild_table(outcome5_2_total,critical_value=69,acceptable_value=90),
  rebuild_table(outcome5_2,critical_value=69,acceptable_value=90),
  rebuild_table(outcome5_2_AGD,
                critical_value=69,
                acceptable_value=90,
                agd_type="disability_gender"),
  # rebuild_table(outcome5_2_age,critical_value=69,acceptable_value=90),
  rebuild_table(outcome5_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat1),
                critical_value=69,acceptable_value=90),
  rebuild_table(outcome5_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat2),
                critical_value=69,acceptable_value=90),
  rebuild_table(outcome5_2_disability,critical_value=69,acceptable_value=90),
  rebuild_table(outcome5_2_gender,critical_value=69,acceptable_value=90),
  
  # # Export des résultats de l'outcome 7.3
  ##' [Cet indicateur ne figure pas dans le RMS Standard]
  # rebuild_table(outcome7_3_total,critical_value=19,acceptable_value=35),
  # rebuild_table(outcome7_3,critical_value=19,acceptable_value=35),
  # rebuild_table(outcome7_3_AGD,
  #               critical_value=19,
  #               acceptable_value=35,
  #               agd_type="disability_age"),
  # rebuild_table(outcome7_3_age,critical_value=19,acceptable_value=35),
  # rebuild_table(outcome7_3_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
  #                 rename(HH07_cat=HH07_cat1),
  #               critical_value=19,acceptable_value=35),
  # rebuild_table(outcome7_3_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
  #                 rename(HH07_cat=HH07_cat2),
  #               critical_value=19,acceptable_value=35),
  # rebuild_table(outcome7_3_disability,critical_value=19,acceptable_value=35),

  # Export des résultats de l'outcome 8.2
  rebuild_table(outcome8_2_total,critical_value=19,acceptable_value=60),
  rebuild_table(outcome8_2,critical_value=19,acceptable_value=60),
  rebuild_table(outcome8_2_AGD,
                critical_value=19,
                acceptable_value=60,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome8_2_age,critical_value=19,acceptable_value=60),
  rebuild_table(outcome8_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=19,acceptable_value=60),
  rebuild_table(outcome8_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=19,acceptable_value=60),
  rebuild_table(outcome8_2_disability,critical_value=19,acceptable_value=60),
  rebuild_table(outcome8_2_gender,critical_value=19,acceptable_value=60),
  
  # Export des résultats de l'outcome 9.1
  rebuild_table(outcome9_1_total,critical_value=69,acceptable_value=85),
  rebuild_table(outcome9_1,critical_value=69,acceptable_value=85),
  rebuild_table(outcome9_1_AGD,
                critical_value=69,
                acceptable_value=85,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome9_1_age,critical_value=69,acceptable_value=85),
  rebuild_table(outcome9_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=69,acceptable_value=85),
  rebuild_table(outcome9_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=69,acceptable_value=85),
  rebuild_table(outcome9_1_disability,critical_value=69,acceptable_value=85),
  rebuild_table(outcome9_1_gender,critical_value=69,acceptable_value=85),
  
  # Export des résultats de l'outcome 9.2
  rebuild_table(outcome9_2_total,critical_value=19,acceptable_value=60),
  rebuild_table(outcome9_2,critical_value=19,acceptable_value=60),
  rebuild_table(outcome9_2_AGD,
                critical_value=19,
                acceptable_value=60,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome9_2_age,critical_value=19,acceptable_value=60),
  rebuild_table(outcome9_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=19,acceptable_value=60),
  rebuild_table(outcome9_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=19,acceptable_value=60),
  rebuild_table(outcome9_2_disability,critical_value=19,acceptable_value=60),
  rebuild_table(outcome9_2_gender,critical_value=19,acceptable_value=60),
  
  # Export des résultats de l'outcome 10.1
  rebuild_table(outcome10_1_total,critical_value=89,acceptable_value=95),
  rebuild_table(outcome10_1,critical_value=89,acceptable_value=95),
  rebuild_table(outcome10_1_AGD,
                critical_value = 89,
                acceptable_value = 95,
                agd_type="gender_age"),
                # agd_type="disability_gender"),
  # rebuild_table(outcome10_1_disability,critical_value=89,acceptable_value=95),
  rebuild_table(outcome10_1_gender,critical_value=89,acceptable_value=95),
  
  # Export des résultats de l'outcome 10.2
  rebuild_table(outcome10_2_total,critical_value=79,acceptable_value=90),
  rebuild_table(outcome10_2,critical_value=79,acceptable_value=90),
  ## Les variables ci-après concerne le chef de ménage (âge, genre, handicap)
  rebuild_table(outcome10_2_AGD,
                critical_value = 79,
                acceptable_value = 90,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome10_2_age,critical_value=79,acceptable_value=90),
  rebuild_table(outcome10_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat1),
                critical_value=79,acceptable_value=90),
  rebuild_table(outcome10_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat2),
                critical_value=79,acceptable_value=90),
  rebuild_table(outcome10_2_disability,critical_value=79,acceptable_value=90),
  rebuild_table(outcome10_2_gender,critical_value=79,acceptable_value=90),
  
  # # Export des résultats de l'outcome 11.1
  ##' [Cet indicateur ne figure pas dans le RMS Standard]
  rebuild_table(outcome11_1_total,critical_value=10,acceptable_value=15),
  rebuild_table(outcome11_1,critical_value=10,acceptable_value=15),
  rebuild_table(outcome11_1_AGD,
                critical_value = 10,
                acceptable_value = 15,
                agd_type="disability_gender"),
  rebuild_table(outcome11_1_disability,critical_value=10,acceptable_value=15),
  rebuild_table(outcome11_1_gender,critical_value=10,acceptable_value=15),
  
  # # Export des résultats de l'outcome 11.2
  ##' [Cet indicateur ne figure pas dans le RMS Standard]
  rebuild_table(outcome11_2_total,critical_value=30,acceptable_value=60),
  rebuild_table(outcome11_2,critical_value=30,acceptable_value=60),
  rebuild_table(outcome11_2_AGD,
                critical_value = 30,
                acceptable_value = 60,
                agd_type="disability_gender"),
  rebuild_table(outcome11_2_disability,critical_value=30,acceptable_value=60),
  rebuild_table(outcome11_2_gender,critical_value=30,acceptable_value=60),
  
  # Export des résultats de l'outcome 12.1
  rebuild_table(outcome12_1_total,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_1,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_1_AGD,
                critical_value = 69,
                acceptable_value = 90,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome12_1_age,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_1_disability,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_1_gender,critical_value=69,acceptable_value=90),
  
  # Export des résultats de l'outcome 12.2
  rebuild_table(outcome12_2_total,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_2,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_2_AGD,
                critical_value = 69,
                acceptable_value = 90,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome12_2_age,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_2_disability,critical_value=69,acceptable_value=90),
  rebuild_table(outcome12_2_gender,critical_value=69,acceptable_value=90),
  
  # Export des résultats de l'outcome 13.1
  rebuild_table(outcome13_1_total,critical_value=50,acceptable_value=60),
  rebuild_table(outcome13_1,critical_value=50,acceptable_value=60),
  rebuild_table(outcome13_1_AGD,
                critical_value=50,acceptable_value=60,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome13_1_age,critical_value=50,acceptable_value=60),
  rebuild_table(outcome13_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=50,acceptable_value=60),
  rebuild_table(outcome13_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=50,acceptable_value=60),
  rebuild_table(outcome13_1_disability,critical_value=50,acceptable_value=60),
  rebuild_table(outcome13_1_gender,critical_value=50,acceptable_value=60),
  
  # Export des résultats de l'outcome 13.2
  rebuild_table(outcome13_2_total,critical_value=10,acceptable_value=33),
  rebuild_table(outcome13_2,critical_value=10,acceptable_value=33),
  rebuild_table(outcome13_2_AGD,
                critical_value=10,acceptable_value=33,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome13_2_age,critical_value=10,acceptable_value=33),
  rebuild_table(outcome13_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=10,acceptable_value=33),
  rebuild_table(outcome13_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=10,acceptable_value=33),
  rebuild_table(outcome13_2_disability,critical_value=10,acceptable_value=33),
  rebuild_table(outcome13_2_gender,critical_value=10,acceptable_value=33),
  
  # Export des résultats de l'outcome 13.3
  rebuild_table(outcome13_3_total,critical_value=20,acceptable_value=10,inverse = TRUE),
  rebuild_table(outcome13_3,critical_value=20,acceptable_value=10,inverse = TRUE),
  rebuild_table(outcome13_3_AGD,
                critical_value=20,acceptable_value=10,inverse = TRUE,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome13_3_age,critical_value=20,acceptable_value=10,inverse = TRUE),
  rebuild_table(outcome13_3_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=20,acceptable_value=10,inverse = TRUE),
  rebuild_table(outcome13_3_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=20,acceptable_value=10,inverse = TRUE),
  rebuild_table(outcome13_3_disability,critical_value=20,acceptable_value=10,inverse = TRUE),
  rebuild_table(outcome13_3_gender,critical_value=20,acceptable_value=10,inverse = TRUE),
  
  # # Export des résultats de l'outcome 14.1
  # rebuild_table(outcome14_1_total,critical_value=79,acceptable_value=90),
  # rebuild_table(outcome14_1,critical_value=79,acceptable_value=90),
  # rebuild_table(outcome14_1_AGD,
  #               critical_value=79,acceptable_value=90,
  #               agd_type="disability_gender_age"),
  # rebuild_table(outcome14_1_age,critical_value=79,acceptable_value=90),
  # rebuild_table(outcome14_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
  #                 rename(HH07_cat=HH07_cat1),
  #               critical_value=79,acceptable_value=90),
  # rebuild_table(outcome14_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
  #                 rename(HH07_cat=HH07_cat2),
  #               critical_value=79,acceptable_value=90),
  # rebuild_table(outcome14_1_disability,critical_value=79,acceptable_value=90),
  # rebuild_table(outcome14_1_gender,critical_value=79,acceptable_value=90),
  
  # Export des résultats de l'outcome 16.1
  rebuild_table(outcome16_1_total,critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_1,critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_1_AGD,
                critical_value=33,acceptable_value=66,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome16_1_age,critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_1_disability,critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_1_gender,critical_value=33,acceptable_value=66),
  
  # Export des résultats de l'outcome 16.2
  rebuild_table(outcome16_2_total,critical_value=32,acceptable_value=66),
  rebuild_table(outcome16_2,critical_value=32,acceptable_value=66),
  rebuild_table(outcome16_2_AGD,
                critical_value=33,acceptable_value=66,
                agd_type="disability_gender_age"),
  # rebuild_table(outcome16_2_age,critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_2_disability,critical_value=33,acceptable_value=66),
  rebuild_table(outcome16_2_gender,critical_value=33,acceptable_value=66),
  
  # USER_DEFINE & GOOD_PRATICS INDICATORS
  #*****************************************
  
  # Export des résultats USER Indicator 1.1
  rebuild_table(user1_1_total,critical_value=25,acceptable_value=75),
  rebuild_table(user1_1,critical_value=25,acceptable_value=75),
  rebuild_table(user1_1_AGD,
                critical_value=25,acceptable_value=75,
                agd_type="disability_gender_age"),
  # rebuild_table(user1_1_age,critical_value=25,acceptable_value=75),
  rebuild_table(user1_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=25,acceptable_value=75),
  rebuild_table(user1_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=25,acceptable_value=75),
  rebuild_table(user1_1_disability,critical_value=25,acceptable_value=75),
  rebuild_table(user1_1_gender,critical_value=25,acceptable_value=75),
  
  # Export des résultats USER Indicator 1.2
  rebuild_table(user1_2_total,critical_value=25,acceptable_value=75,inverse = TRUE),
  rebuild_table(user1_2,critical_value=25,acceptable_value=75,inverse = TRUE),
  rebuild_table(user1_2_AGD,
                critical_value=25,acceptable_value=75,
                agd_type="disability_gender_age",inverse = TRUE),
  # rebuild_table(user1_2_age,critical_value=25,acceptable_value=75,inverse = TRUE),
  rebuild_table(user1_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=25,acceptable_value=75,inverse = TRUE),
  rebuild_table(user1_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=25,acceptable_value=75,inverse = TRUE),
  rebuild_table(user1_2_disability,critical_value=25,acceptable_value=75,inverse = TRUE),
  rebuild_table(user1_2_gender,critical_value=25,acceptable_value=75,inverse = TRUE),
  
  # # Export des résultats USER Indicator 2.1
  rebuild_table(user2_1_total,critical_value=25,acceptable_value=75),
  rebuild_table(user2_1,critical_value=25,acceptable_value=75),
  rebuild_table(user2_1_AGD,
                critical_value=25,acceptable_value=75,
                agd_type="disability_gender_age"),
  # rebuild_table(user2_1_age,critical_value=25,acceptable_value=75),
  rebuild_table(user2_1_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat1),
                critical_value=25,acceptable_value=75),
  rebuild_table(user2_1_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>%
                  rename(HH07_cat=HH07_cat2),
                critical_value=25,acceptable_value=75),
  rebuild_table(user2_1_disability,critical_value=25,acceptable_value=75),
  rebuild_table(user2_1_gender,critical_value=25,acceptable_value=75),
  
  # Export des résultats USER Indicator 2.1
  rebuild_table(user2_2_total,critical_value=25,acceptable_value=75),
  rebuild_table(user2_2,critical_value=25,acceptable_value=75),
  rebuild_table(user2_2_AGD,
                critical_value=25,acceptable_value=75,
                agd_type="disability_gender_age"),
  # rebuild_table(user2_2_age,critical_value=25,acceptable_value=75),
  rebuild_table(user2_2_age_1 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat1),
                critical_value=25,acceptable_value=75),
  rebuild_table(user2_2_age_2 %>% mutate(var_name=str_replace_all(var_name,"_age_1|_age_2","_age")) %>% 
                  rename(HH07_cat=HH07_cat2),
                critical_value=25,acceptable_value=75),
  rebuild_table(user2_2_disability,critical_value=25,acceptable_value=75),
  rebuild_table(user2_2_gender,critical_value=25,acceptable_value=75)
)

# L'étape ci-dessous a pour but de supprimer de combined_RBM_indicators
# les indicateurs qui n'ont pas à être renseigné dans le COMPASS
#  Mais on leur fournira un fichier compler avec de'autres indicateuurs
#  Libre à eux de décider si ils vont les exploiter ou pas...
rms_indicators <- rms_infos %>% 
  select(indicator_code,variable=disaggregation_variable,disaggregation=disaggregation_value) %>% 
  left_join(combined_RBM_indicators,
            by = join_by(indicator_code, variable, disaggregation))

# This will export the combined data frame to a single Excel file
# # Fusion des résultats avec les informations relatifs à chaque indicateur
result <- rms_infos %>%
  left_join(
    rms_indicators %>% 
      #* [pas  correct deja variable='aucun' ensuite la presence de taux rend caduque le calcul de value]
      #* [en outre deja pris en compte dans combined_RBM_indicators]
      # bind_rows( 
      #   rms_indicators %>% 
      #     filter(variable=="population") %>% 
      #     group_by(indicator_code) %>%
      #     summarise(variable="none",
      #               disaggregation="aucun", 
      #               numerator = sum(numerator,na.rm=TRUE), 
      #               denominator = sum(denominator,na.rm=TRUE),
      #               value = round(100*numerator/denominator,1)) #pas correct car on a des taux...
      # ) %>% 
      arrange(indicator_code,desc(variable),disaggregation),
    by=join_by(indicator_code,
               disaggregation_variable==variable,
               disaggregation_value==disaggregation)
  )


# Export des pvlaues
tbl_pvalues_names <-  setdiff(str_subset(ls(),"(_pvalues)$"),
                              c("get_pvalues","rms_pvalues"))
rms_pvalues <- bind_rows(mget(tbl_pvalues_names, envir = .GlobalEnv)) %>% 
  # correction de certaines nom de variables
  mutate(
    variable=str_replace_all(variable,"HH07_cat1|HH07_cat2","HH07_cat") %>% 
      str_replace_all("HH07_cat","age") %>% str_replace_all("age1|age2","age"),
    variable=str_replace_all(variable,"HH04","gender")
  ) %>% 
  distinct()
rm(tbl_pvalues_names) #suppression de ce nom de variables inutiles


# #*****************************************************************************
# # ####EXPORT RESULTS -----
# #*****************************************************************************
# 
# # Export the combined data frame to an Excel file
# cat(glue::glue_col(
#   "{green [ EXPORT DE LA TABLE FINAL DES INDICATEURS ] }"
# ),"\n")
# 
# # Export de la table compléte des indicateurs
# export_dataset(
#   combined_RBM_indicators %>% filter(!is.na(status)),
#   file.path("local","database","analysis","raw_Combined_Indicators.xlsx")
# )
# 
# # Export de la table des indicateurs qui a été ajusté au core indicator metdata 
# # et au questionnaire standard du RMS
# export_dataset(
#   result %>% filter(!is.na(status)), 
#   file.path("local","database","analysis","Combined_Indicators.xlsx")
# )
#  
# 
# #* [Export des données d'analyses]
# cat(glue::glue_col(
#   "{green [ EXPORT DES BASES DE DONNEES DES ANALYSES ] }"
# ),"\n")
# 
# #Enregistrement des bases de données d'analyse
# suppressWarnings(saving_datasets("analysis",echo=FALSE))
# 
# #* [Export des graphiques produits]
# cat(glue::glue_col(
#   "{green [ EXPORT DES GRAPHIQUES PRODUITS ] }"
# ),"\n")
# 
# # Enregistrement des graphiques produits
# saving_graphics(echo = FALSE)
# 
