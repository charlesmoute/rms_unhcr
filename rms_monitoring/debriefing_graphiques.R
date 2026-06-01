# Chargement des  bibliothèques nécessaires
pacman::p_load(
  tidyverse,
  rio,
  unhcrthemes
)

# Chargement et traitement des données utiles
rejected_list <-  config$monitoring_data %>% 
  filter(type=="tx_missing_intra",value>=10) %>% 
  pluck("caseid") %>% unique()

# Effectif par population dénombrés
config$data %>% 
  filter(!caseid %in% rejected_list ) %>% 
  mutate( #' [CAS PARTICULIER RMS TCHAD 2025-2026]
    zone_id=region,zone_name = region_name,
    end_result_code = ifelse(
      caseid %in% c(116487430,116487428,116487427,116487426,116487425,116487423),
      1,end_result_code
    )
  ) %>% 
  count(zone_id,zone_name,localite_id=localite,localite_name,population,population_name) %>% 
  rename(population_id= population,eff=n) %>% 
  mutate(
    cluster_id = as.numeric(str_glue("{localite_id}{population_id}")),
    cluster_zone_id = as.numeric(str_glue("{zone_id}{population_id}"))
  ) %>%
  arrange(cluster_id) %>% 
  group_by(cluster_zone_id,zone_id,zone_name,population_id,population_name) %>% 
  summarise(eff=sum(eff),.groups='drop') %>% 
  select(cluster_id=cluster_zone_id,zone_id,zone_name,population_id,population_name,eff) %>% 
  openxlsx::write.xlsx(file="export_zone_survey.xlsx")

# Effectif population dénombrés
config$data %>% 
  filter(!caseid %in% rejected_list ) %>% 
  mutate( #' [CAS PARTICULIER RMS TCHAD 2025-2026]
    zone_id=region,zone_name = region_name,
    end_result_code = ifelse(
      caseid %in% c(116487430,116487428,116487427,116487426,116487425,116487423),
      1,end_result_code
    )
  ) %>% 
  count(zone_id,zone_name,localite_id=localite,localite_name,population,population_name) %>% 
  rename(population_id= population,eff=n) %>% 
  mutate(
    cluster_id = as.numeric(str_glue("{localite_id}{population_id}")),
    cluster_zone_id = as.numeric(str_glue("{zone_id}{population_id}"))
  ) %>%
  arrange(cluster_id) %>% 
  group_by(cluster_id,localite_id,localite_name,population_id,population_name) %>% 
  summarise(eff=sum(eff),.groups='drop') %>% 
  select(cluster_id,localite_id,localite_name,population_id,population_name,eff) %>% 
  openxlsx::write.xlsx(file="export_localite_survey.xlsx")


dbase <-  config$data %>% 
  filter(!caseid %in% rejected_list ) %>% 
  mutate(zone_name = region_name) %>% #' [CAS PARTICULIER RMS TCHAD 2025-2026]
  count(zone_name,localite_name,population_name) %>% 
  rename(Bureau=zone_name,Localite=localite_name,Population=population_name,Realises=n) %>% 
  arrange(Bureau,Localite,Population)

db_sample <- config$sample %>% 
  select(Bureau=zone_name,Localite=localite_name,Refugee=nbhh_refugee,Host=nbhh_host,PDI=nbhh_idp) %>% 
  pivot_longer(cols=Refugee:PDI,names_to = "Population",values_to = "Planifies") %>% 
  mutate(
    Population = case_when(
      Population=="Refugee" ~ "Réfugié(e)s/Demandeurs d'asile",
      Population=="Host" ~ "Communautés hôtes",
      Population=="PDI" ~ "PDIs"
    )
  ) %>% 
  arrange(Bureau,Localite,Population)

db <- db_sample %>% filter(Planifies > 0) %>% 
  left_join(
    dbase, 
    by = join_by(Bureau,Localite,Population)
  ) #%>% 
  # mutate(
  #   Etiquette = paste(Realises, "/", Planifies)
  # )

couleurs_hcr <- c(unhcr_pal(1,"pal_blue"), # Bleu HCR
                  unhcr_pal(5,"pal_grey")[2]) # Gris HCR
names(couleurs_hcr) <- c("Planifiés (P)","Réalisés (R)")

# Production du graphique par type de population
# Comparaison des effectifs attendus (plan d"échantillonnage) aux effectifs réalisés
# pendant la collecte
# Dans le PowerPoint : indiquer les taux moyens cumulés de valeurs manquantes,
# de retour à la modalité " autres à préciser", 
# le nombre total de questionnaire ainsi que le taux de rejet
data_long <-  db %>% 
  select(-c(Localite,Bureau)) %>% 
  group_by(Population) %>% 
  summarise(
    Planifies = sum(Planifies),
    Realises = sum(Realises),
    Etiquette = paste( paste0(Realises, " (R)"), "/", paste0(Planifies," (P)")),
    .groups = "drop"
  ) %>% 
  pivot_longer(cols=Planifies:Realises,names_to = "Type_Interviews",values_to = "Questionnaires") %>% 
  mutate(
    Type_Interviews = ifelse(Type_Interviews=="Planifies","Planifiés (P)","Réalisés (R)") 
  )

# Creer le graphique
ggplot(data_long, aes(x = Population, y = Questionnaires, fill = Type_Interviews)) +
  # Ajouter les barres pour les ventes planifiees (en arrière-plan, plus larges)
  geom_bar(data = subset(data_long, Type_Interviews == "Planifiés (P)"),
           stat = "identity", 
           position = "dodge", 
           # fill = couleurs_hcr["Planifies"], 
           alpha = 0.75,
           width = 0.9) +
  # Ajouter les barres pour les ventes effectives (au premier plan, plus etroites)
  geom_bar(data = subset(data_long, Type_Interviews == "Réalisés (R)"),
           aes(fill = Type_Interviews),
           stat = "identity", 
           position = "dodge", 
           # fill = couleurs_hcr["Realises"], 
           alpha = 0.75,
           width = 0.75) +
  # # Ajouter une etiquette unique au-dessus des barres
  geom_label(data = subset(data_long, Type_Interviews == "Planifiés (P)"),
             aes(label = Etiquette),
             position = position_dodge(width = 0.9),
             vjust = -0.5,
             size = 5,
             fill = unhcr_pal(1,"pal_blue"), # Fond bleu HCR pour l'etiquette
             color = "white") + # Texte blanc pour contraste
  # Appliquer les couleurs definies manuellement
  scale_fill_manual(
    name = "Questionnaires",
    values = couleurs_hcr #c("Planifies" = "#0072BC", "Realises" = "#CCCCCC")
  ) +
  # Ajouter des etiquettes et personnaliser l'apparence
  labs(#title = "Comparaison des effectifs attendus (en bleu) et réalisés (en gris) par population",
       # subtitle = "bleu=attendu & réalisés = gris",
       x = "Groupe de populations",
       y = "Nombre de questionnaires") +
  # Appliquer le thème HCR
  theme_minimal() + #theme_unhcr() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
    # legend.position.inside = c(0.1, 1500),
  ) +
  annotate("text", x=0.5,y=2700,label="Nb. questionnaire total = 4366 réalisés / 3986 attendus (110%)",hjust = 0,size=6) +
  annotate("text", x=0.5,y=2625,label="Nb. questionnaires rejectés = 112 (16 Hôtes, 93 Réfugiés)",hjust = 0,size=6) +
  annotate("text", x=0.5,y=2550,label="Nb. questionnaires approuvés = 4254",hjust = 0,size=6) +
  annotate("text", x=0.5,y=2475,label="Taux moyen cumulé de valeurs manquantes = 1% (≤ 10%)",hjust = 0,size=6) +
  annotate("text", x=0.5,y=2400,label="Taux moyen cumulé de recours à la modalité autre à préciser = 3% (≤ 10%)",hjust = 0,size=6)


# Production du graphique par type de population selon la  zone d'enquête (sous-délégation)
# Dns le PowerPoint : indiquer les taux moyens cumulés de valeurs manquantes,
# de retour à la modalité " autres à préciser", 
# le nombre total de questionnaire ainsi que le taux de rejet
# On ajoutera également l'indice de similarité associé à la silhouette des
# données par sous-délégation 

db_long <-  db %>% 
  select(-Localite) %>% 
  group_by(Bureau,Population) %>% 
  summarise(
    Planifies = sum(Planifies),
    Realises = sum(Realises),
    Etiquette = paste( paste0(Realises, " (R)"), "/", paste0(Planifies," (P)")),
    .groups = "drop"
  ) %>%
  pivot_longer(cols=Planifies:Realises,names_to = "Type_Interviews",values_to = "Questionnaires") %>% 
  mutate(
    Type_Interviews = ifelse(Type_Interviews=="Planifies","Planifiés (P)","Réalisés (R)") 
  )

# Création d'un dataframe pour les annotations spécifiques à chaque facet
annotations <- data.frame(
  Population = 0.1,
  Questionnaires = rep(c(2100 - 100*(0:5)),each=4),
  Bureau = rep(c("BO N'DJAMENA","SO ABECHE","SO BAGASOLA","SO GORE"),times=6),
  Etiquette = c(
    # Nombre de questionnaire total
    "Nb. questionnaire total = 200 réalisés / 184 attendus", #BO N'DJAMENA
    "Nb. questionnaire total = 2868 réalisés / 2678 attendus", #SO ABECHE
    "Nb. questionnaire total = 611 réalisés / 576 attendus", #SO BAGASOLA
    "Nb. questionnaire total = 655 réalisés / 552 attendus", #SO GORE,
    # Nombre de questionnaire rejeté
    "Nb. questionnaires rejectés = 1 Réfugié", #BO N'DJAMENA
    "Nb. questionnaires rejectés = 106 (16 Hôtes,90 Réfugiés)", #SO ABECHE
    "Nb. questionnaires rejectés = 1 Réfugié", #SO BAGASOLA
    "Nb. questionnaires rejectés = 4 (3 Hôtes, 1 Réfugié)", #SO GORE
    # Nombre de questionnaire approuvé
    "Nb. questionnaires approuvés = 199", #BO N'DJAMENA
    "Nb. questionnaires approuvés = 2769", #SO ABECHE
    "Nb. questionnaires approuvés = 610", #SO BAGASOLA
    "Nb. questionnaires approuvés = 651", #SO GORE
    # Taux moyen cumulé de valeurs manquantes
    "Taux moyen cumulé de valeurs manquantes = 1.4% (≤ 10%)", #BO N'DJAMENA
    "Taux moyen cumulé de valeurs manquantes = 2.3% (≤ 10%)", #SO ABECHE
    "Taux moyen cumulé de valeurs manquantes = 0.6% (≤ 10%)", #SO BAGASOLA
    "Taux moyen cumulé de valeurs manquantes = 0.8% (≤ 10%)", #SO GORE
    # Taux moyen cumulé de recours  à la modalité autre à préciser
    "Taux de recours à la modalité autre à préciser = 0.8% (≤ 10%)", #BO N'DJAMENA
    "Taux de recours à la modalité autre à préciser = 1.1% (≤ 10%)", #SO ABECHE
    "Taux de recours à la modalité autre à préciser = 1.8% (≤ 10%)", #SO BAGASOLA
    "Taux de recours à la modalité autre à préciser = 0.2% (≤ 10%)", #SO GORE
    # Silhouette : Indice moyen de similarité
    "Indice moyen de similarité = 0.22 (≤ 0.8)", #BO N'DJAMENA
    "Indice moyen de similarité = 0.28 (≤ 0.8)", #SO ABECHE
    "Indice moyen de similarité = 0.26 (≤ 0.8)", #SO BAGASOLA
    "Indice moyen de similarité = 0.23 (≤ 0.8)" #SO GORE
  ),
  # couleur = c(rep("black",times=20),rep("red",times=4)),
  Type_Interviews="Planifiés (P)"
)

# Creer le graphique
ggplot(db_long, aes(x = Population, y = Questionnaires, fill = Type_Interviews)) +
  # Ajouter les barres pour les ventes planifiees (en arrière-plan, plus larges)
  geom_bar(data = subset(db_long, Type_Interviews == "Planifiés (P)"),
           aes(group = Bureau), 
           stat = "identity", 
           position = "dodge", 
           alpha = 0.75,
           width = 0.9) +
  # Ajouter les barres pour les ventes effectives (au premier plan, plus etroites)
  geom_bar(data = subset(db_long, Type_Interviews == "Réalisés (R)"),
           aes(group = Bureau), 
           stat = "identity", 
           position = "dodge", 
           alpha = 0.75,
           width = 0.75) +
  # # Ajouter une etiquette unique au-dessus des barres
  geom_label(data = subset(db_long, Type_Interviews == "Planifiés (P)"),
             aes(label = Etiquette, group = Bureau),
             position = position_dodge(width = 0.9),
             vjust = -0.5,
             size = 5,
             fill = unhcr_pal(1,"pal_blue"), # Fond bleu HCR pour l'etiquette
             color = "white") + # Texte blanc pour contraste
  # Appliquer les couleurs definies manuellement
  scale_fill_manual(
    name = "Questionnaires",
    values = couleurs_hcr #c("Planifiés (P)" = "#0072BC", "Réalisés (R)" = "#CCCCCC") #
  ) +
  # Ajouter des etiquettes et personnaliser l'apparence
  labs(#title = "Comparaison des effectifs attendus et réalisés par population et Bureau",
       x = "Population",
       y = "Nombre de questionnaires") +
  # # Appliquer les couleurs definies manuellement
  # scale_fill_manual(values = as.character(couleurs_hcr)) +
  # Appliquer le thème HCR
  # theme_unhcr() + # theme_minimal() +
  ## Appliquer le thème HCR
  theme_unhcr() + #theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x= element_text(size = 14),
    axis.text.y= element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    strip.text = element_text(size = 14)
  ) +
  # Facetter par region (un graphique par region)
  facet_wrap(~ Bureau)  + # Un graphique par region 
  geom_text(data=annotations,aes(label=Etiquette), #,colour = couleur
            hjust = 0,size=5) +
  guides(colour = "none")

# Nettoyage de l'environnement...
rm(db,db_long,data_long,annotations, db_sample,dbase, couleurs_hcr, rejected_list)
