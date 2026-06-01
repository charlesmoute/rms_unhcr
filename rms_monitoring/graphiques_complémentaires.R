
ind <- ind_weighted
main <- main_weighted
rms_pvalues <-  rms_pvalues_weighted
combined_RBM_indicators <- combined_RBM_indicators_weighted


# Graphique complémentaire OUTCOME 1.3  ----
#* [OUTCOME 1.3] 

identity_document_percentages_v2 <- RMS_XXX_202X_ind %>%
  group_by(pop_groups) %>%
  summarise(across(c(REG01a, REG01b, REG01c, REG01d, REG01e, REG01f, REG01g, REG02),
                   ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = REG01a:REG02,#everything(), 
               names_to = "Document", 
               values_to = "Percentage") %>%
  mutate(Document = identity_documents[Document])


# Create the bar chart
outcome1_3_graph03_v2 <- ggplot(identity_document_percentages_v2 %>% filter(!is.na(Percentage)), 
                             aes(x = reorder(Document, Percentage), y = Percentage, fill = Document)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),color = "orangered", #, #orangered
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
  labs(
    title = "Pourcentage de personnes détenant des documents d'identité",
    x = "Document d'identité",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Les pourcentages sont calculés indépendamment pour chaque document pour les individus de 5 ans et plus.")
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )


# Export des graphiques
suppressWarnings(ggsave(
  filename="local/database/graphics/new_outcome1_3_by_population.png", 
  plot = outcome1_3_graph03_v2, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_outcome1_3_by_population.png généré ..."
),"\n")


#****************************************************************************************************************************************
# Graphique complémentaire OUTCOME 1.3  ----
#* [OUTCOME 9.2] 

# Summarize the counts and percentages for each category
light02_percentages_v2 <- main_weighted %>%
  ungroup() %>% 
  filter(!is.na(LIGHT02)) %>%  # Exclude missing values
  count(pop_groups,LIGHT02, wt=UNHCR_WEIGHT) %>%
  mutate(LIGHT02 = factor(LIGHT02, levels = names(lighting_labels), labels = lighting_labels)) %>% 
  ungroup() %>% group_by(pop_groups) %>% 
  mutate(total = sum(n), Percentage = n / sum(n) * 100) #%>%
  # summarise(across(c(n),~ mean(. == "1", na.rm = TRUE) * 100))
  

# Create the chart
outcome9_2_graph03_v2 <- ggplot(light02_percentages_v2, aes(x = reorder(LIGHT02, Percentage), y = Percentage, fill = LIGHT02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), color="orangered", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
  labs(
    title = "Distribution de l'énergie d'éclairage (LIGHT02) par groupe de population",
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
# Graphique complémentaire OUTCOME 8.2  ----
#* [OUTCOME 8.2] 

cook02_percentages_v2 <- main_weighted %>%
  ungroup() %>% 
  filter(!is.na(COOK02)) %>%  # Exclude missing values
  count(pop_groups,COOK02,wt=UNHCR_WEIGHT) %>%
  ungroup() %>% group_by(pop_groups) %>% 
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(COOK02 = factor(COOK02, levels = names(stove_labels), labels = stove_labels))

# Create the chart
outcome8_2_graph03_v2 <- ggplot(cook02_percentages_v2, aes(x = reorder(COOK02, Percentage), y = Percentage, fill = COOK02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), color="orangered", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
  labs(
    title = "Répartition des types de fourneaux par groupe de population",
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
# Graphique complémentaire outcome 16.2  ----
#* [OUTCOME 16.2] 

spf01_percentages_v2 <- main %>%
  summarise(across(c(SPF01a, SPF01b, SPF01c, SPF01d, SPF01e, SPF01f, SPF01g, SPF01h, 
                     SPF01j, SPF01k, SPF01l, SPF01m, SPF01n, SPF01o, SPF01p),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   # ~ mean(. == 1, na.rm = TRUE) * 100)) %>% # non prise en compte de la pondération..
                   ~ weighted.mean(. == 1, w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = SPF01a:SPF01p,  # Exclude the PopGroup column from pivoting
               names_to = "Service", 
               values_to = "Percentage") %>%
  mutate(Service = spf01_mapping[Service])  # Map column names to descriptive labels


# Step 3: Create the bar chart
outcome16_2_graph03_v2 <- ggplot(spf01_percentages_v2, aes(x = reorder(Service, Percentage), y = Percentage, fill = Service)) +
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
  )

# Export des graphiques
suppressWarnings(ggsave(
  filename="local/database/graphics/new_outcome16_2.png", 
  plot = outcome16_2_graph03_v2, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_outcome16_2.png généré ..."
),"\n")


#****************************************************************************************************************************************
# Graphique complémentaire IMPACT 2.3  ----
#* [IMPACT 2.3] 

hacc_percentages_v2 <- ind %>%
  group_by(pop_groups) %>% 
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
  pivot_longer(cols = HACC04_1:HACC04_13,#everything(), 
               names_to = "Reason", 
               values_to = "Percentage") %>%
  mutate(Reason = reasons_mapping[Reason])  # Map column names to descriptive labels


##Chart creation
impact2_3_graph03_v2 <- ggplot(hacc_percentages_v2, aes(x = reorder(Reason, Percentage), y = Percentage, fill = Reason)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
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
# Graphique complémentaire outcome 13.2  ----
#* [OUTCOME 13.2] 

###INC02
# Define stove categories based on the provided list
inc03_labels <- c(
  "1" = "Moins de 35 000FCFA",
  "2" = "Entre 35 000 FCFA et 75 000 FCFA",
  "3" = "Entre 75 000 FCFA et 100 000 FCFA",
  "4" = "Entre 100 000 et 125 000 FCFA",
  "5" = "125 000 FCFA ou plus"
)

# Summarize the counts and percentages for each category
INC03_percentages <- main %>%
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
  filter(!is.na(INC03)) %>%  # Exclude missing values
  ungroup() %>% select(-c(cluster_id,cluster_name)) %>% 
  count(INC03, wt=UNHCR_WEIGHT) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC03 = factor(INC03, levels = names(inc03_labels), labels = inc03_labels))


# Create the chart
outcome13_2_graph05 <- ggplot(INC03_percentages, aes(x = reorder(INC03, Percentage), y = Percentage, fill = INC03)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), color="black",#color="orangered", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  labs(
    title = "Revenu mensuel en FCFA (INC03)",
    x = "Revenu mensuel",
    y = "Pourcentage",
    caption = unhcr_caption
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 10),  # Adjust text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

# Export des graphiques
suppressWarnings(ggsave(
  filename="local/database/graphics/new_outcome13_2.png", 
  plot = outcome13_2_graph05, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_outcome13_2.png généré ..."
),"\n")


## 1. INC03 par type de population ----
#* [INC03 - TYPE DE POPULATION] 

# Summarize the counts and percentages for each category
INC03_percentages_v2 <- main %>%
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
  filter(!is.na(INC03)) %>%  # Exclude missing values
  ungroup() %>% select(-c(cluster_id,cluster_name)) %>% 
  count(pop_groups,INC03, wt=UNHCR_WEIGHT) %>%
  ungroup() %>% group_by(pop_groups) %>% 
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC03 = factor(INC03, levels = names(inc03_labels), labels = inc03_labels))


# Create the chart
outcome13_2_graph05_v2 <- ggplot(INC03_percentages_v2, aes(x = reorder(INC03, Percentage), y = Percentage, fill = INC03)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), color="orangered",#color="orangered", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
  labs(
    title = "Revenu mensuel en FCFA (INC03)",
    x = "Revenu mensuel",
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

## 2. INC02 par type de population ----
#* [INC02 - TYPE DE POPULATION]

# Summarize the counts and percentages for each category
INC02_percentages_v2 <- main %>%
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
  count(pop_groups,INC02, wt=UNHCR_WEIGHT) %>%
  ungroup() %>% group_by(pop_groups) %>% 
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC02 = factor(INC02, levels = names(inc02_labels), labels = inc02_labels))


# Create the chart
outcome13_2_graph04_v2 <- ggplot(INC02_percentages_v2, aes(x = reorder(INC02, Percentage), y = Percentage, fill = INC02)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), color="orangered", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
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

## 3. INC01 par type de population ----
#* [INC01 - TYPE DE POPULATION]
#* 
INC01_percentages_v2 <- main %>%
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
  count(pop_groups,INC01, wt=UNHCR_WEIGHT) %>%
  ungroup() %>% group_by(pop_groups) %>% 
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(INC01 = factor(INC01, levels = names(inc01_labels), labels = inc01_labels))

# Create the chart
outcome13_2_graph03_v2 <- ggplot(INC01_percentages_v2, aes(x = reorder(INC01, Percentage), y = Percentage, fill = INC01)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), color="orangered", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels
  coord_flip() +  # Flip the chart for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
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

## 4. Evoluation des revenus (INC01 & INC02 & INCO03) ----
# income_percentages <- main %>%
#   mutate(
#     HH07_cat = p_HH07_cat,
#     HH07_cat1 = p_HH07_cat1,
#     HH07_cat2 = p_HH07_cat2,
#     disability = p_disability,
#     citizenship = p_citizenship,
#     idp_valid = p_idp_valid,
#     HH07 = p_HH07,HH04 = p_HH04,
#     HH03 = p_HH03,
#     unhcr_name_individual =p_unhcr_name_individual,
#     disability_visual = p_disability_visual,
#     disability_hearing = p_disability_hearing,
#     disability_mobility = p_disability_mobility,
#     disability_intelpsych = p_disability_intelpsych,
#     HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
#     ind_age =p_ind_age,
#     INCO1_oag = as.numeric(
#       if_else(parse_number(INC01)==98, NA, parse_number(INC01) == 1)
#     ),
#     INCO2_oag =  as.numeric(
#       if_else(parse_number(INC02)==98, NA, parse_number(INC02) == 1)
#     ),
#     INCO3_oag = as.numeric(labelled_chr2dbl(INC03) == 1)
#   ) %>%
#   # Filtrer les valeurs manquantes et les réponses "98" pour les deux variables
#   filter((!is.na(INC01) & INC01 != '98') | (!is.na(INC02) & INC02 != '98')) %>%
#   
#   select(INCO1_oag,INCO2_oag,INCO3_oag,UNHCR_WEIGHT) %>% 
#   # select(pop_groups, INCO1_oag,INCO2_oag,INCO3_oag,UNHCR_WEIGHT) %>% 
#   pivot_longer(cols = INCO1_oag:INCO3_oag, 
#                names_to = "access", 
#                values_to = "conditions") %>%
#   group_by(access) %>%
#   # group_by(pop_groups, access) %>%
#   summarise(percentage = weighted.mean(conditions, wt=UNHCR_WEIGHT, na.rm = TRUE) * 100,.groups = "drop")
# 

income_percentages <- main %>%
  mutate(
    HH07_cat = p_HH07_cat,
    HH07_cat1 = p_HH07_cat1,
    HH07_cat2 = p_HH07_cat2,
    disability = p_disability,
    citizenship = p_citizenship,
    idp_valid = p_idp_valid,
    HH07 = p_HH07, HH04 = p_HH04,
    HH03 = p_HH03,
    unhcr_name_individual = p_unhcr_name_individual,
    disability_visual = p_disability_visual,
    disability_hearing = p_disability_hearing,
    disability_mobility = p_disability_mobility,
    disability_intelpsych = p_disability_intelpsych,
    HH01 = p_HH01, ind_name = p_ind_name, ind_gender = p_ind_gender,
    ind_age = p_ind_age
  ) %>%
  # Filtrer les valeurs manquantes et les réponses "98" pour les deux variables
  filter((!is.na(INC01) & INC01 != '98') | (!is.na(INC02) & INC02 != '98')) %>%
  # Sélectionner les colonnes nécessaires
  select(pop_groups,INC01, INC02, INC03, UNHCR_WEIGHT,pop_groups) %>%
  mutate(across(INC01:INC03,parse_number)) %>% 
  # Pivoter les données pour avoir une ligne par variable (INC01 ou INC02)
  pivot_longer(
    cols = c(INC01,INC02,INC03), #,INC03
    names_to = "access",
    values_to = "response"#,
    # values_drop_na = TRUE
  ) %>%
  # Filtrer les valeurs manquantes après le pivot
  filter(!is.na(response) & response != 98) %>%
  # Grouper par variable et réponse
  group_by(pop_groups,access, response) %>%
  # Calculer les sommes pondérées
  summarise(n = sum(UNHCR_WEIGHT), .groups = "drop_last") %>%
  # Calculer les pourcentages pour chaque variable
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  # Convertir les réponses en facteurs avec les bons libellés
  # mutate(
  #   response = case_when(
  #     access == "INC01" ~ factor(response, levels = names(inc01_labels), labels = inc01_labels),
  #     access == "INC02" ~ factor(response, levels = names(inc02_labels), labels = inc02_labels),
  #     TRUE ~ factor(response)
  #   )
  # )
  # # On ne conserve aue les valeurs d'intérêt pour le graphe
  filter(response==1) %>%
  # On supprime les variables inutles
  select(-c(response,n))


###Chart for above with all dimensions 
outcome13_2_graph06 <- ggplot(income_percentages, aes(x = access, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Correct stat to "identity"
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "INC01" = "Revenu a augmenté par rapport\nà l'année précédente",
    "INC02" = "Impession de pouvoir s'offir\nplus de choses que l'année\nprécédente",
    "INC03" = "Revenu mensuel en dessous\nde 35.000 FCFA"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Evolution des revenus (INC01 & INC02 & INCO03)",
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

suppressWarnings(ggsave(
  filename="local/database/graphics/new_outcome13_2b.png", 
  plot = outcome13_2_graph06, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_outcome13_2b.png généré ..."
),"\n")

outcome13_2_graph06b <-  income_percentages %>% 
  filter(!access %in% c('INC03')) %>% 
  ggplot(aes(x = access, y = percentage, fill = pop_groups)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +  # Correct stat to "identity"
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_dodge(0.7), vjust = -0.5, size = 3.5) +  # Add percentage labels on bars
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  scale_x_discrete(labels = c(
    "INC01" = "Revenu a augmenté par rapport\nà l'année précédente",
    "INC02" = "Impession de pouvoir s'offir\nplus de choses que l'année\nprécédente"
  )) +  # Add descriptive labels to the x-axis
  labs(
    title = "Evolution des revenus (INC01 & INC02)",
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
  )

suppressWarnings(ggsave(
  filename="local/database/graphics/new_outcome13_2c.png", 
  plot = outcome13_2_graph06b, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_outcome13_2c.png généré ..."
),"\n")

#****************************************************************************************************************************************
# Graphique complémentaire OUTCOME 4.1  ----
#* [OUTCOME 4.1] 

RMS_XXX_202X_main_o41 <- main_weighted %>% 
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

# Calculate the percentage of '1's for each identity document
gbv01_percentages_v2 <- RMS_XXX_202X_main_o41 %>%
  ungroup() %>% group_by(pop_groups) %>% 
  summarise(across(c(GBV01a, GBV01b, GBV01c, GBV01d),#,GBV01e,GBV01f,GBV01g),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups="drop") %>% 
  pivot_longer(cols = -pop_groups, #everything(), 
               names_to = "Services", 
               values_to = "Percentage") %>%
  mutate(Services = gbv_services[Services])   # Map column names to descriptive labels

# Create the bar chart
outcome4_1_graph03b <- ggplot(gbv01_percentages_v2, aes(x = reorder(Services, Percentage), y = Percentage, fill = Services)) +
  geom_bar(stat = "identity", width = 0.7) +  # Use "identity" for stat
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = "white", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels inside bars
  coord_flip() +  # Flip the axes for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
  labs(
    title = "Connaissance des lieux d'accès aux services de lutte contre la violence liée au sexe",
    x = "Services de lutte contre la violence sexiste",
    y = "Pourcentage",
    caption = paste(unhcr_caption,
                    "Note : Les pourcentages sont calculés indépendamment pour chaque service pour les personnes de 18 ans et plus.")
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust y-axis text size for readability
    legend.position = "none"  # Remove legend for simplicity
  )

#****************************************************************************************************************************************
# Graphique complémentaire OUTCOME 4.2  ----
#* [OUTCOME 4.2] 

vaw01_percentages_v2 <- RMS_XXX_202X_main %>% #main %>%
  ungroup() %>% group_by(pop_groups) %>% 
  summarise(across(c(VAW01a, VAW01b, VAW01c, VAW01d, VAW01e),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups="drop") %>% 
  pivot_longer(cols = -pop_groups, #everything(), 
               names_to = "Question", 
               values_to = "Percentage") %>%
  mutate(Question = vaw_options[Question])  # Map column names to descriptive labels

###Chart
outcome4_2_graph03b <- ggplot(vaw01_percentages_v2, aes(x = reorder(Question, Percentage), y = Percentage, fill = Question)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = "white", 
            position = position_stack(vjust = 0.5), size = 3.5) +  # Add percentage labels on bars
  coord_flip() +  # Flip the axes for better readability
  facet_wrap(~ pop_groups, scales = "free_x") +
  labs(
    title = "Justification de la violence à l'égard des femmes",
    x = "Justification",
    y = "Pourcentage",
    caption = paste(unhcr_caption,
                    "Note : Les pourcentages représentent la proportion de personnes interrogées qui sont d'accord avec la justification.")
  ) +
  scale_fill_unhcr_d() +  # Apply UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )


#****************************************************************************************************************************************
# Graphique complémentaire USER 2.2 ----
#* [USER 2.2]

## Graph01 ---- 
diff_percentages_v2 <- main %>%
  # filter(pop_groups %in% c(var_REFUGEES,var_IDPs,var_RETURNED)) %>%
  summarise(across(c(BQ901c_1, BQ901c_2, BQ901c_3, BQ901c_4, BQ901c_5, 
                     BQ901c_6), 
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = everything(),  # Exclude the PopGroup column from pivoting
               names_to = "Difficulties", 
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>% 
  mutate(Difficulties = mgp_diff[Difficulties])  # Map column names to descriptive labels


# Create the bar chart
user2_2_graph03b <- ggplot(diff_percentages_v2, aes(x = reorder(Difficulties, Percentage), y = Percentage, fill = Difficulties)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),colour = 'white', 
            position = position_stack(vjust = 0.5), size = 3.5) +
  # position = position_stack(vjust = 1.05), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Proportion de personnes par difficultés rencontrées lors de l'utilisation d'un MGP",
    x = "Difficultés rencontrées",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Chaque difficulté est mesurée indépendamment.")
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

suppressWarnings(ggsave(
  filename="local/database/graphics/new_user2_2.png", 
  plot = user2_2_graph03b, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_user2_2.png généré ..."
),"\n")

## Graphe 02 ----
mgp_nouse_percentages_v2 <- main %>%
  summarise(across(c(BQ902b_2, BQ902b_3, BQ902b_4, BQ902b_5, BQ902b_6, BQ902b_7),
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = everything(),  # Exclude the PopGroup column from pivoting
               names_to = "Reasons", 
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>% 
  mutate(Reasons = mgp_nouse[Reasons])  # Map column names to descriptive labels

# Step 3: Create the bar chart
user2_2_graph04b <- ggplot(mgp_nouse_percentages_v2, aes(x = reorder(Reasons, Percentage), y = Percentage, fill = Reasons)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),,colour = 'white', 
            position = position_stack(vjust = 0.5), size = 3.5) +#colour = 'orangered'), 
            # position = position_stack(vjust = 1.09), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Proportion de personnes par motifs pour la non utilisation d'un MGP",
    x = "Motifs",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Chaque raison est mesuré indépendamment.")
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

suppressWarnings(ggsave(
  filename="local/database/graphics/new_user2_2b.png", 
  plot = user2_2_graph04b, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_user2_2b.png généré ..."
),"\n")

#****************************************************************************************************************************************
# Graphique complémentaire USER 1.1  ----
#* [USER 1.1] 
#* 

## Premier graphique sans groups par type de population 

retour_reasons <- c(
  "RET03a" = "Meilleure qualité de vie globale",
  "RET03b" = "Amélioration du bien-être\nde la famille",
  "RET03c" = "Meilleurs conditions de logement",
  "RET03d" = "Plus en sécurité",
  "RET03e" = "Meilleur emploi ou des moyens\nde gagner de l'argent",
  "RET03f" = "Meilleure situation financière",
  "RET03g" = "Accès à de meilleurs soins de santé",
  "RET03h" = "Meilleur accès à la nourriture",
  "RET03i" = "Accès à de meilleures écoles",
  # "RET03j" = "Meilleure qualité de vie globale", #Redondance probable une erreur lors de la traduction
  "RET03k" = "Rapprochement avec la famille\nd'origine",
  "RET03l" = "Meilleures relations avec les\ngens de votre localité",
  "RET03m" = "Se sentira plus heureux "
)


ret_percentages <- main %>%
  summarise(across(c(RET03a:RET03i,RET03k:RET03m), #RET03j exclu
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = everything(),  # Exclude the PopGroup column from pivoting
               names_to = "Reasons", 
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>% 
  mutate(Reasons = retour_reasons[Reasons])  # Map column names to descriptive labels

# Step 3: Create the bar chart
user1_graph01b <- ggplot(ret_percentages, aes(x = reorder(Reasons, Percentage), y = Percentage, fill = Reasons)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),,colour = 'white', 
            position = position_stack(vjust = 0.5), size = 3.5) +#colour = 'orangered'), 
  # position = position_stack(vjust = 1.09), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Raisons majeurs de rentrer dans le lieu d'origine",
    x = "Motifs",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Chaque raison est mesurée indépendamment.")
  ) +
  scale_fill_unhcr_d() +  # Use UNHCR color palette
  theme_unhcr() +  # Apply UNHCR theme
  # Customize theme elements
  theme(
    axis.text.y = element_text(size = 9),  # Adjust text size for readability
    legend.position = "none"  # Remove the legend for simplicity
  )

suppressWarnings(ggsave(
  filename="local/database/graphics/new_user1_1.png", 
  plot = user1_graph01b, 
  # width = 8, height = 7
  # width = 12, height = 6
  width = 10, height = 6
))

cat(glue::glue_col(
  "{green ✔} Graphique new_user1_1.png généré ..."
),"\n")


## Graphique 03 par type de population----
ret_percentages_v2 <- main %>%
  group_by(pop_groups) %>%
  summarise(across(c(RET03a:RET03i,RET03k:RET03m), #RET03j exclu
                   # ~ mean(. == "1", na.rm = TRUE) * 100)) %>%  # Ensure character comparison
                   ~ weighted.mean(. == "1", w = UNHCR_WEIGHT, na.rm = TRUE) * 100),
            .groups = "drop") %>% 
  pivot_longer(cols = -pop_groups,#everything(),  # Exclude the PopGroup column from pivoting
               names_to = "Reasons", 
               values_to = "Percentage") %>%
  filter(!is.na(Percentage)) %>% 
  mutate(Reasons = retour_reasons[Reasons])  # Map column names to descriptive labels

# Step 3: Create the bar chart
user1_graph01c <- ggplot(ret_percentages_v2, aes(x = reorder(Reasons, Percentage), y = Percentage, fill = Reasons)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  # Add percentage labels on bars
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),,colour = 'white', 
            position = position_stack(vjust = 0.5), size = 3.5) +#colour = 'orangered'), 
  # position = position_stack(vjust = 1.09), size = 3.5) +
  coord_flip() +  # Flip the axes for better readability
  # Labels and title
  labs(
    title = "Raisons majeurs de rentrer dans le lieu d'origine",
    x = "Motifs",
    y = "Pourcentage",
    caption = paste(unhcr_caption,"Note : Chaque raison est mesurée indépendamment.")
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
# Graphique complémentaire xxxx  ----
#* [xxx] 
#* 

# # Export des graphiques
# suppressWarnings(ggsave(
#   filename="rplot_250822_1420.png", 
#   plot = outcome1_3_graph03_v2, 
#   # width = 8, height = 7
#   # width = 12, height = 6
#   width = 10, height = 6
# ))


