# Quelques variables globales
# percent_value <- 100
unhcr_caption<- "HCR - RMS Tchad 2025."
var_HOST <- "Communautés d'accueil"
var_IDPs <- "PDI"
var_REFUGEES <- "Réfugiés/Demandeurs d'asile"
var_RETURNED <- "Retournés"
var_population <- c(var_REFUGEES,var_IDPs,var_HOST)

# Pour estimer correctement la variance dans les enquêtes complexes, il faut au moins deux PSU par strate. 
# Si une strate ne contient qu’un seul PSU, la fonction ne peut pas calculer la variance et renvoie cette erreur.

# Le package survey/srvyr propose plusieurs options pour gérer ce cas :
# • "adjust" : ajuste la variance en recentrant la PSU unique sur la moyenne globale.
# • "average" : utilise la moyenne des variances des autres strates.
# • "certainty" : considère la PSU unique comme une strate certaine (variance nulle).
# • "remove" : supprime ces unités uniques de l’analyse.
options(survey.lonely.psu = "adjust") #Nous prenons le cas de adjust

suppressMessages(
  rms_infos <- rio::import(file.path("local","input","cleaning_instructions.xlsx"),
                           sheet="indicators") %>% 
    select(indicator_code,everything()) %>% 
    filter(!indicator_source %in% c("Revue documentaire")) %>% 
    select(-indicator_source) %>% 
    type_convert()
)

# Quelques fonctions utilitaires

#* [Fonctions utilitaires]
github_raw <- function(x) paste0("https://raw.githubusercontent.com/", x)

#* [Create function that turn character values into numeric if you imported your ind from Kobo]
labelled_chr2dbl <- function(x) {
  varlab <- labelled::var_label(x)
  vallab <- labelled::val_labels(x)
  vallab <- setNames(as.numeric(vallab),names(vallab))
  x <- as.numeric(as.character(x))
  var_label(x) <- varlab
  val_labels(x) <- vallab
  x
}

#* [Obtention des p-value des tables RMS] 
get_pvalues <- function(database,var_indicator,var_expl,var_table_name,
                        numerator=NULL,denominator=NULL){
  
  # #  On s'assure d'abord que la modalité Homme et bien la réfénce
  # if('HH04' %in% names(database)){
  #   #  Modification de la référence .. on évite de changer depuis le début
  #   # dans data_preparation afin d'éviter des incohérences
  #   database <- database %>% mutate(HH04 = relevel(HH04, ref = "Homme"))
  # }
  
  # Initialisation du nom de la table au nom de l'indicateur si le nom de la
  # table est manquant lors de l'appel
  if(missing(var_table_name) || is.null(var_table_name)) var_table_name <- var_indicator
  
  if(is.null(numerator) | is.null(denominator)){
    # Estimation des p-values par modèle de régresion logistique
    suppressWarnings(rms_model <- survey::svyglm(
      formula = as.formula(str_glue("{var_indicator} ~ {var_expl}")),#!!sym(var_indicator) ~ !!sym(var_expl),
      family = quasibinomial(),
      design = database
    ))
    # Extraction de toutes les p-values de toutes les combinaisons des tests statistiques
    subtmp <- suppressWarnings(emmeans::emmeans(
      rms_model, 
      specs = as.formula(str_glue("pairwise ~ {var_expl}")),#pairwise ~ !!sym(var_expl),
      adjust = "tukey",
      data=database))
    
  }else{
    # Cette section a trait au calcul des p-value dans d'un ratio
    database <-  database %>% 
      mutate(
        ratio_ind = ifelse(!!sym(denominator) == 0, NA, !!sym(numerator)/!!sym(denominator))
      )
    # Estimation des p-values par modèle de régresion logistique
    suppressWarnings(rms_model <- survey::svyglm(
      as.formula(str_glue("ratio_ind ~ {var_expl}")), #ratio_ind ~ !!sym(var_expl),
      design = database,
      family = quasibinomial()  # Ou quasibinomial() si ratio [0,1] dabs le cas contraire  gaussian()
    ))
    # Extraction de toutes les p-values de toutes les combinaisons des tests statistiques
    subtmp <-  suppressWarnings(emmeans::emmeans(
      rms_model, 
      specs = as.formula(str_glue("pairwise ~ {var_expl}")), #pairwise ~ pop_groups, 
      adjust = "tukey",
      data=database %>%  filter(!is.na(ratio_ind))
    ))
  }
  
  #  Resultats a retourner et à exploiter
  indicator_pvalues <-  broom::tidy(subtmp$contrasts) %>% 
    mutate(indicator=var_indicator,odds_ratio = exp(estimate))
    # La staistique peut-être un t_student ou z
  
  if('adj.p.value' %in% names(indicator_pvalues))
    indicator_pvalues <- indicator_pvalues %>% rename(`p.value`=`adj.p.value`)
    
  indicator_pvalues <-   indicator_pvalues %>% 
    select(indicator,variable=term,disaggregation=contrast,
           estimate,str_error=std.error,statistic,
           odds_ratio,pvalue=p.value) %>% 
    mutate(term=disaggregation) %>% 
    tidyr::separate(term,into = c("group_1","group_2") , sep = " - ") %>% 
    mutate(
      notes = as.character(str_glue(
        "Le groupe 1 [{group_1}] est comparé au groupe 2 [{group_2}]. Le groupe 1 aurait {round(odds_ratio,2)} fois {ifelse(estimate>=0,'plus','moins')} de chances/risques de vivre {indicator} que le groupe 2. Cette différence est{ifelse(pvalue<0.05,'',' non')} statistiquement significative au seuil de 5% (pvalue={round(pvalue,3)})."
      ))
    ) %>% select(-c(group_1,group_2))
  
  # Affiche des p-values...
  # format(indicator_pvalues$pvalue,scientific = F)
  
  # Export de la table des résultats dans l'environnement de travail global
  assign(
    x=as.character(str_glue("{var_table_name}_pvalues")),
    value = indicator_pvalues,
    envir = .GlobalEnv
  )
  # Retour de la valeur
  invisible(indicator_pvalues)
}

#* agd = [disability_age,disability_gender,gender_age,disability_gender_age]
get_rmsTable <- function(database, indicator, agd=NULL, get_ratio=FALSE,
                           var_numerator=NULL, var_denominator=NULL){
  
  if(is.null(database) | is.null(indicator)){
    cat(glue::glue_col(
      "{red ✖} Erreur lors de l'exécution du programme pour {indicator}..." 
    ),"\n")
    invisible(1)
  }
  
  if(get_ratio & (is.null(var_numerator) | is.null(var_denominator))){
    cat(glue::glue_col(
      "{red ✖} Erreur lors de l'exécution du programme pour {indicator} & get_ratio ..." 
    ),"\n")
    invisible(1) 
  }
  
  ##Table with all data
  if(get_ratio){
    
    indicator_total <- database %>%
      filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
      ungroup() %>% 
      summarise(                                         # put all variables here
        total = "aucun",
        var_name = indicator,                            # name of the variable
        num_obs_uw = unweighted(sum(!!sym(var_denominator),na.rm=TRUE)),      # Unweighted total count
        denominator = survey_total(!!sym(var_denominator), na.rm = TRUE), 
        mean_value = survey_ratio(
          numerator = !!sym(var_numerator),
          denominator = !!sym(var_denominator),
          vartype = c("ci", "se"), na.rm = TRUE
        )
      )
  }else{
    
    indicator_total <- database %>%
      filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
      ungroup() %>% 
      summarise(                                         # put all variables here
        total = "aucun",
        var_name = indicator,                            # name of the variable
        num_obs_uw = unweighted(n()),                    # unweighted total count
        denominator = survey_total(),                    # weighted total count
        mean_value = survey_mean(!!sym(indicator), vartype = c("ci", "se"),na.rm = TRUE) # indicator value ( weighted) with CI and SE
      )
  }
  assign(as.character(str_glue("{indicator}_total")),indicator_total,envir = .GlobalEnv)
  
  ###Table by population groups
  if(get_ratio){
    indicator_table <- database %>%
      filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
      group_by(pop_groups) %>%                           # Group by pop_groups
      summarise(                                         # put all variables here
        var_name = indicator,                            # name of the variable
        num_obs_uw = unweighted(sum(!!sym(var_denominator),na.rm=TRUE)),      # Unweighted total count
        denominator = survey_total(!!sym(var_denominator), na.rm = TRUE), 
        mean_value = survey_ratio(
          numerator = !!sym(var_numerator),
          denominator = !!sym(var_denominator),
          vartype = c("ci", "se"), na.rm = TRUE
        )
      )
  }else{
    
    indicator_table <- database %>%
      filter(!is.na(pop_groups)) %>%                     # Exclude if pop groups is NA
      group_by(pop_groups) %>%                           # Group by pop_groups
      summarise(                                         # put all variables here
        var_name = indicator,                            # name of the variable
        num_obs_uw = unweighted(n()),                    # unweighted total count
        denominator = survey_total(),                    # weighted total count
        mean_value = survey_mean(!!sym(indicator), vartype = c("ci", "se"),na.rm = TRUE) # indicator value ( weighted) with CI and SE
      )
  }
  
  assign(as.character(str_glue("{indicator}")),indicator_table,envir = .GlobalEnv)
  if(nrow(indicator_table)>1){
    #* [Calcul des p-values de l'indicateur par rapport ]
    get_pvalues(database,
                var_indicator=indicator,
                var_expl="pop_groups",
                var_table_name=indicator,
                numerator=var_numerator,
                denominator=var_denominator)
  }
 
  
  if(!is.null(agd)){
    
    dbtmp <- switch( agd,
      disability_age = database %>%
        filter(!is.na(disability) & !is.na(HH07_cat)) %>%
        mutate(cat_agd = fct_cross(disability,HH07_cat, sep = " x ")) %>% 
        group_by(disability,HH07_cat),
      disability_gender = database %>%
        filter(!is.na(disability) & !is.na(HH04)) %>%
        mutate(cat_agd = fct_cross(disability,HH04, sep = " x ")) %>%
        group_by(disability,HH04),
      gender_age = database %>%
        filter(!is.na(HH04) & !is.na(HH07_cat)) %>%
        mutate(cat_agd = fct_cross(HH04,HH07_cat, sep = " x ")) %>%
        group_by(HH04,HH07_cat),
      disability_gender_age = database %>%
        filter(!is.na(disability) & !is.na(HH04) & !is.na(HH07_cat)) %>%
        mutate(cat_agd = fct_cross(disability,HH04,HH07_cat, sep = " x ")) %>%
        group_by(disability,HH04,HH07_cat)
    )
    
    # Renomme la variable catégorielle
    dbtmp <- dbtmp %>% rename(!!sym(agd):=cat_agd)
    if(nrow(dbtmp)>0){
      indicator_name <- as.character(str_glue("{indicator}_AGD"))
      if(get_ratio){
        indicator_agd <- dbtmp %>%            # Group by Age, Gender, and Disability
          summarise(                                       # Summarise to compute values
            var_name = indicator_name,                        # Name of the variable
            num_obs_uw = unweighted(sum(!!sym(var_denominator),na.rm=TRUE)),      # Unweighted total count
            denominator = survey_total(!!sym(var_denominator), na.rm = TRUE), 
            mean_value = survey_ratio(
              numerator = !!sym(var_numerator),
              denominator = !!sym(var_denominator),
              vartype = c("ci", "se"), na.rm = TRUE
            )
          ) %>% filter(denominator>0)
      }else{
        indicator_agd <- dbtmp %>%            # Group by Age, Gender, and Disability
          summarise(                                       # Summarise to compute values
            var_name = indicator_name,                        # Name of the variable
            num_obs_uw = unweighted(n()),                    # unweighted total count
            denominator = survey_total(),                    # weighted total count
            mean_value = survey_mean(!!sym(indicator), vartype = c("ci", "se"),na.rm = TRUE) # indicator value ( weighted) with CI and SE
          )
      }
      
      assign(indicator_name,indicator_agd,envir = .GlobalEnv)
      if(nrow(indicator_agd)>0){
        #* [Calcul des p-values de l'indicateur par rapport ]
        get_pvalues(dbtmp %>% ungroup(),
                    var_indicator = indicator, 
                    var_expl = agd, #"cat_agd",
                    var_table_name = indicator_name,
                    numerator=var_numerator,
                    denominator=var_denominator)
      }
    }
  }
  cat(glue::glue_col(
    "{green ✔} Programme exécuté avec succès! Tables standards pour {indicator} générées ..."
  ),"\n")
  invisible(0)
}

#* [Fonction permettant de génération des tables AGD]
get_rmsTable_AGD <- function(database,indicator,
                               age=TRUE,gender=TRUE,disability=TRUE,
                               get_ratio=FALSE,
                               var_numerator=NULL, var_denominator=NULL){
  
  varlist <- NULL
  new_varname <-  NULL
  if(age){
    varlist <- c(varlist,"HH07_cat")
    new_varname <-  c(new_varname,as.character(str_glue("{indicator}_age")))
    
    # Calcule pour groupe d'âge : 0-4, 5-17, 18-59 & 60+ (pour compass)
    varlist <- c(varlist,"HH07_cat1")
    new_varname <-  c(new_varname,as.character(str_glue("{indicator}_age_1")))
    # Calcule pour groupe d'âge : 0-17, 18-60+ (pour le rapport)
    varlist <- c(varlist,"HH07_cat2")
    new_varname <-  c(new_varname,as.character(str_glue("{indicator}_age_2")))
  }
  
  if(gender){
    varlist <- c(varlist,"HH04")
    new_varname <-  c(new_varname,as.character(str_glue("{indicator}_gender")))
  }
  
  if(disability){
    varlist <- c(varlist,"disability")
    new_varname <-  c(new_varname,as.character(str_glue("{indicator}_disability")))
  }
  
  if(is.null(database) | is.null(indicator) | is.null(varlist)){
    
    cat(glue::glue_col(
      "{red ✖} Erreur lors de l'exécution du programme pour {indicator}..." 
    ),"\n")
    # invisible(1)
  }
  
  if(get_ratio & (is.null(var_numerator) | is.null(var_denominator))){
    cat(glue::glue_col(
      "{red ✖} Erreur lors de l'exécution du programme pour {indicator} & get_ratio ..." 
    ),"\n")
    invisible(1) 
  }
  
  # Affectation des noms des variables...
  names(new_varname) <- varlist
  # var_indicator <- c("age","gender","disability")
  # names(var_indicator) <- varlist
  for(varname in varlist){
    if(!get_ratio){
      tmp <-  database %>%
        filter(!is.na(!!sym(varname))) %>%      # Exclude if HH07_cat is NA
        group_by(!!sym(varname)) %>%            # Group by Age, Gender, and Disability
        summarise(                              # Summarise to compute values
          var_name = new_varname[varname],      # Name of the variable
          num_obs_uw = unweighted(n()),         # Unweighted total count
          denominator = survey_total(),         # Weighted total count
          # mean_value = survey_mean(!!sym(new_varname[varname]), vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
          mean_value = survey_mean(!!sym(indicator), vartype = c("ci", "se"), na.rm = TRUE)  # Compute mean with NA removed
        )
    }else{
      tmp <- database %>%
        filter(!is.na(!!sym(varname))) %>%      # Exclude if HH07_cat is NA
        group_by(!!sym(varname)) %>%            # Group by Age, Gender, and Disability
        summarise(                                         # put all variables here
          var_name = new_varname[varname],                            # name of the variable
          num_obs_uw = unweighted(sum(!!sym(var_denominator),na.rm=TRUE)),      # Unweighted total count
          denominator = survey_total(!!sym(var_denominator), na.rm = TRUE), 
          mean_value = survey_ratio(
            numerator = !!sym(var_numerator),
            denominator = !!sym(var_denominator),
            vartype = c("ci", "se"), na.rm = TRUE
          ) 
        ) %>% filter(denominator>0)
    }
    
    assign(as.character(new_varname[varname]),tmp,envir = .GlobalEnv)
    if(nrow(tmp)>1){
      #* [Calcul des p-values de l'indicateur par rapport ]
      get_pvalues(database %>% filter(!is.na(!!sym(varname))),
                  var_indicator=indicator,
                  var_expl=varname,
                  var_table_name = new_varname[varname],
                  numerator=var_numerator,
                  denominator=var_denominator)
    }
    cat(glue::glue_col(
      "{green ✔} Programme exécuté avec succès! Tables standards pour {new_varname[varname]} générées ..."
    ),"\n")
  }
  # Suppression des variables inutiles
  # rm(varlist,new_varname,var_indicator,varname,tmp)
  rm(varlist,new_varname,varname,tmp)
  # invisible(0)
}

#* [Fonction permettant de production des graphiques standard RMS]
#* agd = [disability_age,disability_gender,gender_age,disability_gender_age]
get_rmsGraphics <- function(indicator,indicator_name,plt_subtitle,agd=NULL,plt_caption=NULL){
  
  if(is.na(indicator)|is.null(indicator)|is.na(indicator_name)|is.null(indicator_name)|
     is.na(plt_subtitle)|is.null(plt_subtitle)){
    cat(glue::glue_col(
      "{red ✖}  get_rmsGraphics:: {indicator} ::Paramétres en entrée incorrectes lors de l'exécution du programme." 
    ))
    invisible(1) 
  }
  
  if(exists(indicator,envir = .GlobalEnv)){
    
    rmsTable <-  get(indicator,envir = .GlobalEnv)
    ###Chart of impact 2_2 by pop groups
    graph <-  ggplot(rmsTable, aes(x = pop_groups, y = mean_value, fill = pop_groups)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_errorbar(aes(ymin = (mean_value - mean_value_se), ymax = (mean_value + mean_value_se)),
                    width = 0.2, position = position_dodge(0.7)) +
      geom_text(aes(label = round(mean_value, 2)), 
                vjust = -0.5, position = position_dodge(0.7)) +  # Add labels for mean_value
      # scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      # scale_y_continuous(limits = NULL, expand = c(0, 0)) +
      labs(
        title = paste("Résultats de ",indicator_name,sep=" "),
        subtitle = plt_subtitle,
        caption = unhcr_caption,
        x = "Groupes de population",
        y = "Proportion avec erreurs standard"
      ) +
      scale_fill_unhcr_d() +  # Use UNHCR color palette (requires unhcrthemes package)
      theme_unhcr() +         # Apply UNHCR theme (requires unhcrthemes package)
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
      )
      
    assign(
      as.character(str_glue("{indicator}_graph01")),
      graph,envir = .GlobalEnv
    )
    
    if(exists(as.character(str_glue("{indicator}_graph01")),envir = .GlobalEnv)){
      cat(glue::glue_col(
        "{green ✔} Programme exécuté avec succès! Graphique standard {indicator}_graph01 générées ..."
      ),"\n")
    }else{
      cat(glue::glue_col(
        "{green ✔} Echec Programme! Graphique standard {indicator}_graph01 non générées ..."
      ),"\n")
      invisible(1)
    }
      
    
    indicator_agd <- as.character(str_glue("{indicator}_AGD"))  
    if(!is.null(agd) & exists(indicator_agd,envir = .GlobalEnv)){
      
      rmsTable_agd <- get(indicator_agd,envir = .GlobalEnv)
      
      rmsGraphics <- switch(
        agd,
        disability_age = ggplot(rmsTable_agd, aes(x = HH07_cat, y = mean_value, fill = disability)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.7) +
          geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, position = position_dodge(0.7)) +  
          scale_fill_unhcr_d() +  # UNHCR color palette
          labs(
            title = paste0(indicator_name," par age et situattion de handicap"),
            subtitle = plt_subtitle,
            x = "Age",
            y = "Proportion",
            fill="Situation de handicap",
            caption = paste(unhcr_caption,plt_caption)  #
          ),
        disability_gender = ggplot(rmsTable_agd, aes(x = HH04, y = mean_value, fill = disability)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.7) +
          geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, position = position_dodge(0.7)) +  
          scale_fill_unhcr_d() +  # UNHCR color palette
          labs(
            title = paste0(indicator_name," par sexe et situation de handiccap"),
            subtitle = plt_subtitle,
            x = "Sexe",
            y = "Proportion",
            fill="Situation de handicap",
            caption = paste(unhcr_caption,plt_caption)  #
          ),
        gender_age = ggplot(rmsTable_agd, aes(x = HH04, y = mean_value, fill = HH07_cat)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.7) +
          geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, position = position_dodge(0.7)) +  
          scale_fill_unhcr_d() +  # UNHCR color palette
          labs(
            title = paste0(indicator_name," par sexe et age"),
            subtitle = plt_subtitle,
            x = "Sexe",
            y = "Proportion",
            fill="Age",
            caption = paste(unhcr_caption,plt_caption)  #
          ),
        disability_gender_age = ggplot(rmsTable_agd, aes(x = HH04, y = mean_value, fill = disability)) +
          geom_bar(stat = "identity", position = "dodge", width = 0.7) +
          geom_text(aes(label = round(mean_value, 2)), vjust = -0.5, position = position_dodge(0.7)) +  
          scale_fill_unhcr_d() +  # UNHCR color palette
          facet_wrap(~ HH07_cat) +  # Create facets for each age group
          labs(
            title = paste0(indicator_name," par sexe, age et situation de handicap"),
            subtitle = plt_subtitle,
            x = "Sexe",
            y = "Proportion",
            fill = "Situation de handicap",
            caption = paste(unhcr_caption,"Note : Le module sur le handicap n'inclut pas les enfants de moins de 5 ans.",plt_caption)  #
          )
      )
      
      ####Chart with the AGD variables 
      graph_agd <- rmsGraphics +
        theme_unhcr() +  # Apply UNHCR theme
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
        )
      
      assign(
        as.character(str_glue("{indicator}_graph02")),
        graph_agd,envir = .GlobalEnv
      )
      
      cat(glue::glue_col(
        "{green ✔} Programme exécuté avec succès! Graphique standard {indicator}_graph02 générées ..."
      ),"\n")
      
    }
    
    # else{
    #   cat(glue::glue_col(
    #     "{red ✖}  get_rmsGraphics:: {indicator}_AGD table rms non disponible." 
    #   ),"\n")
    #   invisible(1) 
    # }
    
  }else{
    cat(glue::glue_col(
      "{red ✖}  get_rmsGraphics:: {indicator} table rms non disponible." 
    ))
    invisible(1) 
  }
  invisible(0)  
}

#* [Fonction permettant de restructuration des tables standards]
#* Cette fonction reformate les tables standards rms pour faciliter leur exploitation
#* au format excel...
#* agd_type = disability_age,disability_gender,gender_age,disability_gender_age
rebuild_table <- function(rms_table,
                          critical_value,
                          acceptable_value,
                          var_pivot=NULL,agd_type=NULL,
                          inverse=FALSE,
                          infos=TRUE){
  
  if(is_empty(rms_table) | is.null(rms_table)) 
    invisible(NULL)
  
  if(is.null(var_pivot)) var_pivot <- names(rms_table)[1]
  
  rms_tmp <- rms_table
  
  if(!is.null(agd_type)){
    
    rms_tmp <-  switch (agd_type,
      disability_age = rms_table %>% 
        rename(age=HH07_cat) %>% ungroup() %>% distinct() %>% 
        mutate(
          variable="disability_age",
          disaggregation = str_trim(as.character(str_glue("{disability} {age}")))
        ),
      disability_gender = rms_table %>% 
        rename(gender=HH04) %>% ungroup() %>% distinct() %>% 
        mutate(
          variable="disability_gender",
          disaggregation = str_trim(as.character(str_glue("{disability} {gender}")))
        ),
      gender_age = rms_table %>% 
        rename(gender=HH04,age=HH07_cat) %>% ungroup() %>% distinct() %>% 
        mutate(
          variable="gender_age",
          disaggregation = str_trim(as.character(str_glue("{gender} {age}")))
        ),
      disability_gender_age = rms_table %>% 
        rename(age=HH07_cat, gender=HH04) %>% ungroup() %>% distinct() %>% 
        mutate(
          variable="disability_gender_age",
          disaggregation = str_trim(as.character(str_glue("{disability} {gender} {age}")))
        )
    )
    
    result <- rms_tmp %>% 
      # select(c(denominator_se,mean_value_low,mean_value_upp,mean_value_se, age,gender,disability)) %>% 
      # rename(indicator_code=var_name, numerator=num_obs_uw,value=mean_value) %>% 
      select(indicator_code=var_name,variable,disaggregation,
             unweighted_denominator=num_obs_uw,
             numerator=num_obs_uw,denominator,value=mean_value) %>% 
      mutate(
        indicator_code = str_replace_all(indicator_code,"_disability|_gender|_age|_AGD",""),
        denominator = round(denominator),
        numerator = round(value*denominator),
        value = round(100*value,1),
        variable = case_when(
          variable=="pop_groups" ~"population",
          variable=="HH04" ~ "sexe",
          variable=="HH07_cat" | variable=="HH07_cat2" ~ "age",
          variable=="disability" ~ "handicap",
          TRUE ~ variable
        )
      ) %>% 
      # select(indicator_code,variable,disaggregation,everything()) %>% 
      select(indicator_code,variable,disaggregation,unweighted_denominator,
             numerator,denominator,value) %>% 
      arrange(indicator_code,variable,disaggregation)
    
  }else{
    
    result <- rms_tmp %>% 
      # select(-c(denominator_se,mean_value_low,mean_value_upp,mean_value_se)) %>%
      select(!!sym(var_pivot),var_name,num_obs_uw,denominator,mean_value) %>% 
      # pivot_longer(cols=pop_groups,names_to="variable",values_to = "disaggregation") %>% 
      pivot_longer(cols=!!sym(var_pivot),names_to="variable",values_to = "disaggregation") %>% 
      rename(indicator_code=var_name, unweighted_denominator=num_obs_uw,value=mean_value) %>% 
      mutate(
        indicator_code = str_replace_all(indicator_code,"_disability|_gender|_age|_AGD",""),
        denominator = round(denominator),
        numerator = round(value*denominator),
        value = round(100*value,1),
        variable = case_when(
          variable=="pop_groups" ~"population",
          variable=="HH04" ~ "sexe",
          variable=="HH07_cat" | variable=="HH07_cat2" ~ "age",
          variable=="disability" ~ "handicap",
          TRUE ~ variable
        )
      ) %>% 
      select(indicator_code,variable,disaggregation,unweighted_denominator,
             numerator,denominator,value) %>% 
      arrange(indicator_code,variable,disaggregation)
  }
  
  if(inverse){
    result <- result %>% 
      mutate(
        status = case_when(
          value >= critical_value ~"CRITIQUE",
          value < critical_value &  value>acceptable_value ~ "INACCEPTABLE",
          value <= acceptable_value ~ "ACCEPTABLE"
        ),
        status_message = case_when(
          status=="CRITIQUE" ~ as.character(str_glue("valeur >= {critical_value}. Cible : au plus {acceptable_value}.")),
          status=="INACCEPTABLE" ~ as.character(str_glue("valeur < {critical_value} et valeur > {acceptable_value}. Cible : au plus {acceptable_value}.")),
          status=="ACCEPTABLE" ~ as.character(str_glue("valeur ≤ {acceptable_value}. Cible : au plus {acceptable_value}."))
        )
      )
  }else{
    result <- result %>% 
      mutate(
        status = case_when(
          value <= critical_value ~"CRITIQUE",
          value > critical_value &  value<acceptable_value ~ "INACCEPTABLE",
          value >= acceptable_value ~ "ACCEPTABLE"
        ),
        status_message = case_when(
          status=="CRITIQUE" ~ as.character(str_glue("valeur ≤ {critical_value}. Cible : au moins {acceptable_value}.")),
          status=="INACCEPTABLE" ~ as.character(str_glue("valeur > {critical_value} et valeur < {acceptable_value}. Cible : au moins {acceptable_value}.")),
          status=="ACCEPTABLE" ~ as.character(str_glue("valeur ≥ {acceptable_value}. Cible : au moins {acceptable_value}."))
        )
      )
  }
  
  ## Prise en compte de valeurs target et cible si existe...
  ##* [Dans le cas du RMS TCHAD les valeurs baseline et target ont été fournies. Donc on va réactiver cette section]
  if(!is.null(infos) && isTRUE(infos)){
    # Ajout des informations de comparaison
    result <-  result %>%
      left_join(
        rms_infos %>%
          select(
            indicator_code,variable=disaggregation_variable,
            disaggregation=disaggregation_value,baseline,target
          ),
        by = join_by(indicator_code,variable,disaggregation)
      ) %>%
      mutate(
        comp_baseline = round(100*((value - baseline)/baseline),0),
        status_message = case_when(
          comp_baseline < 0 ~ as.character(str_glue("{status_message} Par rapport à la valeur du baseline qui était de {baseline}, l'indicateur a connu une dimunition de {abs(comp_baseline)}%, passant de {baseline} à {value}.")),
          comp_baseline == 0 ~ as.character(str_glue("{status_message} Par rapport à la valeur du baseline qui était de {baseline}, l'indicateur n'a connu aucune évolution.")),
          comp_baseline > 0 ~ as.character(str_glue("{status_message} Par rapport à la valeur du baseline qui était de {baseline}, l'indicateur a connu une augmentation de {comp_baseline}%, passant de {baseline} à {value}.")),
          is.na(comp_baseline) ~ status_message
        ),
        comp_target = value - target,
        status_message = case_when(
          comp_target < 0 ~ as.character(str_glue("{status_message} La cible fixée à {target} n'a pas été atteinte. La valeur observée ({value}) est inférieure de {abs(comp_target)} points par rapport à l'objectif de {target}.")),
          comp_target == 0 ~ as.character(str_glue("{status_message} La cible fixée à {target} a été atteinte.")),
          comp_target > 0 ~ as.character(str_glue("{status_message} La cible fixée à {target} a été atteinte. La valeur observée ({value}) dépasse la cible de {comp_target} points par rapport à l'objectif de {target}.")),
          is.na(comp_target) ~ status_message
        )
      ) %>% select(-c(baseline,target,comp_baseline,comp_target))
  }
  invisible(result)    
}

#* [Export des données aux différents format]
#* L'écriture des
unhcr_export <- function(dataset,file_path){
  file_ext <- str_split_1(basename(file_path),pattern = "\\.")[2]
  
  if(file_ext=="csv"){
    write_excel_csv(dataset,file_path)
    invisible(0)
  }
  if(file_ext=="dta"){
    haven::write_dta(dataset,file_path)
    invisible(0)
  } 
  if(file_ext=="sav"){
    haven::write_sav(dataset,file_path)
    invisible(0)
  } 
  if(file_ext=="rda"){
    save(dataset,file=file_path)
    invisible(0)
  } 
  if(file_ext=="xlsx"){
    openxlsx::write.xlsx(dataset,file=file_path,creator="Charles Mouté")
    invisible(0)
  } 
  # Si aucun des cas ci-dessus alors retourne un code erreur..
  invisible(1)
}


#* [Fonction d'écriture des données]
export_dataset <- function(dataset,file_path,echo=TRUE){
  # Suppression préalable du fichier si il existe
  if(file.exists(file_path)) file.remove(file_path)
  # Ecriture sur disque du fichier de données
  # suppressMessages(suppressWarnings(rio::export(dataset,file_path)))
  # rio::export(dataset,file_path)
  
  unhcr_export(dataset,file_path)
  if(echo){
    #Message indiqaunt le résultat de l'opération
    if (file.exists(file_path)) {
      cat(glue::glue_col("{green ✔} Dataset exported to ", file_path), "\n")
    } else {
      cat(glue::glue_col("{red ✖} Export failed to ", file_path), "\n")
    }
  }
}

#* [Fonction d'enregistrement des bases de données]
saving_datasets <- function(output_folder,echo=TRUE){
  
  if(!output_folder %in% c("raw","clean","analysis")){
    cat(glue::glue_col("{red ✖} ", output_folder), " : option unavailable\n")
    invisible(1)
  }
  
  path <- file.path("local","database",output_folder)
  list_folder <- c("csv","R","stata","spss","excel")
  file_ext <- c(".csv",".rda",".dta",".sav",".xlsx")
  names(file_ext) <- list_folder
  
  #' [Export des données sur les individus]
  if(exists("ind",envir = .GlobalEnv)){
    
    dbase_name <- "individual"
    dbase <-  ind %>% 
      as_tibble() %>% 
      mutate( 
        across(
          where(function(x) all(x %in% c("haven_labelled", "vctrs_vctr", "character"))) & 
            !c(REF02,REF04,REF05,REF07,REF08,REF14),
          labelled_chr2dbl),
        across(
          where(is.logical),as.character),
        citizenship_code = parse_character(citizenship),
        citizenship = to_character(citizenship),
        REF02_code = parse_character(REF02),
        REF02 = to_character(REF02),
        REF04_code = parse_character(REF04),
        REF04 = to_character(REF04),
        REF05_code = parse_character(REF05),
        REF05 = to_character(REF05),
        REF07_code = parse_character(REF07),
        REF07 = to_character(REF07),
        REF08_code = parse_character(REF08),
        REF08 = to_character(REF08),
        REF14 = parse_character(REF14),
        REF14 = to_character(REF14)
      ) %>% 
      clean_names(case="none") %>%
      haven::as_factor()
    
    for(folder in list_folder){
      
      # if(folder=="csv"){
      #   dbase <- ind
      # }else{
      #   suppressWarnings(dbase <- ind  %>% to_factor() %>% clean_names(case="none"))
      # }
      
      file_path <- file.path(path,folder,paste0(dbase_name,file_ext[folder]))
      export_dataset(dbase,file_path,echo)
    }
  }else{
    cat(glue::glue_col("{red ✖} The individual database saving failed"), "\n")
    invisible(1)
  }
  
  #' [Export des données sur les ménages]
  if(exists("main",envir = .GlobalEnv)){
    
    dbase_name <- "household"
    # dbase <- main  %>% to_factor() %>% clean_names(case="none")
    # dbase <-  get("main",envir = .GlobalEnv) %>% 
    #   to_factor() %>% clean_names(case="none")
    dbase <-  main %>% 
      as_tibble() %>% 
      mutate( 
        across(
          where(function(x) all(x %in% c("haven_labelled", "vctrs_vctr", "character"))) & 
            !c(Country,Intro05),
          labelled_chr2dbl),
        across(
          where(is.logical),as.character),
        Country_code = parse_character(Country),
        Country = to_character(Country),
        Intro05_code = parse_number(Intro05),
        Intro05 = to_character(Intro05)
      ) %>% 
      clean_names(case="none") %>%
      haven::as_factor()
    
    for(folder in list_folder){
      # if(folder=="csv"){
      #   dbase <- main
      # }else{
      #   suppressWarnings(dbase <- main  %>% to_factor() %>% clean_names(case="none"))
      # }
      file_path <- file.path(path,folder,paste0(dbase_name,file_ext[folder]))
      export_dataset(dbase,file_path,echo)
    }
  }else{
    cat(glue::glue_col("{red ✖} The household database saving failed"), "\n")
    invisible(1)
  }
    
}

#* [Fonction d'enregistrement des graphiques]
saving_graphics <-  function(output_folder=NULL, echo=TRUE){
  
  if(is.null(output_folder)){
    path <- file.path("local","database","graphics")
  }else{
    if(!output_folder %in% c("raw","clean","analysis")){
      cat(glue::glue_col("{red ✖} ", output_folder), " : option unavailable\n")
      invisible(1)
    }
    path <- file.path("local","database",output_folder)
  }
  
  graphes <- str_subset(ls(envir = .GlobalEnv),"_graph*") %>% 
    setdiff(c("saving_graphics"))
  filenames <- paste0(str_replace_all(graphes,"_graph"," ("),").png")
  names(filenames) <-  graphes
  for(graph in graphes){
    var_graph <-  get(graph,envir = .GlobalEnv)
    file_path <- file.path(path,as.character(filenames[graph]))
    if(file.exists(file_path)) file.remove(file_path)
    suppressWarnings(ggsave(
      filename=file_path, 
      plot = var_graph, 
      # width = 8, height = 7
      width = 10, height = 6
    ))
    if(echo){
      if(file.exists(file_path)){
        cat(glue::glue_col("{green ✔} ",file_path), " saved successfully\n")
      }else{
        cat(glue::glue_col("{red ✖} ", file_path), " saving failed\n")
      }
    }
  }
}

