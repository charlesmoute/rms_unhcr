#*********************************************************************************
# server.R - RMS Data Collection Monitoring Dashboard
#*********************************************************************************
# Logique serveur pour les graphiques, tableaux et indicateurs du dashboard.
#*********************************************************************************

function(input, output, session) {
  
  #*********************************************************************************
  # FILTRES INTERCONNECTÉS (DÉPENDANTS)
  #*********************************************************************************
  # Ces réactifs calculent les valeurs disponibles pour chaque filtre
  # en fonction des autres filtres sélectionnés
  
  # Zones disponibles selon les autres filtres
  available_zones <- reactive({
    get_available_zones(
      date_range = input$date_range,
      localites = input$filter_localite,
      enumerators = input$filter_enumerator,
      populations = input$filter_population
    )
  })
  
  # Localités disponibles selon les autres filtres
  available_localites <- reactive({
    get_available_localites(
      date_range = input$date_range,
      zones = input$filter_zone,
      enumerators = input$filter_enumerator,
      populations = input$filter_population
    )
  })
  
  # Enquêteurs disponibles selon les autres filtres
  available_enumerators <- reactive({
    get_available_enumerators(
      date_range = input$date_range,
      zones = input$filter_zone,
      localites = input$filter_localite,
      populations = input$filter_population
    )
  })
  
  # Populations disponibles selon les autres filtres
  available_populations <- reactive({
    get_available_populations(
      date_range = input$date_range,
      zones = input$filter_zone,
      localites = input$filter_localite,
      enumerators = input$filter_enumerator
    )
  })
  
  # # Observer pour mettre à jour les choix des zones
  # observe({
  #   updatePickerInput(session, "filter_zone",
  #                    choices = available_zones(),
  #                    selected = intersect(input$filter_zone, available_zones()))
  # })
  # 
  # # Observer pour mettre à jour les choix des localités
  # observe({
  #   updatePickerInput(session, "filter_localite",
  #                    choices = available_localites(),
  #                    selected = intersect(input$filter_localite, available_localites()))
  # })
  # 
  # # Observer pour mettre à jour les choix des enquêteurs
  # observe({
  #   updatePickerInput(session, "filter_enumerator",
  #                    choices = available_enumerators(),
  #                    selected = intersect(input$filter_enumerator, available_enumerators()))
  # })
  # 
  # # Observer pour mettre à jour les choix des populations
  # observe({
  #   updatePickerInput(session, "filter_population",
  #                    choices = available_populations(),
  #                    selected = intersect(input$filter_population, available_populations()))
  # })
  
  # Remplacer les 4 observers dans server.R par :
  
  # Observer pour mettre à jour les choix des zones
  observe({
    req(available_zones())
    updatePickerInput(session, "filter_zone",
                      choices = available_zones(),
                      selected = intersect(input$filter_zone %||% character(0), 
                                           available_zones()))
  })
  
  # Observer pour mettre à jour les choix des localités
  observe({
    req(available_localites())
    updatePickerInput(session, "filter_localite",
                      choices = available_localites(),
                      selected = intersect(input$filter_localite %||% character(0), 
                                           available_localites()))
  })
  
  # Observer pour mettre à jour les choix des enquêteurs
  observe({
    req(available_enumerators())
    updatePickerInput(session, "filter_enumerator",
                      choices = available_enumerators(),
                      selected = intersect(input$filter_enumerator %||% character(0), 
                                           available_enumerators()))
  })
  
  # Observer pour mettre à jour les choix des populations
  observe({
    req(available_populations())
    updatePickerInput(session, "filter_population",
                      choices = available_populations(),
                      selected = intersect(input$filter_population %||% character(0), 
                                           available_populations()))
  })
  
  # Observer pour réinitialiser les filtres
  observeEvent(input$reset_filters, {
    updateDateRangeInput(session, "date_range",
                         start = FILTERS$date_min,
                         end = FILTERS$date_max)
    
    updatePickerInput(session, "filter_zone", selected = character(0))
    updatePickerInput(session, "filter_localite", selected = character(0))
    updatePickerInput(session, "filter_enumerator", selected = character(0))
    updatePickerInput(session, "filter_population", selected = character(0))
  })
  
  # Ajouter un helper pour éviter les erreurs NULL
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  #*********************************************************************************
  # DONNÉES RÉACTIVES FILTRÉES
  #*********************************************************************************
  
  # Données filtrées pour chaque feuille
  filtered_missing <- reactive({
    filter_data(DATA$missing,
                date_range  = input$date_range,
                zones       = input$filter_zone,
                localites   = input$filter_localite,
                enumerators = input$filter_enumerator,
                populations = input$filter_population)
  })
  
  filtered_error <- reactive({
    filter_data(DATA$error,
                date_range  = input$date_range,
                zones       = input$filter_zone,
                localites   = input$filter_localite,
                enumerators = input$filter_enumerator,
                populations = input$filter_population)
  })
  
  filtered_other <- reactive({
    filter_data(DATA$other,
                date_range  = input$date_range,
                zones       = input$filter_zone,
                localites   = input$filter_localite,
                enumerators = input$filter_enumerator,
                populations = input$filter_population)
  })
  
  filtered_evaluation <- reactive({
    filter_data(DATA$evaluation,
                date_range  = input$date_range,
                zones       = input$filter_zone,
                localites   = input$filter_localite,
                enumerators = input$filter_enumerator,
                populations = input$filter_population)
  })
  
  filtered_silhouette <- reactive({
    filter_data(DATA$silhouette,
                zones       = input$filter_zone,
                localites   = input$filter_localite,
                enumerators = input$filter_enumerator)
  })
  
  filtered_perf_zone <- reactive({
    filter_data(DATA$perf_zone,
                date_range = input$date_range,
                zones      = input$filter_zone)
  })
  
  filtered_perf_localite <- reactive({
    filter_data(DATA$perf_localite,
                date_range  = input$date_range,
                zones       = input$filter_zone,
                localites   = input$filter_localite)
  })
  
  filtered_perf_enum <- reactive({
    filter_data(DATA$perf_enum,
                date_range  = input$date_range,
                zones       = input$filter_zone,
                enumerators = input$filter_enumerator)
  })
  
  filtered_perf_pop <- reactive({
    filter_data(DATA$perf_pop,
                date_range  = input$date_range,
                populations = input$filter_population)
  })
  
  #*********************************************************************************
  # === ONGLET ÉVALUATION - MISSING ====
  #*********************************************************************************
  
  output$missing_kpi_total <- renderUI({
    df <- filtered_missing()
    create_value_box("Total anomalies manquantes", nrow(df),
                     subtitle = paste(n_distinct(df$caseid), "questionnaires concernés"),
                     icon = "exclamation-triangle", color = "warning")
  })
  
  output$missing_kpi_types <- renderUI({
    df <- filtered_missing()
    create_value_box("Types de variables", n_distinct(df$type),
                     subtitle = "Variables avec valeurs manquantes",
                     icon = "list-check", color = "info")
  })
  
  output$missing_kpi_enum <- renderUI({
    df <- filtered_missing()
    create_value_box("Enquêteurs concernés", n_distinct(df$enumerator),
                     subtitle = paste("sur", length(FILTERS$enumerators), "enquêteurs"),
                     icon = "people", color = "primary")
  })
  
  output$missing_kpi_treated <- renderUI({
    df <- filtered_missing()
    n_treated <- sum(df$status == "TREATED", na.rm = TRUE)
    pct <- ifelse(nrow(df) > 0, round(n_treated / nrow(df) * 100, 1), 0)
    create_value_box("Taux de traitement", paste0(pct, "%"),
                     subtitle = paste(n_treated, "/", nrow(df), "traités"),
                     icon = "check-circle", color = ifelse(pct >= 80, "success", "danger"))
  })
  
  output$missing_by_zone <- renderPlotly({
    df <- filtered_missing()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(zone, sort = TRUE)
    
    plot_ly(agg, x = ~reorder(zone, n), y = ~n, type = "bar",
            marker = list(color = COLORS$warning, 
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Anomalies: %{y}<extra></extra>") %>%
      plotly_layout(title = "Valeurs manquantes par zone",
                    xlab = "", ylab = "Nombre d'anomalies") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$missing_by_type <- renderPlotly({
    df <- filtered_missing()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% 
      count(type, sort = TRUE) %>%
      head(15) %>%
      mutate(type_short = str_replace(type, "^missing_", ""))
    
    plot_ly(agg, x = ~n, y = ~reorder(type_short, n), type = "bar",
            orientation = "h",
            marker = list(color = COLORS$chart_palette[1:nrow(agg)]),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Occurrences: %{x}<extra></extra>") %>%
      plotly_layout(title = "Top 15 variables avec valeurs manquantes",
                    xlab = "Nombre d'occurrences", ylab = "")
  })
  
  output$missing_trend <- renderPlotly({
    df <- filtered_missing()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(date) %>% arrange(date)
    
    plot_ly(agg, x = ~date, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = COLORS$warning, width = 2.5),
            marker = list(color = COLORS$warning, size = 6),
            fill = "tozeroy", fillcolor = "rgba(217, 119, 6, 0.1)",
            hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>Anomalies: %{y}<extra></extra>") %>%
      plotly_layout(title = "Évolution temporelle des valeurs manquantes",
                    xlab = "Date", ylab = "Nombre d'anomalies")
  })
  
  output$missing_by_enumerator <- renderPlotly({
    df <- filtered_missing()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(enumerator, sort = TRUE) %>% head(15)
    
    plot_ly(agg, x = ~n, y = ~reorder(enumerator, n), type = "bar",
            orientation = "h",
            marker = list(color = COLORS$primary,
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Anomalies: %{x}<extra></extra>") %>%
      plotly_layout(title = "Top 15 enquêteurs - Valeurs manquantes",
                    xlab = "Nombre d'anomalies", ylab = "")
  })
  
  output$missing_table <- renderDT({
    df <- filtered_missing() %>%
      select(date, zone, localite, enumerator, population, caseid, type, message, status) %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type = str_replace(type, "^missing_", ""))
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Localité", "Enquêteur", "Population",
                           "CaseID", "Variable", "Message", "Statut"),
              class = "compact stripe hover") %>%
      formatStyle("status",
                  backgroundColor = styleEqual(c("TREATED", "NO_TREATED"),
                                                c("#D1FAE5", "#FEE2E2")))
  })
  
  #*********************************************************************************
  # === ONGLET ÉVALUATION - ERROR ====
  #*********************************************************************************
  
  output$error_kpi_total <- renderUI({
    df <- filtered_error()
    create_value_box("Total erreurs", nrow(df),
                     subtitle = paste(n_distinct(df$caseid), "questionnaires concernés"),
                     icon = "x-circle", color = "danger")
  })
  
  output$error_kpi_types <- renderUI({
    df <- filtered_error()
    create_value_box("Types d'erreurs", n_distinct(df$type),
                     subtitle = "Catégories d'erreurs détectées",
                     icon = "bug", color = "warning")
  })
  
  output$error_kpi_enum <- renderUI({
    df <- filtered_error()
    create_value_box("Enquêteurs concernés", n_distinct(df$enumerator),
                     subtitle = paste("sur", length(FILTERS$enumerators), "enquêteurs"),
                     icon = "people", color = "info")
  })
  
  output$error_kpi_treated <- renderUI({
    df <- filtered_error()
    n_treated <- sum(df$status == "TREATED", na.rm = TRUE)
    pct <- ifelse(nrow(df) > 0, round(n_treated / nrow(df) * 100, 1), 0)
    create_value_box("Taux de traitement", paste0(pct, "%"),
                     subtitle = paste(n_treated, "/", nrow(df), "traités"),
                     icon = "check-circle", color = ifelse(pct >= 80, "success", "danger"))
  })
  
  output$error_by_zone <- renderPlotly({
    df <- filtered_error()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(zone, sort = TRUE)
    
    plot_ly(agg, x = ~reorder(zone, n), y = ~n, type = "bar",
            marker = list(color = COLORS$danger,
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Erreurs: %{y}<extra></extra>") %>%
      plotly_layout(title = "Erreurs par zone", xlab = "", ylab = "Nombre d'erreurs") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$error_by_type <- renderPlotly({
    df <- filtered_error()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% 
      count(type, sort = TRUE) %>%
      mutate(type_label = str_replace(type, "^error_", ""))
    
    plot_ly(agg, labels = ~type_label, values = ~n, type = "pie",
            textinfo = "label+percent+value",
            marker = list(colors = COLORS$chart_palette),
            hovertemplate = "<b>%{label}</b><br>Erreurs: %{value}<br>Part: %{percent}<extra></extra>") %>%
      plotly_layout(title = "Répartition par type d'erreur", showlegend = TRUE)
  })
  
  output$error_trend <- renderPlotly({
    df <- filtered_error()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(date, type) %>% arrange(date)
    
    plot_ly(agg, x = ~date, y = ~n, color = ~type, type = "scatter",
            mode = "lines+markers",
            colors = COLORS$chart_palette,
            hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>Type: %{data.name}<br>Erreurs: %{y}<extra></extra>") %>%
      plotly_layout(title = "Évolution temporelle des erreurs par type",
                    xlab = "Date", ylab = "Nombre d'erreurs")
  })
  
  output$error_by_enumerator <- renderPlotly({
    df <- filtered_error()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(enumerator, type) %>%
      arrange(desc(n)) %>%
      group_by(enumerator) %>%
      mutate(total = sum(n)) %>%
      ungroup()
    
    top_enum <- agg %>% distinct(enumerator, total) %>% arrange(desc(total)) %>% head(15)
    agg <- agg %>% filter(enumerator %in% top_enum$enumerator)
    
    plot_ly(agg, x = ~n, y = ~reorder(enumerator, total), color = ~type,
            type = "bar", orientation = "h",
            colors = COLORS$chart_palette,
            hovertemplate = "<b>%{y}</b><br>Type: %{data.name}<br>Erreurs: %{x}<extra></extra>") %>%
      plotly_layout(title = "Top 15 enquêteurs - Erreurs",
                    xlab = "Nombre d'erreurs", ylab = "") %>%
      layout(barmode = "stack")
  })
  
  output$error_table <- renderDT({
    df <- filtered_error() %>%
      select(date, zone, localite, enumerator, population, caseid, type, message, status) %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type = str_replace(type, "^error_", ""))
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Localité", "Enquêteur", "Population",
                           "CaseID", "Type erreur", "Message", "Statut"),
              class = "compact stripe hover") %>%
      formatStyle("status",
                  backgroundColor = styleEqual(c("TREATED", "NO_TREATED"),
                                                c("#D1FAE5", "#FEE2E2")))
  })
  
  #*********************************************************************************
  # === ONGLET ÉVALUATION - OTHER ====
  #*********************************************************************************
  
  output$other_kpi_total <- renderUI({
    df <- filtered_other()
    create_value_box("Total valeurs 'Autre'", nrow(df),
                     subtitle = paste(n_distinct(df$caseid), "questionnaires concernés"),
                     icon = "question-circle", color = "secondary")
  })
  
  output$other_kpi_types <- renderUI({
    df <- filtered_other()
    create_value_box("Variables concernées", n_distinct(df$type),
                     subtitle = "Variables avec réponses 'Autre'",
                     icon = "list-ul", color = "info")
  })
  
  output$other_kpi_enum <- renderUI({
    df <- filtered_other()
    create_value_box("Enquêteurs concernés", n_distinct(df$enumerator),
                     subtitle = paste("sur", length(FILTERS$enumerators), "enquêteurs"),
                     icon = "people", color = "primary")
  })
  
  output$other_kpi_treated <- renderUI({
    df <- filtered_other()
    n_treated <- sum(df$status == "TREATED", na.rm = TRUE)
    pct <- ifelse(nrow(df) > 0, round(n_treated / nrow(df) * 100, 1), 0)
    create_value_box("Taux de traitement", paste0(pct, "%"),
                     subtitle = paste(n_treated, "/", nrow(df), "traités"),
                     icon = "check-circle", color = ifelse(pct >= 80, "success", "danger"))
  })
  
  output$other_by_zone <- renderPlotly({
    df <- filtered_other()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(zone, sort = TRUE)
    
    plot_ly(agg, x = ~reorder(zone, n), y = ~n, type = "bar",
            marker = list(color = COLORS$secondary,
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Valeurs 'Autre': %{y}<extra></extra>") %>%
      plotly_layout(title = "Valeurs 'Autre' par zone", xlab = "", ylab = "Nombre") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$other_by_type <- renderPlotly({
    df <- filtered_other()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% 
      count(type, sort = TRUE) %>%
      head(15) %>%
      mutate(type_short = str_replace(type, "^other_", ""))
    
    plot_ly(agg, x = ~n, y = ~reorder(type_short, n), type = "bar",
            orientation = "h",
            marker = list(color = COLORS$chart_palette[1:nrow(agg)]),
            text = ~n, textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Occurrences: %{x}<extra></extra>") %>%
      plotly_layout(title = "Top 15 variables avec valeurs 'Autre'",
                    xlab = "Nombre d'occurrences", ylab = "")
  })
  
  output$other_trend <- renderPlotly({
    df <- filtered_other()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>% count(date) %>% arrange(date)
    
    plot_ly(agg, x = ~date, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = COLORS$secondary, width = 2.5),
            marker = list(color = COLORS$secondary, size = 6),
            fill = "tozeroy", fillcolor = "rgba(124, 58, 237, 0.1)",
            hovertemplate = "<b>%{x|%d/%m/%Y}</b><br>Valeurs 'Autre': %{y}<extra></extra>") %>%
      plotly_layout(title = "Évolution temporelle des valeurs 'Autre'",
                    xlab = "Date", ylab = "Nombre")
  })
  
  output$other_table <- renderDT({
    df <- filtered_other() %>%
      select(date, zone, localite, enumerator, population, caseid, type, message, status) %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type = str_replace(type, "^other_", ""))
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Localité", "Enquêteur", "Population",
                           "CaseID", "Variable", "Message", "Statut"),
              class = "compact stripe hover") %>%
      formatStyle("status",
                  backgroundColor = styleEqual(c("TREATED", "NO_TREATED"),
                                                c("#D1FAE5", "#FEE2E2")))
  })
  
  #*********************************************************************************
  # ONGLET ÉVALUATION - EVALUATION
  #*********************************************************************************
  
  output$eval_kpi_total <- renderUI({
    df <- filtered_evaluation()
    create_value_box("Total évaluations", nrow(df),
                     subtitle = paste(n_distinct(df$caseid), "questionnaires évalués"),
                     icon = "clipboard-check", color = "primary")
  })
  
  output$eval_kpi_incoherence <- renderUI({
    df <- filtered_evaluation() %>% filter(type == "tx_incoherence_intra")
    avg_val <- ifelse(nrow(df) > 0, mean(df$value, na.rm = TRUE), 0)
    create_value_box("Taux moyen d'incohérence", fmt_pct(avg_val),
                     subtitle = paste(nrow(df), "questionnaires évalués"),
                     icon = "exclamation-diamond", color = ifelse(avg_val > 50, "danger", "success"))
  })
  
  output$eval_kpi_duree <- renderUI({
    df <- filtered_evaluation() %>% filter(type == "duree_collecte")
    avg_val <- ifelse(nrow(df) > 0, mean(df$value, na.rm = TRUE), 0)
    create_value_box("Durée moyenne de collecte", paste0(fmt_number(avg_val, 0), " min"),
                     subtitle = paste(nrow(df), "questionnaires"),
                     icon = "clock", color = "info")
  })
  
  output$eval_kpi_missing_intra <- renderUI({
    df <- filtered_evaluation() %>% filter(type == "tx_missing_intra")
    avg_val <- ifelse(nrow(df) > 0, mean(df$value, na.rm = TRUE), 0)
    create_value_box("Taux moyen missing intra", fmt_pct(avg_val),
                     subtitle = paste(nrow(df), "questionnaires évalués"),
                     icon = "file-earmark-x", color = "warning")
  })
  
  output$eval_incoherence_dist <- renderPlotly({
    df <- filtered_evaluation() %>% filter(type == "tx_incoherence_intra")
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, x = ~value, type = "histogram",
            marker = list(color = COLORS$primary, line = list(color = "white", width = 1)),
            nbinsx = 20,
            hovertemplate = "Taux: %{x:.1f}%<br>Fréquence: %{y}<extra></extra>") %>%
      plotly_layout(title = "Distribution du taux d'incohérence intra-questionnaire",
                    xlab = "Taux d'incohérence (%)", ylab = "Fréquence")
  })
  
  output$eval_duree_dist <- renderPlotly({
    df <- filtered_evaluation() %>% filter(type == "duree_collecte")
    if (nrow(df) == 0) return(plotly_empty())
    
    plot_ly(df, x = ~value, type = "histogram",
            marker = list(color = COLORS$info, line = list(color = "white", width = 1)),
            nbinsx = 25,
            hovertemplate = "Durée: %{x:.0f} min<br>Fréquence: %{y}<extra></extra>") %>%
      plotly_layout(title = "Distribution de la durée de collecte",
                    xlab = "Durée (minutes)", ylab = "Fréquence")
  })
  
  output$eval_by_enumerator <- renderPlotly({
    df <- filtered_evaluation() %>% filter(type == "tx_incoherence_intra")
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>%
      group_by(enumerator) %>%
      summarise(mean_val = mean(value, na.rm = TRUE),
                n = n(), .groups = "drop") %>%
      arrange(desc(mean_val))
    
    plot_ly(agg, x = ~mean_val, y = ~reorder(enumerator, mean_val), type = "bar",
            orientation = "h",
            marker = list(
              color = ~ifelse(mean_val > 80, COLORS$danger,
                              ifelse(mean_val > 50, COLORS$warning, COLORS$success)),
              line = list(color = COLORS$dark, width = 0.3)
            ),
            text = ~paste0(round(mean_val, 1), "% (n=", n, ")"),
            textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Taux moyen: %{x:.1f}%<extra></extra>") %>%
      plotly_layout(title = "Taux moyen d'incohérence par enquêteur",
                    xlab = "Taux d'incohérence moyen (%)", ylab = "")
  })
  
  output$eval_by_zone <- renderPlotly({
    df <- filtered_evaluation()
    if (nrow(df) == 0) return(plotly_empty())
    
    agg <- df %>%
      group_by(zone, type) %>%
      summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(type_label = case_when(
        type == "tx_incoherence_intra" ~ "Incohérence",
        type == "tx_missing_intra" ~ "Missing",
        type == "tx_other_intra" ~ "Autre",
        type == "duree_collecte" ~ "Durée (min)",
        TRUE ~ type
      ))
    
    plot_ly(agg, x = ~zone, y = ~mean_val, color = ~type_label, type = "bar",
            colors = COLORS$chart_palette,
            text = ~round(mean_val, 1), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>%{data.name}: %{y:.1f}<extra></extra>") %>%
      plotly_layout(title = "Évaluation moyenne par zone et type",
                    xlab = "", ylab = "Valeur moyenne") %>%
      layout(barmode = "group", xaxis = list(tickangle = -45))
  })
  
  output$eval_table <- renderDT({
    df <- filtered_evaluation() %>%
      select(date, zone, localite, enumerator, population, caseid, type, value, message) %>%
      mutate(date = format(date, "%d/%m/%Y"),
             value = round(value, 2))
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Localité", "Enquêteur", "Population",
                           "CaseID", "Type", "Valeur", "Message"),
              class = "compact stripe hover") %>%
      formatStyle("value",
                  background = styleColorBar(range(df$value, na.rm = TRUE), "#E0E7FF"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  #*********************************************************************************
  # === ONGLET ÉVALUATION - SILHOUETTE ====
  #*********************************************************************************
  
  output$silhouette_kpi_avg <- renderUI({
    df <- filtered_silhouette()
    avg_val <- ifelse(nrow(df) > 0, mean(df$silhouette, na.rm = TRUE), 0)
    create_value_box("Score silhouette moyen", fmt_number(avg_val, 3),
                     subtitle = paste(nrow(df), "enquêteurs évalués"),
                     icon = "speedometer2", color = ifelse(avg_val > 0.2, "success", "warning"))
  })
  
  output$silhouette_kpi_warning <- renderUI({
    df <- filtered_silhouette()
    n_warn <- sum(grepl("Warning", df$comment, ignore.case = TRUE), na.rm = TRUE)
    create_value_box("Alertes falsification", n_warn,
                     subtitle = paste("sur", nrow(df), "enquêteurs"),
                     icon = "shield-exclamation", color = ifelse(n_warn > 0, "danger", "success"))
  })
  
  output$silhouette_kpi_interviews <- renderUI({
    df <- filtered_silhouette()
    total <- sum(df$number_interviews, na.rm = TRUE)
    create_value_box("Total interviews", fmt_number(total, 0),
                     subtitle = paste("Moyenne:", fmt_number(mean(df$number_interviews, na.rm = TRUE), 0), "par enquêteur"),
                     icon = "journal-text", color = "info")
  })
  
  output$silhouette_kpi_zones <- renderUI({
    df <- filtered_silhouette()
    col_zone <- if ("zone_name" %in% names(df)) "zone_name" else "zone"
    create_value_box("Zones couvertes", n_distinct(df[[col_zone]]),
                     subtitle = paste("sur", length(FILTERS$zones), "zones"),
                     icon = "geo-alt", color = "primary")
  })
  
  output$silhouette_plot <- renderPlotly({
    df <- filtered_silhouette()
    if (nrow(df) == 0) return(plotly_empty())
    
    df <- df %>% arrange(silhouette)
    col_enum <- if ("enumerator_name" %in% names(df)) "enumerator_name" else "enumerator"
    col_zone <- if ("zone_name" %in% names(df)) "zone_name" else "zone"
    
    plot_ly(df, x = ~silhouette, y = ~reorder(get(col_enum), silhouette),
            type = "bar", orientation = "h",
            color = ~get(col_zone),
            colors = COLORS$chart_palette,
            text = ~paste0(round(silhouette, 3), " (", number_interviews, " int.)"),
            textposition = "outside",
            hovertemplate = paste0("<b>%{y}</b><br>",
                                   "Silhouette: %{x:.3f}<br>",
                                   "<extra></extra>")) %>%
      plotly_layout(title = "Score silhouette par enquêteur",
                    xlab = "Score silhouette", ylab = "") %>%
      layout(shapes = list(
        list(type = "line", x0 = 0.25, x1 = 0.25, y0 = -0.5, y1 = nrow(df) - 0.5,
             line = list(color = COLORS$danger, dash = "dash", width = 2))
      ))
  })
  
  output$silhouette_by_zone <- renderPlotly({
    df <- filtered_silhouette()
    if (nrow(df) == 0) return(plotly_empty())
    
    col_zone <- if ("zone_name" %in% names(df)) "zone_name" else "zone"
    
    plot_ly(df, x = ~get(col_zone), y = ~silhouette, type = "box",
            color = ~get(col_zone), colors = COLORS$chart_palette,
            boxpoints = "all", jitter = 0.3,
            hovertemplate = "<b>%{x}</b><br>Silhouette: %{y:.3f}<extra></extra>",
            showlegend = FALSE) %>%
      plotly_layout(title = "Distribution du score silhouette par zone",
                    xlab = "", ylab = "Score silhouette", showlegend = FALSE)
  })
  
  output$silhouette_scatter <- renderPlotly({
    df <- filtered_silhouette()
    if (nrow(df) == 0) return(plotly_empty())
    
    col_enum <- if ("enumerator_name" %in% names(df)) "enumerator_name" else "enumerator"
    col_zone <- if ("zone_name" %in% names(df)) "zone_name" else "zone"
    
    plot_ly(df, x = ~number_interviews, y = ~silhouette,
            color = ~comment,
            colors = c("Données de qualité acceptable" = COLORS$success,
                       "Warning : Risque de falsification des données" = COLORS$danger),
            type = "scatter", mode = "markers", showlegend = TRUE,
            marker = list(size = 10, opacity = 0.8,
                          line = list(color = "white", width = 1)),
            text = ~get(col_enum),
            hovertemplate = paste0("<b>%{text}</b><br>",
                                   "Interviews: %{x}<br>",
                                   "Silhouette: %{y:.3f}<extra></extra>")) %>%
      plotly_layout(title = "Silhouette vs Nombre d'interviews",
                    xlab = "Nombre d'interviews", ylab = "Score silhouette") %>%
      layout(shapes = list(
        list(type = "line", x0 = 0, x1 = max(df$number_interviews, na.rm = TRUE),
             y0 = 0.25, y1 = 0.25,
             line = list(color = COLORS$danger, dash = "dash", width = 1.5))
      ))
  })
  
  output$silhouette_table <- renderDT({
    df <- filtered_silhouette() %>%
      mutate(silhouette = round(silhouette, 4))
    
    col_names <- c("Zone", "Localité", "Enquêteur", "Nb interviews", "Silhouette", "Commentaire")
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = col_names,
              class = "compact stripe hover") %>%
      formatStyle("comment",
                  backgroundColor = styleEqual(
                    c("Données de qualité acceptable",
                      "Warning : Risque de falsification des données"),
                    c("#D1FAE5", "#FEE2E2"))) %>%
      formatStyle("silhouette",
                  background = styleColorBar(c(0, max(df$silhouette, na.rm = TRUE)), "#DBEAFE"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })
  
  #*********************************************************************************
  # === PERFORMANCE ZONE ====
  #*********************************************************************************
  
  output$perf_zone_kpi1 <- renderUI({
    df <- filtered_perf_zone() %>% filter(type == "tx_progression")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Progression moyenne", fmt_pct(avg_val),
                     subtitle = "Dernière date disponible",
                     icon = "graph-up-arrow", color = ifelse(avg_val >= 100, "success", "warning"))
  })
  
  output$perf_zone_kpi2 <- renderUI({
    df <- filtered_perf_zone() %>% filter(type == "nb_interview")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    total <- sum(latest$value, na.rm = TRUE)
    create_value_box("Interviews (dernière date)", fmt_number(total, 0),
                     subtitle = paste(n_distinct(df$zone), "zones"),
                     icon = "clipboard-data", color = "primary")
  })
  
  output$perf_zone_kpi3 <- renderUI({
    df <- filtered_perf_zone() %>% filter(type == "tx_approbation")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Taux d'approbation moyen", fmt_pct(avg_val),
                     subtitle = "Dernière date disponible",
                     icon = "check2-all", color = ifelse(avg_val >= 90, "success", "danger"))
  })
  
  output$perf_zone_kpi4 <- renderUI({
    df <- filtered_perf_zone() %>% filter(type == "duree_collecte")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Durée collecte moyenne", paste0(fmt_number(avg_val, 0), " min"),
                     subtitle = "Dernière date disponible",
                     icon = "stopwatch", color = "info")
  })
  
  output$perf_zone_trend <- renderPlotly({
    df <- filtered_perf_zone()
    sel_type <- input$perf_zone_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    df_f <- df %>% filter(type == sel_type)
    if (nrow(df_f) == 0) return(plotly_empty())
    
    plot_ly(df_f, x = ~date, y = ~value, color = ~zone, type = "scatter",
            mode = "lines+markers", colors = COLORS$chart_palette,
            hovertemplate = paste0("<b>%{data.name}</b><br>",
                                   "Date: %{x|%d/%m/%Y}<br>",
                                   "Valeur: %{y:.1f}<extra></extra>")) %>%
      plotly_layout(title = paste("Évolution -", get_type_label(sel_type), "par zone"),
                    xlab = "Date", ylab = get_type_label(sel_type))
  })
  
  output$perf_zone_comparison <- renderPlotly({
    df <- filtered_perf_zone()
    sel_type <- input$perf_zone_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    latest <- df %>% filter(type == sel_type, date == max(date, na.rm = TRUE))
    if (nrow(latest) == 0) return(plotly_empty())
    
    plot_ly(latest, x = ~reorder(zone, value), y = ~value, type = "bar",
            marker = list(color = COLORS$chart_palette[1:nrow(latest)],
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~round(value, 1), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Valeur: %{y:.1f}<extra></extra>") %>%
      plotly_layout(title = paste("Comparaison par zone -", get_type_label(sel_type)),
                    xlab = "", ylab = get_type_label(sel_type)) %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$perf_zone_heatmap <- renderPlotly({
    df <- filtered_perf_zone()
    if (nrow(df) == 0) return(plotly_empty())
    
    # Sélectionner les taux principaux
    tx_types <- c("tx_progression", "tx_participation", "tx_couverture",
                   "tx_consentement", "tx_approbation", "tx_incoherence")
    
    latest <- df %>%
      filter(type %in% tx_types, date == max(date, na.rm = TRUE)) %>%
      mutate(type_label = get_type_label(type))
    
    if (nrow(latest) == 0) return(plotly_empty())
    
    mat <- latest %>%
      select(zone, type_label, value) %>%
      pivot_wider(names_from = type_label, values_from = value)
    
    zones_v <- mat$zone
    mat_vals <- as.matrix(mat[, -1])
    
    plot_ly(x = colnames(mat_vals), y = zones_v, z = mat_vals,
            type = "heatmap",
            colorscale = list(c(0, "#FEE2E2"), c(0.5, "#FEF3C7"), c(1, "#D1FAE5")),
            text = round(mat_vals, 1), texttemplate = "%{text}",
            hovertemplate = "<b>%{y}</b><br>%{x}: %{z:.1f}<extra></extra>") %>%
      plotly_layout(title = "Carte thermique des indicateurs par zone",
                    xlab = "", ylab = "") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$perf_zone_table <- renderDT({
    df <- filtered_perf_zone() %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type_label = get_type_label(type),
             value = round(value, 2)) %>%
      select(date, zone, type_label, value, message)
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Indicateur", "Valeur", "Message"),
              class = "compact stripe hover")
  })
  
  #*********************************************************************************
  # === PERFORMANCE LOCALITÉ ====
  #*********************************************************************************
  
  output$perf_loc_kpi1 <- renderUI({
    df <- filtered_perf_localite() %>% filter(type == "tx_progression")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Progression moyenne", fmt_pct(avg_val),
                     subtitle = paste(n_distinct(latest$localite), "localités"),
                     icon = "graph-up-arrow", color = ifelse(avg_val >= 100, "success", "warning"))
  })
  
  output$perf_loc_kpi2 <- renderUI({
    df <- filtered_perf_localite() %>% filter(type == "nb_interview")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    total <- sum(latest$value, na.rm = TRUE)
    create_value_box("Interviews (dernière date)", fmt_number(total, 0),
                     subtitle = paste(n_distinct(df$localite), "localités"),
                     icon = "clipboard-data", color = "primary")
  })
  
  output$perf_loc_kpi3 <- renderUI({
    df <- filtered_perf_localite() %>% filter(type == "tx_approbation")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Taux d'approbation moyen", fmt_pct(avg_val),
                     subtitle = "Dernière date",
                     icon = "check2-all", color = ifelse(avg_val >= 90, "success", "danger"))
  })
  
  output$perf_loc_kpi4 <- renderUI({
    df <- filtered_perf_localite() %>% filter(type == "duree_collecte")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Durée collecte moyenne", paste0(fmt_number(avg_val, 0), " min"),
                     subtitle = "Dernière date",
                     icon = "stopwatch", color = "info")
  })
  
  output$perf_loc_trend <- renderPlotly({
    df <- filtered_perf_localite()
    sel_type <- input$perf_loc_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    df_f <- df %>% filter(type == sel_type)
    if (nrow(df_f) == 0) return(plotly_empty())
    
    plot_ly(df_f, x = ~date, y = ~value, color = ~localite, type = "scatter",
            mode = "lines+markers", colors = COLORS$chart_palette,
            hovertemplate = paste0("<b>%{data.name}</b><br>",
                                   "Date: %{x|%d/%m/%Y}<br>",
                                   "Valeur: %{y:.1f}<extra></extra>")) %>%
      plotly_layout(title = paste("Évolution -", get_type_label(sel_type), "par localité"),
                    xlab = "Date", ylab = get_type_label(sel_type))
  })
  
  output$perf_loc_comparison <- renderPlotly({
    df <- filtered_perf_localite()
    sel_type <- input$perf_loc_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    latest <- df %>% filter(type == sel_type, date == max(date, na.rm = TRUE))
    if (nrow(latest) == 0) return(plotly_empty())
    
    plot_ly(latest, x = ~reorder(localite, value), y = ~value, type = "bar",
            marker = list(color = COLORS$chart_palette[1:min(nrow(latest), 15)],
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~round(value, 1), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Valeur: %{y:.1f}<extra></extra>") %>%
      plotly_layout(title = paste("Comparaison par localité -", get_type_label(sel_type)),
                    xlab = "", ylab = get_type_label(sel_type)) %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$perf_loc_heatmap <- renderPlotly({
    df <- filtered_perf_localite()
    if (nrow(df) == 0) return(plotly_empty())
    
    tx_types <- c("tx_progression", "tx_participation", "tx_couverture",
                   "tx_consentement", "tx_approbation", "tx_incoherence")
    
    latest <- df %>%
      filter(type %in% tx_types, date == max(date, na.rm = TRUE)) %>%
      mutate(type_label = get_type_label(type))
    
    if (nrow(latest) == 0) return(plotly_empty())
    
    mat <- latest %>%
      select(localite, type_label, value) %>%
      pivot_wider(names_from = type_label, values_from = value)
    
    locs <- mat$localite
    mat_vals <- as.matrix(mat[, -1])
    
    plot_ly(x = colnames(mat_vals), y = locs, z = mat_vals,
            type = "heatmap",
            colorscale = list(c(0, "#FEE2E2"), c(0.5, "#FEF3C7"), c(1, "#D1FAE5")),
            text = round(mat_vals, 1), texttemplate = "%{text}",
            hovertemplate = "<b>%{y}</b><br>%{x}: %{z:.1f}<extra></extra>") %>%
      plotly_layout(title = "Carte thermique des indicateurs par localité",
                    xlab = "", ylab = "") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$perf_loc_table <- renderDT({
    df <- filtered_perf_localite() %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type_label = get_type_label(type),
             value = round(value, 2)) %>%
      select(date, zone, localite, type_label, value, message)
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Localité", "Indicateur", "Valeur", "Message"),
              class = "compact stripe hover")
  })
  
  #*********************************************************************************
  # === PERFORMANCE ENQUÊTEUR ====
  #*********************************************************************************
  
  output$perf_enum_kpi1 <- renderUI({
    df <- filtered_perf_enum()
    create_value_box("Enquêteurs suivis", n_distinct(df$enumerator),
                     subtitle = paste("sur", length(FILTERS$enumerators), "enquêteurs"),
                     icon = "people-fill", color = "primary")
  })
  
  output$perf_enum_kpi2 <- renderUI({
    df <- filtered_perf_enum() %>% filter(type == "nb_interview")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    total <- sum(latest$value, na.rm = TRUE)
    create_value_box("Interviews (dernière date)", fmt_number(total, 0),
                     subtitle = paste(n_distinct(latest$enumerator), "enquêteurs actifs"),
                     icon = "journal-check", color = "success")
  })
  
  output$perf_enum_kpi3 <- renderUI({
    df <- filtered_perf_enum() %>% filter(type == "tx_incoherence")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Incohérence moyenne", fmt_pct(avg_val),
                     subtitle = "Dernière date",
                     icon = "exclamation-diamond", color = ifelse(avg_val > 50, "danger", "success"))
  })
  
  output$perf_enum_kpi4 <- renderUI({
    df <- filtered_perf_enum() %>% filter(type == "duree_collecte")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Durée collecte moyenne", paste0(fmt_number(avg_val, 0), " min"),
                     subtitle = "Dernière date",
                     icon = "stopwatch", color = "info")
  })
  
  output$perf_enum_trend <- renderPlotly({
    df <- filtered_perf_enum()
    sel_type <- input$perf_enum_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    df_f <- df %>% filter(type == sel_type)
    if (nrow(df_f) == 0) return(plotly_empty())
    
    # Limiter à 10 enquêteurs pour la lisibilité
    top_enum <- df_f %>%
      group_by(enumerator) %>%
      summarise(avg = mean(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(avg)) %>%
      head(10)
    
    df_f <- df_f %>% filter(enumerator %in% top_enum$enumerator)
    
    plot_ly(df_f, x = ~date, y = ~value, color = ~enumerator, type = "scatter",
            mode = "lines+markers", colors = COLORS$chart_palette,
            hovertemplate = paste0("<b>%{data.name}</b><br>",
                                   "Date: %{x|%d/%m/%Y}<br>",
                                   "Valeur: %{y:.1f}<extra></extra>")) %>%
      plotly_layout(title = paste("Évolution -", get_type_label(sel_type), "(Top 10 enquêteurs)"),
                    xlab = "Date", ylab = get_type_label(sel_type))
  })
  
  output$perf_enum_comparison <- renderPlotly({
    df <- filtered_perf_enum()
    sel_type <- input$perf_enum_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    latest <- df %>% filter(type == sel_type, date == max(date, na.rm = TRUE)) %>%
      arrange(desc(value)) %>% head(20)
    if (nrow(latest) == 0) return(plotly_empty())
    
    plot_ly(latest, x = ~value, y = ~reorder(enumerator, value), type = "bar",
            orientation = "h",
            marker = list(color = COLORS$chart_palette[1:min(nrow(latest), 15)],
                          line = list(color = COLORS$dark, width = 0.3)),
            text = ~round(value, 1), textposition = "outside",
            hovertemplate = "<b>%{y}</b><br>Valeur: %{x:.1f}<extra></extra>") %>%
      plotly_layout(title = paste("Top 20 enquêteurs -", get_type_label(sel_type)),
                    xlab = get_type_label(sel_type), ylab = "")
  })
  
  output$perf_enum_radar <- renderPlotly({
    df <- filtered_perf_enum()
    if (nrow(df) == 0) return(plotly_empty())
    
    tx_types <- c("tx_progression", "tx_participation", "tx_approbation",
                   "tx_incoherence", "tx_missing", "tx_other")
    
    latest <- df %>%
      filter(type %in% tx_types, date == max(date, na.rm = TRUE)) %>%
      group_by(type) %>%
      summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(type_label = get_type_label(type))
    
    if (nrow(latest) == 0) return(plotly_empty())
    
    plot_ly(type = "scatterpolar",
            r = c(latest$mean_val, latest$mean_val[1]),
            theta = c(latest$type_label, latest$type_label[1]),
            fill = "toself",
            fillcolor = "rgba(37, 99, 235, 0.2)",
            line = list(color = COLORS$primary, width = 2),
            marker = list(size = 6, color = COLORS$primary)) %>%
      plotly_layout(title = "Profil moyen des enquêteurs", showlegend = FALSE) %>%
      layout(polar = list(
        radialaxis = list(visible = TRUE, range = c(0, 100)),
        angularaxis = list(tickfont = list(size = 10))
      ))
  })
  
  output$perf_enum_table <- renderDT({
    df <- filtered_perf_enum() %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type_label = get_type_label(type),
             value = round(value, 2)) %>%
      select(date, zone, enumerator, type_label, value, message)
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Zone", "Enquêteur", "Indicateur", "Valeur", "Message"),
              class = "compact stripe hover")
  })
  
  #*********************************************************************************
  # === PERFORMANCE POPULATION ====
  #*********************************************************************************
  
  output$perf_pop_kpi1 <- renderUI({
    df <- filtered_perf_pop() %>% filter(type == "tx_progression")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Progression moyenne", fmt_pct(avg_val),
                     subtitle = paste(n_distinct(latest$population), "populations"),
                     icon = "graph-up-arrow", color = ifelse(avg_val >= 100, "success", "warning"))
  })
  
  output$perf_pop_kpi2 <- renderUI({
    df <- filtered_perf_pop() %>% filter(type == "nb_interview")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    total <- sum(latest$value, na.rm = TRUE)
    create_value_box("Interviews (dernière date)", fmt_number(total, 0),
                     subtitle = paste(n_distinct(df$population), "populations"),
                     icon = "clipboard-data", color = "primary")
  })
  
  output$perf_pop_kpi3 <- renderUI({
    df <- filtered_perf_pop() %>% filter(type == "tx_approbation")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Taux d'approbation moyen", fmt_pct(avg_val),
                     subtitle = "Dernière date",
                     icon = "check2-all", color = ifelse(avg_val >= 90, "success", "danger"))
  })
  
  output$perf_pop_kpi4 <- renderUI({
    df <- filtered_perf_pop() %>% filter(type == "duree_collecte")
    latest <- df %>% filter(date == max(date, na.rm = TRUE))
    avg_val <- ifelse(nrow(latest) > 0, mean(latest$value, na.rm = TRUE), 0)
    create_value_box("Durée collecte moyenne", paste0(fmt_number(avg_val, 0), " min"),
                     subtitle = "Dernière date",
                     icon = "stopwatch", color = "info")
  })
  
  output$perf_pop_trend <- renderPlotly({
    df <- filtered_perf_pop()
    sel_type <- input$perf_pop_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    df_f <- df %>% filter(type == sel_type)
    if (nrow(df_f) == 0) return(plotly_empty())
    
    plot_ly(df_f, x = ~date, y = ~value, color = ~population, type = "scatter",
            mode = "lines+markers", colors = COLORS$chart_palette,
            hovertemplate = paste0("<b>%{data.name}</b><br>",
                                   "Date: %{x|%d/%m/%Y}<br>",
                                   "Valeur: %{y:.1f}<extra></extra>")) %>%
      plotly_layout(title = paste("Évolution -", get_type_label(sel_type), "par population"),
                    xlab = "Date", ylab = get_type_label(sel_type))
  })
  
  output$perf_pop_comparison <- renderPlotly({
    df <- filtered_perf_pop()
    sel_type <- input$perf_pop_type
    if (is.null(sel_type) || nrow(df) == 0) return(plotly_empty())
    
    latest <- df %>% filter(type == sel_type, date == max(date, na.rm = TRUE))
    if (nrow(latest) == 0) return(plotly_empty())
    
    plot_ly(latest, x = ~reorder(population, value), y = ~value, type = "bar",
            marker = list(color = COLORS$chart_palette[1:nrow(latest)],
                          line = list(color = COLORS$dark, width = 0.5)),
            text = ~round(value, 1), textposition = "outside",
            hovertemplate = "<b>%{x}</b><br>Valeur: %{y:.1f}<extra></extra>") %>%
      plotly_layout(title = paste("Comparaison par population -", get_type_label(sel_type)),
                    xlab = "", ylab = get_type_label(sel_type)) %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$perf_pop_heatmap <- renderPlotly({
    df <- filtered_perf_pop()
    if (nrow(df) == 0) return(plotly_empty())
    
    tx_types <- c("tx_progression", "tx_participation", "tx_couverture",
                   "tx_consentement", "tx_approbation", "tx_incoherence")
    
    latest <- df %>%
      filter(type %in% tx_types, date == max(date, na.rm = TRUE)) %>%
      mutate(type_label = get_type_label(type))
    
    if (nrow(latest) == 0) return(plotly_empty())
    
    mat <- latest %>%
      select(population, type_label, value) %>%
      pivot_wider(names_from = type_label, values_from = value)
    
    pops <- mat$population
    mat_vals <- as.matrix(mat[, -1])
    
    plot_ly(x = colnames(mat_vals), y = pops, z = mat_vals,
            type = "heatmap",
            colorscale = list(c(0, "#FEE2E2"), c(0.5, "#FEF3C7"), c(1, "#D1FAE5")),
            text = round(mat_vals, 1), texttemplate = "%{text}",
            hovertemplate = "<b>%{y}</b><br>%{x}: %{z:.1f}<extra></extra>") %>%
      plotly_layout(title = "Carte thermique des indicateurs par population",
                    xlab = "", ylab = "") %>%
      layout(xaxis = list(tickangle = -45))
  })
  
  output$perf_pop_table <- renderDT({
    df <- filtered_perf_pop() %>%
      mutate(date = format(date, "%d/%m/%Y"),
             type_label = get_type_label(type),
             value = round(value, 2)) %>%
      select(date, population, type_label, value, message)
    
    datatable(df, options = dt_options(), rownames = FALSE,
              colnames = c("Date", "Population", "Indicateur", "Valeur", "Message"),
              class = "compact stripe hover")
  })
  
}
