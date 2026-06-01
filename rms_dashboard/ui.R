#*********************************************************************************
# ui.R - RMS Data Collection Monitoring Dashboard
#*********************************************************************************
# Interface utilisateur Shiny standard (compatible Shiny classique)
#*********************************************************************************

fluidPage(
  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#2563EB",
    secondary = "#7C3AED",
    success = "#059669",
    warning = "#D97706",
    danger = "#DC2626",
    "font-family-base" = "'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
    "border-radius" = "0.75rem"
  ),
  
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap",
              rel = "stylesheet"),
    tags$style(HTML("
      body { background-color: #F1F5F9; }
      .navbar { background: linear-gradient(135deg, #2563EB, #7C3AED) !important; box-shadow: 0 2px 10px rgba(0,0,0,0.15); }
      .card { border: none; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.08); margin-bottom: 16px; }
      .card:hover { box-shadow: 0 4px 12px rgba(0,0,0,0.12); }
      .nav-tabs .nav-link { font-weight: 600; color: #94A3B8; border: none; padding: 10px 20px; }
      .nav-tabs .nav-link.active { color: #2563EB; border-bottom: 3px solid #2563EB; background: transparent; }
      .sidebar { background-color: white !important; border-right: 1px solid #E2E8F0; }
      .well { background-color: white; border: 1px solid #E2E8F0; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); }
      h4 { color: #1E293B; font-weight: 700; border-bottom: 2px solid #2563EB; padding-bottom: 8px; }
      .dataTables_wrapper { font-size: 0.85rem; }
      table.dataTable thead th { background-color: #F8FAFC; font-weight: 600; border-bottom: 2px solid #2563EB !important; }
      table.dataTable tbody tr:hover { background-color: #EFF6FF !important; }
      .footer { text-align: center; color: #94A3B8; font-size: 0.8rem; padding: 16px; margin-top: 24px; }
      .kpi-box { padding: 16px; border-radius: 12px; color: white; text-align: center; }
      .kpi-value { font-size: 2.5rem; font-weight: 700; }
      .kpi-label { font-size: 0.9rem; opacity: 0.9; }
    "))
  ),
  
  titlePanel(
    div(
      style = "background: linear-gradient(135deg, #2563EB, #7C3AED); color: white; padding: 15px 25px; margin: -15px -15px 20px -15px; border-radius: 0 0 12px 12px;",
      h3("RMS - Tableau de Bord de Suivi de la Collecte", style = "margin: 25px 0 0 0; font-weight: 700;"),
      p(textOutput("header_date"), style = "margin: 5px 0px 0px 0px; opacity: 0.9; font-size: 0.9rem;")
    ),
    windowTitle = "RMS Monitoring Dashboard"
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtres globaux"),
      dateRangeInput("date_range", "Période",
                     # start = Sys.Date() - 30, end = Sys.Date(),
                     start = FILTERS$date_min, end = FILTERS$date_max,
                     format = "dd/mm/yyyy", language = "fr", separator = " au "),
      # selectInput("filter_zone", "Zone", choices = FILTERS$zones,
      #             selected = NULL, multiple = TRUE),
      #             # selected = FILTERS$zones, multiple = TRUE),
      pickerInput("filter_zone", "Zone", 
                  choices = FILTERS$zones,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} zones sélectionnées",
                    `none-selected-text` = "Aucune zone sélectionnée"
                  )),
      # selectInput("filter_localite", "Localité", choices = FILTERS$localites,
      #             selected = NULL, multiple = TRUE),
      #             # selected = FILTERS$localites, multiple = TRUE),
      pickerInput("filter_localite", "Localité", 
                  choices = FILTERS$localites,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} localités sélectionnées",
                    `none-selected-text` = "Aucune localité sélectionnée"
                  )),
      # selectInput("filter_enumerator", "Enquêteur", choices = FILTERS$enumerators,
      #             selected = NULL, multiple = TRUE),
      #             # selected = FILTERS$enumerators, multiple = TRUE),
      pickerInput("filter_enumerator", "Enquêteur", 
                  choices = FILTERS$enumerators,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} enquêteurs sélectionnés",
                    `none-selected-text` = "Aucun enquêteur sélectionné"
                  )),
      # selectInput("filter_population", "Population", choices = FILTERS$populations,
      #             selected = NULL, multiple = TRUE),
      #             # selected = FILTERS$populations, multiple = TRUE),
      pickerInput("filter_population", "Population", 
                  choices = FILTERS$populations,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} populations sélectionnées",
                    `none-selected-text` = "Aucune population sélectionnée"
                  )),
      hr(),
      actionButton("reset_filters", "Réinitialiser les filtres", 
                   icon = icon("refresh"),
                   class = "btn btn-secondary btn-sm",
                   style = "width: 100%;"),
      div(class = "footer",
          tags$strong("RMS Monitoring Dashboard"),
          tags$br(),
          tags$em(paste(APP_VERSION," |", format(Sys.Date(), "%Y")))
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs", type = "tabs",
        
        # === ONGLET ÉVALUATION ===
        tabPanel("Évaluation",
          tabsetPanel(
            id = "eval_tabs", type = "pills",
            
            # --- Missing ---
            tabPanel("Missing", br(),
              fluidRow(
                column(3, uiOutput("missing_kpi_total")),
                column(3, uiOutput("missing_kpi_types")),
                column(3, uiOutput("missing_kpi_enum")),
                column(3, uiOutput("missing_kpi_treated"))
              ),
              fluidRow(
                column(12, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_by_zone", height = "400px")))),
                # column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_by_type", height = "350px"))))
              ),
              fluidRow(
                # column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_by_zone", height = "350px")))),
                column(12, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_by_type", height = "400px"))))
              ),
              fluidRow(
                column(12, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_trend", height = "400px")))),
                # column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_by_enumerator", height = "320px"))))
              ),
              fluidRow(
                # column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_trend", height = "320px")))),
                column(12, div(class = "card", div(class = "card-body", plotly::plotlyOutput("missing_by_enumerator", height = "400px"))))
              ),
              div(class = "card", div(class = "card-body", h5("Détail des valeurs manquantes"), DT::DTOutput("missing_table")))
            ),
            
            # --- Error ---
            tabPanel("Error", br(),
              fluidRow(
                column(3, uiOutput("error_kpi_total")),
                column(3, uiOutput("error_kpi_types")),
                column(3, uiOutput("error_kpi_enum")),
                column(3, uiOutput("error_kpi_treated"))
              ),
              fluidRow(
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("error_by_zone", height = "400px")))),
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("error_by_type", height = "400px"))))
              ),
              fluidRow(
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("error_trend", height = "400px")))),
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("error_by_enumerator", height = "400px"))))
              ),
              div(class = "card", div(class = "card-body", h5("Détail des erreurs"), DT::DTOutput("error_table")))
            ),
            
            # --- Other ---
            tabPanel("Other", br(),
              fluidRow(
                column(3, uiOutput("other_kpi_total")),
                column(3, uiOutput("other_kpi_types")),
                column(3, uiOutput("other_kpi_enum")),
                column(3, uiOutput("other_kpi_treated"))
              ),
              fluidRow(
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("other_by_zone", height = "400px")))),
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("other_by_type", height = "400px"))))
              ),
              div(class = "card", div(class = "card-body", plotly::plotlyOutput("other_trend", height = "400px"))),
              div(class = "card", div(class = "card-body", h5("Détail des valeurs 'Autre'"), DT::DTOutput("other_table")))
            ),
            
            # --- Evaluation ---
            tabPanel("Evaluation", br(),
              fluidRow(
                column(3, uiOutput("eval_kpi_total")),
                column(3, uiOutput("eval_kpi_incoherence")),
                column(3, uiOutput("eval_kpi_duree")),
                column(3, uiOutput("eval_kpi_missing_intra"))
              ),
              fluidRow(
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("eval_incoherence_dist", height = "400px")))),
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("eval_duree_dist", height = "400px"))))
              ),
              fluidRow(
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("eval_by_enumerator", height = "450px")))),
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("eval_by_zone", height = "450px"))))
              ),
              div(class = "card", div(class = "card-body", h5("Détail des évaluations"), DT::DTOutput("eval_table")))
            ),
            
            # --- Silhouette ---
            tabPanel("Silhouette", br(),
              fluidRow(
                column(3, uiOutput("silhouette_kpi_avg")),
                column(3, uiOutput("silhouette_kpi_warning")),
                column(3, uiOutput("silhouette_kpi_interviews")),
                column(3, uiOutput("silhouette_kpi_zones"))
              ),
              div(class = "card", div(class = "card-body", plotly::plotlyOutput("silhouette_plot", height = "600px"))),
              fluidRow(
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("silhouette_by_zone", height = "400px")))),
                column(6, div(class = "card", div(class = "card-body", plotly::plotlyOutput("silhouette_scatter", height = "400px"))))
              ),
              div(class = "card", div(class = "card-body", h5("Détail des scores silhouette"), DT::DTOutput("silhouette_table")))
            )
          )
        ),
        
        # === ONGLET PERFORMANCE ZONE ===
        tabPanel("Performance Zone", br(),
          fluidRow(
            column(3, uiOutput("perf_zone_kpi1")),
            column(3, uiOutput("perf_zone_kpi2")),
            column(3, uiOutput("perf_zone_kpi3")),
            column(3, uiOutput("perf_zone_kpi4"))
          ),
          # selectInput("perf_zone_type", "Indicateur",
          #             choices = c("Taux de progression", "Interviews (dernière date)", "Taux d'approbation", "Durée collecte"),
          #             selected = "Taux de progression"),
          selectInput("perf_zone_type", "Indicateur",
                      choices = c(
                        "Taux de progression"       = "tx_progression",
                        "Interviews (dernière date)" = "nb_interview",
                        "Taux d'approbation"         = "tx_approbation",
                        "Durée collecte"              = "duree_collecte"
                      ),
                      selected = "tx_progression"),
          fluidRow(
            column(7, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_zone_trend", height = "400px")))),
            column(5, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_zone_comparison", height = "400px"))))
          ),
          div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_zone_heatmap", height = "400px"))),
          div(class = "card", div(class = "card-body", h5("Données détaillées"), DT::DTOutput("perf_zone_table")))
        ),
        
        # === ONGLET PERFORMANCE LOCALITÉ ===
        tabPanel("Performance Localité", br(),
          fluidRow(
            column(3, uiOutput("perf_loc_kpi1")),
            column(3, uiOutput("perf_loc_kpi2")),
            column(3, uiOutput("perf_loc_kpi3")),
            column(3, uiOutput("perf_loc_kpi4"))
          ),
          # selectInput("perf_loc_type", "Indicateur",
          #             choices = c("Taux de progression", "Interviews (dernière date)", "Taux d'approbation", "Durée collecte"),
          #             selected = "Taux de progression"),
          selectInput("perf_loc_type", "Indicateur",
                      choices = c(
                        "Taux de progression"       = "tx_progression",
                        "Interviews (dernière date)" = "nb_interview",
                        "Taux d'approbation"         = "tx_approbation",
                        "Durée collecte"              = "duree_collecte"
                      ),
                      selected = "tx_progression"),
          fluidRow(
            column(7, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_loc_trend", height = "400px")))),
            column(5, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_loc_comparison", height = "400px"))))
          ),
          div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_loc_heatmap", height = "400px"))),
          div(class = "card", div(class = "card-body", h5("Données détaillées"), DT::DTOutput("perf_loc_table")))
        ),
        
        # === ONGLET PERFORMANCE ENQUÊTEUR ===
        tabPanel("Performance Enquêteur", br(),
          fluidRow(
            column(3, uiOutput("perf_enum_kpi1")),
            column(3, uiOutput("perf_enum_kpi2")),
            column(3, uiOutput("perf_enum_kpi3")),
            column(3, uiOutput("perf_enum_kpi4"))
          ),
          # selectInput("perf_enum_type", "Indicateur",
          #             choices = c("Taux de progression", "Interviews (dernière date)", "Taux d'approbation", "Durée collecte"),
          #             selected = "Taux de progression"),
          selectInput("perf_enum_type", "Indicateur",
                      choices = c(
                        "Taux de progression"       = "tx_progression",
                        "Interviews (dernière date)" = "nb_interview",
                        "Taux d'approbation"         = "tx_approbation",
                        "Durée collecte"              = "duree_collecte"
                      ),
                      selected = "tx_progression"),
          fluidRow(
            column(7, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_enum_trend", height = "400px")))),
            column(5, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_enum_comparison", height = "400px"))))
          ),
          div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_enum_radar", height = "400px"))),
          div(class = "card", div(class = "card-body", h5("Données détaillées"), DT::DTOutput("perf_enum_table")))
        ),
        
        # === ONGLET PERFORMANCE POPULATION ===
        tabPanel("Performance Population", br(),
          fluidRow(
            column(3, uiOutput("perf_pop_kpi1")),
            column(3, uiOutput("perf_pop_kpi2")),
            column(3, uiOutput("perf_pop_kpi3")),
            column(3, uiOutput("perf_pop_kpi4"))
          ),
          # selectInput("perf_pop_type", "Indicateur",
          #             choices = c("Taux de progression", "Interviews (dernière date)", "Taux d'approbation", "Durée collecte"),
          #             selected = "Taux de progression"),
          selectInput("perf_pop_type", "Indicateur",
                      choices = c(
                        "Taux de progression"       = "tx_progression",
                        "Interviews (dernière date)" = "nb_interview",
                        "Taux d'approbation"         = "tx_approbation",
                        "Durée collecte"              = "duree_collecte"
                      ),
                      selected = "tx_progression"),
          fluidRow(
            column(7, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_pop_trend", height = "400px")))),
            column(5, div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_pop_comparison", height = "400px"))))
          ),
          div(class = "card", div(class = "card-body", plotly::plotlyOutput("perf_pop_heatmap", height = "400px"))),
          div(class = "card", div(class = "card-body", h5("Données détaillées"), DT::DTOutput("perf_pop_table")))
        )
      )
    )
  ),
  
  div(class = "footer",
      HTML("<strong>RMS Monitoring Dashboard</strong> | Développé pour le suivi de la collecte de données | Source : rms_monitoring_datamanager.xlsx / rms23426_data.rds")
  )
)
