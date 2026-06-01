#*********************************************************************************
# global.R - RMS Data Collection Monitoring Dashboard
#*********************************************************************************
# Ce fichier charge les données, les packages et définit les fonctions utilitaires
# pour l'ensemble de l'application Shiny.
#*********************************************************************************

# --- Chargement des packages --------------------------------------------------
library(shiny)
library(shinyWidgets)
library(bslib)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(plotly)
library(DT)
library(scales)
library(stringr)
library(bsicons)
library(htmltools)

# --- Configuration globale ----------------------------------------------------
APP_TITLE <- "RMS - Tableau de Bord de Suivi de la Collecte"
APP_VERSION <- "v1.0.0"

# Palette de couleurs professionnelle
COLORS <- list(
  primary    = "#2563EB",
  secondary  = "#7C3AED",
  success    = "#059669",
  warning    = "#D97706",
  danger     = "#DC2626",
  info       = "#0891B2",
  dark       = "#1E293B",
  light      = "#F8FAFC",
  muted      = "#94A3B8",
  bg_card    = "#FFFFFF",
  bg_page    = "#F1F5F9",
  chart_palette = c("#2563EB", "#7C3AED", "#059669", "#D97706", "#DC2626",
                     "#0891B2", "#EC4899", "#F59E0B", "#10B981", "#6366F1",
                     "#EF4444", "#14B8A6", "#F97316", "#8B5CF6", "#06B6D4")
)

# Configuration pour lire les données à jour depuis le github public
REPO_URL <- Sys.getenv("GITHUB_REPO_URL")
user_name <- Sys.getenv("USER_NAME")
user_email <- Sys.getenv("USER_EMAIL")
token <- Sys.getenv("GITHUB_PAT")

# --- Chargement des données ---------------------------------------------------



# Charger depuis le fichier Excel local
load_data <- function() {
  
  # Fonction de chargement des données local
  load_localData <- function(){
    # Chemins possibles pour le fichier Excel
    possible_paths <- c(
      "rms_monitoring_datamanager.xlsx",
      "./rms_monitoring_datamanager.xlsx",
      "../rms_dashboard/rms_monitoring_datamanager.xlsx",
      "../upload/rms_monitoring_datamanager.xlsx",
      "data/rms_monitoring_datamanager.xlsx"
    )
    
    local_path <- NULL
    for (path in possible_paths) {
      if (file.exists(path)) {
        local_path <- path
        break
      }
    }
    
    if (is.null(local_path)) {
      stop("Fichier Excel non trouvé. Assurez-vous que 'rms_monitoring_datamanager.xlsx' est dans le répertoire de l'application.")
    }
    
    cat("  Chargement depuis:", local_path, "\n")
    
    data <- list()
    
    tryCatch({
      # Charger les feuilles depuis Excel
      data$missing       <- as.data.frame(read_excel(local_path, sheet = "missing"))
      data$error         <- as.data.frame(read_excel(local_path, sheet = "error"))
      data$other         <- as.data.frame(read_excel(local_path, sheet = "other"))
      data$evaluation    <- as.data.frame(read_excel(local_path, sheet = "evaluation"))
      data$perf_zone     <- as.data.frame(read_excel(local_path, sheet = "performance_zone"))
      data$perf_localite <- as.data.frame(read_excel(local_path, sheet = "performance_localite"))
      data$perf_enum     <- as.data.frame(read_excel(local_path, sheet = "performance_enumerator"))
      data$perf_pop      <- as.data.frame(read_excel(local_path, sheet = "performance_population"))
      data$silhouette    <- as.data.frame(read_excel(local_path, sheet = "silhouette"))
      
      cat("  ✓ Données chargées avec succès\n")
    }, error = function(e) {
      stop(paste("Erreur lors du chargement des données:", e$message))
    })
    
    invisible(data)
  }
  
  #* [Download le fichier depuis le github public]
  # Pour lire des fichiers bruts sur GitHub sans utiliser la structure de board pins,
  # on peut utiliser board_url() en pointant vers les fichiers bruts (raw).
  # Exemple : Lecture d'un fichier spécifique nommé 'rms23526_data.rds'
  repo_owner <- "charlesmoute"
  repo_name <- "datasets"
  file_name_to_read <- "rms23526_data.rds"
  raw_url <- sprintf("https://raw.githubusercontent.com/%s/%s/main/%s",
                     repo_owner, repo_name, file_name_to_read)
  # Espace de stockage des données (board)
  board <- pins::board_url(c(my_data = raw_url))
  
  # Téléchargement du fichier
  message(sprintf("Téléchargement du fichier via board_url : %s", file_name_to_read))
  tryCatch(
    {
      temp_file_path <- pins::pin_download(board, "my_data")
      gh_data_load <- 1
    }, error = function(e) {
      gh_data_load <- 0
      message(paste("Erreur lors du chargement des données:", e$message))
    }
  )
  
  if(gh_data_load){
    # Données disponibles sur l'espace de stockage public. On essaye de le lire
    tmp_data <- readRDS(temp_file_path)
    if (is.null(tmp_data)) {
      # Données non disponibles sur l'espace de stockage public. 
      # On essaye de lire en local
      data <- load_localData()
    }else{
      data <- list()
      tryCatch({
        # Charger les feuilles depuis Excel
        data$missing        <- openxlsx::read.xlsx(tmp_data, sheet = "missing") %>% as_tibble()
        data$error          <- openxlsx::read.xlsx(tmp_data, sheet = "error") %>% as_tibble()
        data$other          <- openxlsx::read.xlsx(tmp_data, sheet = "other") %>% as_tibble()
        data$evaluation     <- openxlsx::read.xlsx(tmp_data, sheet = "evaluation") %>% as_tibble()
        data$perf_zone      <- openxlsx::read.xlsx(tmp_data, sheet = "performance_zone") %>% as_tibble()
        data$perf_localite  <- openxlsx::read.xlsx(tmp_data, sheet = "performance_localite") %>% as_tibble()
        data$perf_enum      <- openxlsx::read.xlsx(tmp_data, sheet = "performance_enumerator") %>% as_tibble()
        data$perf_pop       <- openxlsx::read.xlsx(tmp_data, sheet = "performance_population") %>% as_tibble()
        data$silhouette     <- openxlsx::read.xlsx(tmp_data, sheet = "silhouette") %>% as_tibble()
        
        # Nettoyage et formatage des dates
        for (nm in names(data)) {
          if ("date" %in% names(data[[nm]])) {
            # Excel : les dates sont stockées comme des nombres (le nombre de jours depuis le 30 décembre 1899 pour Windows ou 1904 pour Mac), 
            # Et la conversion peut être affectée par l'origine choisie.
            # Pour Excel Windows (par défaut)
            data[[nm]]$date <- as.Date(data[[nm]]$date,origin = "1899-12-30")
            # # Pour Excel MAC 
            # data[[nm]]$date <- as.Date(data[[nm]]$date,origin = "1904-01-01")
          }
        }
        
        # cat("  ✓ Données chargées avec succès\n")
        # message("  ✓ Données chargées avec succès\n")
        cli::cli_alert_success("Données chargées avec succès")
      }, error = function(e) {
        # cli::cli_alert_warning(paste("Erreur lors du chargement des données:", e$message))
        # cli::cli_alert_danger(paste("Erreur lors du chargement des données:", e$message))
        # warning(paste("Erreur lors du chargement des données:", e$message))
        stop(paste("Erreur lors du chargement des données:", e$message))
      })
    }
  }else{
    # Données non disponibles sur l'espace de stockage public. 
    # On essaye de lire localement les données
    data <- load_localData()
  }
  # rm(repo_owner,repo_name,file_name_to_read,raw_url,temp_file_path,tmp_data)
  return(data)
}

DATA <- load_data()

# --- Extraction des valeurs de filtres ----------------------------------------
# get_filter_values <- function() {
#   
#   # Dates
#   all_dates <- c()
#   for (nm in names(DATA)) {
#     if ("date" %in% names(DATA[[nm]])) {
#       all_dates <- c(all_dates, DATA[[nm]]$date)
#     }
#   }
#   all_dates <- as.Date(all_dates, origin = "1970-01-01")
#   all_dates <- all_dates[!is.na(all_dates)]
#   
#   # Zones
#   zones <- unique(c(
#     if ("zone" %in% names(DATA$missing)) DATA$missing$zone else NULL,
#     if ("zone" %in% names(DATA$error)) DATA$error$zone else NULL,
#     if ("zone" %in% names(DATA$other)) DATA$other$zone else NULL,
#     if ("zone" %in% names(DATA$evaluation)) DATA$evaluation$zone else NULL,
#     if ("zone" %in% names(DATA$perf_zone)) DATA$perf_zone$zone else NULL,
#     if ("zone" %in% names(DATA$perf_localite)) DATA$perf_localite$zone else NULL,
#     if ("zone" %in% names(DATA$perf_enum)) DATA$perf_enum$zone else NULL,
#     if ("zone_name" %in% names(DATA$silhouette)) DATA$silhouette$zone_name else NULL
#   ))
#   zones <- sort(zones[!is.na(zones)])
#   
#   # Localités
#   localites <- unique(c(
#     if ("localite" %in% names(DATA$missing)) DATA$missing$localite else NULL,
#     if ("localite" %in% names(DATA$error)) DATA$error$localite else NULL,
#     if ("localite" %in% names(DATA$other)) DATA$other$localite else NULL,
#     if ("localite" %in% names(DATA$evaluation)) DATA$evaluation$localite else NULL,
#     if ("localite" %in% names(DATA$perf_localite)) DATA$perf_localite$localite else NULL,
#     if ("localite_name" %in% names(DATA$silhouette)) DATA$silhouette$localite_name else NULL
#   ))
#   localites <- sort(localites[!is.na(localites)])
#   
#   # Enquêteurs
#   enumerators <- unique(c(
#     if ("enumerator" %in% names(DATA$missing)) DATA$missing$enumerator else NULL,
#     if ("enumerator" %in% names(DATA$error)) DATA$error$enumerator else NULL,
#     if ("enumerator" %in% names(DATA$other)) DATA$other$enumerator else NULL,
#     if ("enumerator" %in% names(DATA$evaluation)) DATA$evaluation$enumerator else NULL,
#     if ("enumerator" %in% names(DATA$perf_enum)) DATA$perf_enum$enumerator else NULL,
#     if ("enumerator_name" %in% names(DATA$silhouette)) DATA$silhouette$enumerator_name else NULL
#   ))
#   enumerators <- sort(enumerators[!is.na(enumerators)])
#   
#   # Populations
#   populations <- unique(c(
#     if ("population" %in% names(DATA$missing)) DATA$missing$population else NULL,
#     if ("population" %in% names(DATA$error)) DATA$error$population else NULL,
#     if ("population" %in% names(DATA$other)) DATA$other$population else NULL,
#     if ("population" %in% names(DATA$evaluation)) DATA$evaluation$population else NULL,
#     if ("population" %in% names(DATA$perf_pop)) DATA$perf_pop$population else NULL
#   ))
#   populations <- sort(populations[!is.na(populations)])
#   
#   list(
#     date_min    = min(all_dates, na.rm = TRUE),
#     date_max    = max(all_dates, na.rm = TRUE),
#     zones       = zones,
#     localites   = localites,
#     enumerators = enumerators,
#     populations = populations
#   )
# }

get_filter_values <- function() {
  
  # Dates
  all_dates <- c()
  for (nm in names(DATA)) {
    if (!is.null(DATA[[nm]]) && "date" %in% names(DATA[[nm]])) {
      all_dates <- c(all_dates, DATA[[nm]]$date)
    }
  }
  all_dates <- as.Date(all_dates, origin = "1970-01-01")
  all_dates <- all_dates[!is.na(all_dates)]
  
  # Zones - vérifier chaque dataframe avant d'accéder aux colonnes
  zones <- c()
  for (nm in names(DATA)) {
    if (!is.null(DATA[[nm]])) {
      if ("zone" %in% names(DATA[[nm]])) {
        zones <- c(zones, DATA[[nm]]$zone)
      }
      if ("zone_name" %in% names(DATA[[nm]])) {
        zones <- c(zones, DATA[[nm]]$zone_name)
      }
    }
  }
  zones <- sort(unique(zones[!is.na(zones)]))
  
  # Localités
  localites <- c()
  for (nm in names(DATA)) {
    if (!is.null(DATA[[nm]])) {
      if ("localite" %in% names(DATA[[nm]])) {
        localites <- c(localites, DATA[[nm]]$localite)
      }
      if ("localite_name" %in% names(DATA[[nm]])) {
        localites <- c(localites, DATA[[nm]]$localite_name)
      }
    }
  }
  localites <- sort(unique(localites[!is.na(localites)]))
  
  # Enquêteurs
  enumerators <- c()
  for (nm in names(DATA)) {
    if (!is.null(DATA[[nm]])) {
      if ("enumerator" %in% names(DATA[[nm]])) {
        enumerators <- c(enumerators, DATA[[nm]]$enumerator)
      }
      if ("enumerator_name" %in% names(DATA[[nm]])) {
        enumerators <- c(enumerators, DATA[[nm]]$enumerator_name)
      }
    }
  }
  enumerators <- sort(unique(enumerators[!is.na(enumerators)]))
  
  # Populations
  populations <- c()
  for (nm in names(DATA)) {
    if (!is.null(DATA[[nm]]) && "population" %in% names(DATA[[nm]])) {
      populations <- c(populations, DATA[[nm]]$population)
    }
  }
  populations <- sort(unique(populations[!is.na(populations)]))
  
  list(
    date_min    = if(length(all_dates) > 0) min(all_dates, na.rm = TRUE) else Sys.Date() - 30,
    date_max    = if(length(all_dates) > 0) max(all_dates, na.rm = TRUE) else Sys.Date(),
    zones       = zones,
    localites   = localites,
    enumerators = enumerators,
    populations = populations
  )
}

FILTERS <- get_filter_values()

# --- Fonctions utilitaires ----------------------------------------------------

# Fonction pour filtrer les données selon les filtres sélectionnés
filter_data <- function(df, date_range = NULL, zones = NULL, localites = NULL,
                        enumerators = NULL, populations = NULL) {
  
  result <- df
  
  if (!is.null(date_range) && "date" %in% names(result)) {
    result <- result %>% filter(date >= date_range[1] & date <= date_range[2])
  }
  
  # Zone (gérer les deux noms de colonnes)
  if (!is.null(zones) && length(zones) > 0) {
    if ("zone" %in% names(result)) {
      result <- result %>% filter(zone %in% zones)
    } else if ("zone_name" %in% names(result)) {
      result <- result %>% filter(zone_name %in% zones)
    }
  }
  
  # Localité
  if (!is.null(localites) && length(localites) > 0) {
    if ("localite" %in% names(result)) {
      result <- result %>% filter(localite %in% localites)
    } else if ("localite_name" %in% names(result)) {
      result <- result %>% filter(localite_name %in% localites)
    }
  }
  
  # Enquêteur
  if (!is.null(enumerators) && length(enumerators) > 0) {
    if ("enumerator" %in% names(result)) {
      result <- result %>% filter(enumerator %in% enumerators)
    } else if ("enumerator_name" %in% names(result)) {
      result <- result %>% filter(enumerator_name %in% enumerators)
    }
  }
  
  # Population
  if (!is.null(populations) && length(populations) > 0) {
    if ("population" %in% names(result)) {
      result <- result %>% filter(population %in% populations)
    }
  }
  
  return(result)
}

# Fonction pour créer une value box
create_value_box <- function(title, value, subtitle = NULL, icon = "bar-chart",
                              color = "primary") {
  value_box(
    title = title,
    value = value,
    showcase = bsicons::bs_icon(icon),
    p(subtitle),
    theme = color
  )
}

# Fonction pour formater les nombres
fmt_number <- function(x, digits = 1) {
  formatC(round(x, digits), format = "f", digits = digits, big.mark = " ")
}

fmt_pct <- function(x, digits = 1) {
  paste0(formatC(round(x, digits), format = "f", digits = digits), "%")
}

# Layout plotly standard
plotly_layout <- function(p, title = "", xlab = "", ylab = "", showlegend = TRUE) {
  p %>%
    layout(
      title = list(
        text = title,
        font = list(size = 14, color = COLORS$dark, family = "Inter, sans-serif"),
        x = 0
      ),
      xaxis = list(
        title = list(text = xlab, font = list(size = 11)),
        gridcolor = "#E2E8F0",
        zerolinecolor = "#E2E8F0"
      ),
      yaxis = list(
        title = list(text = ylab, font = list(size = 11)),
        gridcolor = "#E2E8F0",
        zerolinecolor = "#E2E8F0"
      ),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      showlegend = showlegend,
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.25,
        xanchor = "center",
        x = 0.5,
        font = list(size = 10)
      ),
      margin = list(l = 60, r = 20, t = 40, b = 60),
      hoverlabel = list(
        bgcolor = "white",
        font = list(size = 12, family = "Inter, sans-serif")
      )
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE,
      locale = "fr"
    )
}

# Thème DT standard
dt_options <- function() {
  list(
    pageLength = 15,
    scrollX = TRUE,
    language = list(
      search = "Rechercher :",
      lengthMenu = "Afficher _MENU_ entrées",
      info = "Affichage de _START_ à _END_ sur _TOTAL_ entrées",
      paginate = list(
        first = "Premier",
        last = "Dernier",
        `next` = "Suivant",
        previous = "Précédent"
      ),
      emptyTable = "Aucune donnée disponible",
      zeroRecords = "Aucun résultat trouvé"
    ),
    dom = "Bfrtip",
    buttons = c("copy", "csv", "excel")
  )
}

# Labels lisibles pour les types de performance
TYPE_LABELS <- c(
  "tx_progression"    = "Taux de progression",
  "tx_participation"  = "Taux de participation",
  "tx_couverture"     = "Taux de couverture",
  "tx_consentement"   = "Taux de consentement",
  "tx_approbation"    = "Taux d'approbation",
  "tx_incoherence"    = "Taux d'incohérence",
  "tx_missing"        = "Taux de valeurs manquantes",
  "tx_other"          = "Taux de valeurs autres",
  "tx_reject"         = "Taux de rejet",
  "tx_replacement"    = "Taux de remplacement",
  "tx_nonlisted"      = "Taux hors liste",
  "nb_interview"      = "Nombre d'interviews",
  "nb_hhmembers"      = "Nombre de membres du ménage",
  "duree_collecte"    = "Durée de collecte (min)",
  "duree_de_travail"  = "Durée de travail (h)",
  "duree_au_travail"  = "Durée au travail (h)",
  "heure_dbt_travail" = "Heure début de travail",
  "heure_fin_travail" = "Heure fin de travail",
  "prop_hhsize_one"   = "Proportion ménages 1 personne",
  "prop_hhsize_atMost3" = "Proportion ménages <= 3 personnes"
)

get_type_label <- function(type_code) {
  label <- TYPE_LABELS[type_code]
  ifelse(is.na(label), type_code, label)
}

# Catégories de types pour les onglets performance
PERF_CATEGORIES <- list(
  "Indicateurs clés" = c("tx_progression", "tx_couverture", "tx_participation",
                          "tx_consentement", "tx_approbation"),
  "Qualité des données" = c("tx_incoherence", "tx_missing", "tx_other",
                             "tx_reject", "tx_replacement", "tx_nonlisted"),
  "Volume & Durée" = c("nb_interview", "nb_hhmembers", "duree_collecte",
                        "duree_de_travail", "duree_au_travail"),
  "Horaires" = c("heure_dbt_travail", "heure_fin_travail"),
  "Composition ménages" = c("prop_hhsize_one", "prop_hhsize_atMost3")
)

# Message de bienvenue
cat("\n========================================\n")
cat("  RMS Monitoring Dashboard - Loaded\n")
cat(paste("  Données:", format(FILTERS$date_min, "%d/%m/%Y"), "-", format(FILTERS$date_max, "%d/%m/%Y"), "\n"))
cat(paste("  Zones:", length(FILTERS$zones), "\n"))
cat(paste("  Localités:", length(FILTERS$localites), "\n"))
cat(paste("  Enquêteurs:", length(FILTERS$enumerators), "\n"))
cat(paste("  Populations:", length(FILTERS$populations), "\n"))
cat("========================================\n\n")


#*********************************************************************************
# FONCTIONS POUR LES FILTRES INTERCONNECTÉS
#*********************************************************************************
# Ces fonctions calculent les valeurs disponibles pour chaque filtre
# en fonction des autres filtres sélectionnés (filtres dépendants)

# Fonction utilitaire pour combiner toutes les données
# get_all_data <- function() {
#   bind_rows(
#     DATA$missing %>% mutate(source = "missing"),
#     DATA$error %>% mutate(source = "error"),
#     DATA$other %>% mutate(source = "other"),
#     DATA$evaluation %>% mutate(source = "evaluation")
#   )
# }
get_all_data <- function() {
  # Créer une liste de dataframes avec leurs sources
  data_list <- list()
  
  if (!is.null(DATA$missing) && nrow(DATA$missing) > 0) {
    data_list$missing <- DATA$missing %>% 
      mutate(source = "missing")
  }
  
  if (!is.null(DATA$error) && nrow(DATA$error) > 0) {
    data_list$error <- DATA$error %>% 
      mutate(source = "error")
  }
  
  if (!is.null(DATA$other) && nrow(DATA$other) > 0) {
    data_list$other <- DATA$other %>% 
      mutate(source = "other")
  }
  
  if (!is.null(DATA$evaluation) && nrow(DATA$evaluation) > 0) {
    data_list$evaluation <- DATA$evaluation %>% 
      mutate(source = "evaluation")
  }
  
  # Combiner tous les dataframes en s'assurant qu'ils ont les mêmes colonnes
  all_cols <- unique(unlist(lapply(data_list, names)))
  
  data_list <- lapply(data_list, function(df) {
    missing_cols <- setdiff(all_cols, names(df))
    for (col in missing_cols) {
      df[[col]] <- NA
    }
    df[, all_cols, drop = FALSE]
  })
  
  # Combiner
  bind_rows(data_list)
}

# Fonction pour obtenir les zones disponibles selon les autres filtres
get_available_zones <- function(date_range = NULL, localites = NULL, 
                                enumerators = NULL, populations = NULL) {
  df <- get_all_data()
  
  if (!is.null(date_range) && "date" %in% names(df)) {
    df <- df %>% filter(date >= date_range[1] & date <= date_range[2])
  }
  
  if (!is.null(localites) && length(localites) > 0) {
    df <- df %>% filter(localite %in% localites)
  }
  
  if (!is.null(enumerators) && length(enumerators) > 0) {
    df <- df %>% filter(enumerator %in% enumerators)
  }
  
  if (!is.null(populations) && length(populations) > 0) {
    df <- df %>% filter(population %in% populations)
  }
  
  sort(unique(df$zone[!is.na(df$zone)]))
}

# Fonction pour obtenir les localités disponibles selon les autres filtres
get_available_localites <- function(date_range = NULL, zones = NULL, 
                                    enumerators = NULL, populations = NULL) {
  df <- get_all_data()
  
  if (!is.null(date_range) && "date" %in% names(df)) {
    df <- df %>% filter(date >= date_range[1] & date <= date_range[2])
  }
  
  if (!is.null(zones) && length(zones) > 0) {
    df <- df %>% filter(zone %in% zones)
  }
  
  if (!is.null(enumerators) && length(enumerators) > 0) {
    df <- df %>% filter(enumerator %in% enumerators)
  }
  
  if (!is.null(populations) && length(populations) > 0) {
    df <- df %>% filter(population %in% populations)
  }
  
  sort(unique(df$localite[!is.na(df$localite)]))
}

# Fonction pour obtenir les enquêteurs disponibles selon les autres filtres
get_available_enumerators <- function(date_range = NULL, zones = NULL, 
                                      localites = NULL, populations = NULL) {
  df <- get_all_data()
  
  if (!is.null(date_range) && "date" %in% names(df)) {
    df <- df %>% filter(date >= date_range[1] & date <= date_range[2])
  }
  
  if (!is.null(zones) && length(zones) > 0) {
    df <- df %>% filter(zone %in% zones)
  }
  
  if (!is.null(localites) && length(localites) > 0) {
    df <- df %>% filter(localite %in% localites)
  }
  
  if (!is.null(populations) && length(populations) > 0) {
    df <- df %>% filter(population %in% populations)
  }
  
  sort(unique(df$enumerator[!is.na(df$enumerator)]))
}

# Fonction pour obtenir les populations disponibles selon les autres filtres
get_available_populations <- function(date_range = NULL, zones = NULL, 
                                      localites = NULL, enumerators = NULL) {
  df <- get_all_data()
  
  if (!is.null(date_range) && "date" %in% names(df)) {
    df <- df %>% filter(date >= date_range[1] & date <= date_range[2])
  }
  
  if (!is.null(zones) && length(zones) > 0) {
    df <- df %>% filter(zone %in% zones)
  }
  
  if (!is.null(localites) && length(localites) > 0) {
    df <- df %>% filter(localite %in% localites)
  }
  
  if (!is.null(enumerators) && length(enumerators) > 0) {
    df <- df %>% filter(enumerator %in% enumerators)
  }
  
  sort(unique(df$population[!is.na(df$population)]))
}
