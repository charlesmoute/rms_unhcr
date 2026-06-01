# Programme d'initialisation de l'environnement de monitoring de l'enquête
# Enquête : RMS (Result Monitoring Survey) 2026 - HCR TCHAD


#****************************************************************************
#* Préchargement des fonctions et variables nécessaires au monitoring
#****************************************************************************
source("monitoring_utilities.R")

#****************************************************************************
#  Lancement des routines de monitoring 
#****************************************************************************

# Import des données d'intérêts
db <- import_data()

# Le code ci-dessous est à supprimer une fois tout ok
# saveRDS(db,"raw_data.rds")
# db <- readRDS("raw_data.rds") #asupprimer une fois tout okay... les donnees doivent toujours être telechargé

# Ajustement des paramètres du systèmes
config$db_raw <- db$raw
config$data <- db$clean

# Construction de la base de monitoring
result <- build_monitoringData(config$data)
config$monitoring_data <- result

#* [Export des indicateurs de performance]
# Export des données de monitoring dans un format accessible par un être
# humain...
export_dataset(target="all")

# Export du fichier de configuration à exploiter pour la production du rapport
# Avec des graphiques et autres...
# export(result,"db_monitoring.rds")
export(config,"rms_chad26_params.rds")


#**************************************************************************** 
#* [Export des données pour le tableau de bord]
#* Les données traitées doivent être mise à jour sans que l'on ait à redéployer
#* systématiquement le tableau de bord pour leur prise en compte.
#* Cette phase est nécessaire car pour des raisons de sécurites nous n'accédons
#* pas directement au données sur le serveur kobo et en outre il y'a un traitement
#* nécessaire à effectuer avant de visualiser les données
#****************************************************************************
#*
#* Les données seront exporées dans un dépôt github public
gh_config <- config[1:36]

# repo_url <- "https://github.com/charlesmoute/datasets.git"#Sys.getenv("GITHUB_REPO_URL")
# user_name <- "charlesmoute"#Sys.getenv("USER_NAME")
# user_email <- "charlesmoute@gmail.com" #Sys.getenv("USER_EMAIL")
# token <- Sys.getenv("GITHUB_PAT")

repo_url <- Sys.getenv("GITHUB_REPO_URL") #"https://github.com/charlesmoute/datasets.git"
user_name <- Sys.getenv("USER_NAME") #"charlesmoute"#
user_email <- Sys.getenv("USER_EMAIL") #"charlesmoute@gmail.com" #
token <- Sys.getenv("GITHUB_PAT")

# Dossier local où le dépôt sera cloné
local_path <- config$gh_local_path

# --- 1. CLONAGE OU OUVERTURE DU DÉPÔT ---
if (!dir.exists(local_path)) {
  # cat("Clonage du dépôt...\n")
  cat(glue::glue_col("{green Charles >} Clonage du dépôt...\n"))
  repo <- git2r::clone(
    url = repo_url, 
    local_path = local_path, 
    credentials = git2r::cred_user_pass(user_name, token)
  )
} else {
  # cat("Ouverture du dépôt existant...\n")
  # cat(glue::glue_col("{green Charles >} Ouverture du dépôt existant..."),"\n")
  message(glue::glue_col("{green Charles >} Ouverture du dépôt existant..."))
  repo <- git2r::repository(local_path)
}

# Configuration de l'identité pour les commits
git2r::config(repo, user.name = user_name, user.email = user_email)

# --- 2. LECTURE (PULL) ---
# cat("Mise à jour du dépôt (Pull)...\n")
# cat(glue::glue_col("{green Charles >} Mise à jour du dépôt (Pull)..."),"\n")
message(glue::glue_col("{green Charles >} Mise à jour du dépôt (Pull)..."))
git2r::pull(repo, credentials = git2r::cred_user_pass(user_name, token))

# --- 3. EXECUTION DU PROGRAMME DE MISE A JOUR DES DONNEES ---
file_monitoring <- file.path("local","output","rms_monitoring_datamanager.xlsx")
if(file.exists(file_monitoring)){
  
  # --- 3.1. ÉCRITURE (MODIFICATION) ---
  wb <- openxlsx::loadWorkbook(file_monitoring)
  file_name <- config$gh_data_file
  rio::export(wb,file.path(local_path,file_name))
  
  # --- 3.2. ENREGISTREMENT (COMMIT) ---
  # cat(glue::glue_col("{green Charles >} Enregistrement des modifications (Commit)..."),"\n")
  message(glue::glue_col("{green Charles >} Enregistrement des modifications (Commit)..."))
  git2r::add(repo, file_name)
  tryCatch({
    # git2r::commit(repo, paste("Mise à jour automatique :", Sys.time()))
    git2r::commit(
      repo, 
      paste(
        "Mise à jour automatique :", #Sys.time()
        as.character(stringr::str_glue("{stringr::str_sub(as.character(lubridate::now()),1,19)}"))
      )
    )
  }, error = function(e) {
    # Si la branche par défaut est 'master', réessayez avec 'master'
    # git2r::push(repo, name = "origin", refspec = "refs/heads/master", credentials = cred)
    # message("Push réussi vers GitHub (branche master) !")
    warning("Nothing added to commit...")
  })
  
  # --- 3.3. ENVOI (PUSH) ---
  # cat(glue::glue_col("{green Charles >} Envoi vers GitHub (Push)..."),"\n")
  message(glue::glue_col("{green Charles >} Envoi vers GitHub (Push)..."))
  # Note cruciale : Utiliser cred_user_pass() au lieu de cred_token() 
  # pour éviter l'erreur 'git2r_push: no error'
  git2r::push(
    repo, 
    name = "origin", 
    refspec = "refs/heads/main", 
    credentials = git2r::cred_user_pass(user_name, token)
  )
  
  # cat(glue::glue_col("{green Charles >} Opération terminée avec succès !"),"\n")
  message(glue::glue_col("{green Charles >} Opération terminée avec succès !"))
  # Suppression des variables inutiles
  rm(wb,file_name)
  
}else{
  # cat(glue::glue_col("{red Charles >} Le fichier {file_monitoring} n'a pas été trouvé...","\n"))
  cli::cli_alert_danger(glue::glue_col("{red Charles >} Le fichier {file_monitoring} n'a pas été trouvé..."))
}


#* [--- LECTURE DES DONNÉES AVEC PINS ---]
# # Pour lire des fichiers bruts sur GitHub sans utiliser la structure de board pins,
# # on peut utiliser board_url() en pointant vers les fichiers bruts (raw).
# # Exemple : Lecture d'un fichier spécifique nommé 'rms23526_data.rds'
# file_name_to_read <- config$gh_data_file #"rms23526_data.rds"
# raw_url <- config$gh_data_url
# 
# # repo_owner <- "charlesmoute"
# # repo_name <- "datasets"
# # raw_url <- sprintf("https://raw.githubusercontent.com/%s/%s/main/%s",
# #                    repo_owner, repo_name, file_name_to_read)
# # file_name_to_read <- "rms23526_data.rds"
# # board <- pins::board_url(c(my_data = raw_url))
# 
# # Téléchargement du fichier
# message(sprintf("Téléchargement du fichier via board_url : %s", file_name_to_read))
# temp_file_path <- pins::pin_download(board, "my_data")
# test_data <- readRDS(temp_file_path)
# 
# if (!is.null(test_data)) {
#   message("Données lues avec succès.\n")
#   if (isS4(test_data)) {
#     message("L'objet lu est de type S4. Affichage de sa structure :\n")
#     print(str(test_data, max.level = 1))
#   } else {
#     print(head(test_data))
#   }
# }
# # rm(repo_owner,repo_name,file_name_to_read,raw_url,temp_file_path,test_data)
# rm(file_name_to_read,raw_url,temp_file_path,test_data)


#* [--- SUPPRESSION DES DONNÉES DU GITHUB PUBLIC ---]
# #local_path <- config$gh_local_path
# #"/Users/charles/Documents/GitHub/datasets"
# # Clonage ou ouverture du dépôt
# if (!dir.exists(local_path)) {
#   cat("Clonage du dépôt pour la suppression...\n")
#   repo <- clone(repo_url, local_path, credentials = cred_user_pass(user_name, token))
# } else {
#   repo <- repository(local_path)
# }
# 
# # Configuration de l'identité
# config(repo, user.name = user_name, user.email = user_email)
# 
# # Liste des fichiers dans le dépôt local
# files_in_repo <- list.files(local_path, recursive = TRUE)
# cat("Fichiers présents localement :\n")
# print(files_in_repo)
# 
# # --- EXEMPLE DE SUPPRESSION ---
# # Nous allons supprimer le fichier de test s'il existe
# file_to_delete <- "test_file.txt" 
# 
# if (file_to_delete %in% files_in_repo) {
#   cat(sprintf("Suppression du fichier : %s\n", file_to_delete))
#   
#   # 1. Suppression physique du fichier
#   file.remove(file.path(local_path, file_to_delete))
#   
#   # 2. Ajout de la suppression à l'index git
#   add(repo, file_to_delete)
#   
#   # 3. Commit de la suppression
#   commit(repo, sprintf("Suppression de %s via script R moderne", file_to_delete))
#   
#   # 4. Push vers GitHub
#   cat("Envoi de la suppression vers GitHub...\n")
#   push(
#     repo, 
#     credentials = cred_user_pass(user_name, token)
#   )
#   cat("Fichier supprimé avec succès sur GitHub.\n")
# } else {
#   cat(sprintf("Le fichier '%s' n'a pas été trouvé pour la suppression.\n", file_to_delete))
# }
# cat("\nOpérations terminées.\n")
# 
# rm(file_to_delete)

# Nettoyage de l'espace de travail
# file_monitoring,repo,cred,config_file,status,gh_config
rm(
  db,result,repo_url,user_name,user_email,token,repo,local_path,file_monitoring
)

# Sauvegarde de l'environnement de travail en local
save.image()


