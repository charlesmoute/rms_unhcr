# RMS UNHCR — Progiciels de Suivi et d'Analyse des Résultats

> **Enquête de Suivi des Résultats (RMS)** — Outils R pour la collecte, le suivi en temps réel et l'analyse des indicateurs selon la norme standard du HCR.

**🌐 Démo en ligne : [charlesmoute.shinyapps.io/rms_dashboard](https://charlesmoute.shinyapps.io/rms_dashboard/)**

---

## Table des matières

- [Présentation générale](#présentation-générale)
- [Architecture du système](#architecture-du-système)
- [Progiciel rms\_monitoring](#progiciel-rms_monitoring)
- [Progiciel rms\_dashboard](#progiciel-rms_dashboard)
- [Prérequis et installation](#prérequis-et-installation)
- [Configuration](#configuration)
- [Utilisation](#utilisation)
- [Structure des fichiers](#structure-des-fichiers)
- [Flux de données](#flux-de-données)
- [Production des résultats](#production-des-résultats)
- [Contribution](#contribution)
- [Licence](#licence)

---

## Présentation générale

Ce dépôt regroupe deux progiciels R complémentaires conçus pour l'**enquête annuelle de Suivi des Résultats (Result Monitoring Survey — RMS)** du Haut-Commissariat des Nations Unies pour les Réfugiés (HCR). Ils couvrent l'intégralité du cycle de collecte :

| Progiciel | Rôle principal |
|---|---|
| **`rms_monitoring`** | Téléchargement des données depuis KoboToolbox, contrôle qualité en temps réel, alimentation du tableau de bord, et production des résultats finaux |
| **`rms_dashboard`** | Tableau de bord Shiny interactif pour la visualisation du suivi de collecte en temps réel |

Les outils respectent la **norme standard RMS du HCR (version CAPI 3.2)** et ont été développés pour les enquêtes menées au Tchad et au Cameroun.

---

## Architecture du système

```
┌─────────────────────────────────────────────────────────┐
│                   KoboToolbox (UNHCR)                   │
│              https://kobo.unhcr.org                     │
└──────────────────────┬──────────────────────────────────┘
                       │  API (robotoolbox)
                       ▼
┌─────────────────────────────────────────────────────────┐
│                  rms_monitoring                         │
│                                                         │
│  ┌─────────────┐  ┌────────────┐  ┌─────────────────┐  │
│  │  Collecte   │  │  Contrôle  │  │    Analyse &    │  │
│  │  des données│→ │  qualité   │→ │    Résultats    │  │
│  └─────────────┘  └────────────┘  └─────────────────┘  │
│         │                                               │
│         │ Export (rms_monitoring_datamanager.xlsx)      │
│         ▼                                               │
│  ┌──────────────────────┐                               │
│  │  Dépôt GitHub public │  (données de monitoring)      │
│  └──────────┬───────────┘                               │
└─────────────│───────────────────────────────────────────┘
              │  Lecture automatique
              ▼
┌─────────────────────────────────────────────────────────┐
│                  rms_dashboard                          │
│         (Shiny App — Tableau de bord interactif)        │
└─────────────────────────────────────────────────────────┘
```

---

## Progiciel `rms_monitoring`

### Description

`rms_monitoring` est le cœur opérationnel du dispositif. Il se connecte au serveur KoboToolbox du HCR, télécharge les soumissions en temps réel, effectue les contrôles de qualité, construit la base de monitoring et publie les données vers le tableau de bord. En fin de collecte, il dispose d'un module complet de calcul des indicateurs RMS.

### Modules principaux

#### 1. Initialisation et configuration (`monitoring_utilities.R`)

- Chargement des paramètres depuis `local/input/params.xlsx` (plan d'échantillonnage, déploiement des enquêteurs)
- Configuration de la connexion KoboToolbox via token API
- Définition des seuils de performance journaliers par agent et par collecte globale
- Paramétrage des variables de désagrégation : groupe de population, zone, localité, enquêteur
- Intégration avec GitHub via `git2r` pour la publication automatique des données

#### 2. Suivi de la collecte (`init_monitoring.R`, `lauch_monitoring.R`)

- Téléchargement des données brutes depuis KoboToolbox (`robotoolbox::kobo_data`)
- Nettoyage et recodage automatique des variables
- Construction de la base de monitoring (`build_monitoringData`) avec calcul des indicateurs de performance :
  - Taux d'avancement de la collecte par zone, localité et enquêteur
  - Détection des valeurs manquantes (`missing`) et des erreurs (`error`)
  - Suivi des modalités "autres à préciser" (`other`)
  - Évaluation de la qualité par enquêteur (`evaluation`)
  - Analyse des silhouettes pour la détection d'anomalies (`silhouette`)
- Export vers `rms_monitoring_datamanager.xlsx` (fichier multi-feuilles consommé par le tableau de bord)
- Publication automatique sur un dépôt GitHub public via commit/push

#### 3. Génération des rapports de débrief (`debriefing_graphiques.R`, `graphiques_complémentaires.R`)

- Production de graphiques de suivi par enquêteur pour les séances de débrief quotidiennes

#### 4. Préparation des données d'analyse (`data_preparation.R`)

- Extraction des tables de données depuis KoboToolbox :
  - `main` — données ménage
  - `S1` — membres du ménage
  - `S2_repeat` — groupe de répétition
  - `rpt_hhmnames` — noms des membres
- Nettoyage selon les instructions de `cleaning_instructions.xlsx`
- Recodage des variables, création des catégories d'âge, des variables de désagrégation
- Application des pondérations d'échantillonnage (`calc_ponderation.xlsx`)

#### 5. Calcul des indicateurs (`indicator_calculations.R`, `launch_calculations.R`)

- Calcul des indicateurs RMS standards du HCR avec pondération complexe (`srvyr`)
- Désagrégation par : groupe de population (Réfugiés/DA, PDI, Communautés d'accueil, Retournés), genre, catégorie d'âge, pays d'origine, handicap
- Calcul des p-values pour les tests statistiques (comparaisons entre groupes)
- Export des tableaux de résultats pondérés en Excel (`Weighted_Combined_Indicators.xlsx`, `Weighted_pvalues.xlsx`)
- Génération des sorties par enquêteur dans `local/output/<ZONE>/`

### Fichiers de scripts

| Fichier | Description |
|---|---|
| `start_program.R` | Point d'entrée principal — orchestre le monitoring et l'analyse |
| `init_monitoring.R` | Initialise la collecte, exporte les données, publie sur GitHub |
| `lauch_monitoring.R` | Lance le monitoring (appel simplifié) |
| `monitoring_utilities.R` | Fonctions utilitaires de monitoring et configuration globale |
| `init_data_analysis.R` | Initialise la phase d'analyse finale |
| `launch_analysis.R` | Lance le programme d'analyse (appel simplifié) |
| `data_preparation.R` | Préparation et nettoyage des données pour l'analyse |
| `launch_calculations.R` | Orchestration du calcul des indicateurs avec pondération |
| `indicator_calculations.R` | Calcul détaillé de tous les indicateurs RMS |
| `analysis_utilities.R` | Fonctions statistiques et utilitaires d'analyse |
| `debriefing_graphiques.R` | Graphiques de performance pour les débriefs quotidiens |
| `graphiques_complémentaires.R` | Visualisations complémentaires |

---

## Progiciel `rms_dashboard`

### Description

`rms_dashboard` est une application **R Shiny** qui visualise en temps réel les données de suivi de collecte publiées par `rms_monitoring`. Elle se déploie de manière autonome et se met à jour automatiquement en lisant les données depuis le dépôt GitHub partagé, sans nécessiter de redéploiement.

### Fonctionnalités

- **Filtres croisés dynamiques** — par zone, localité, enquêteur, groupe de population et plage de dates
- **Indicateurs de performance globaux** — taux d'avancement, nombre de soumissions, comparaison aux cibles journalières
- **Suivi par zone géographique** (`performance_zone`) et par localité (`performance_localite`)
- **Performance par enquêteur** (`performance_enumerator`) — identification des agents en retard ou avec des problèmes qualité
- **Suivi par groupe de population** (`performance_population`)
- **Contrôle qualité** — tableaux des valeurs manquantes, erreurs détectées, modalités "autres"
- **Évaluation de la qualité des questionnaires** — scores par enquêteur
- **Analyse de silhouette** — détection automatique des questionnaires atypiques

### Architecture Shiny

| Fichier | Description |
|---|---|
| `global.R` | Chargement des packages, configuration, fonctions de lecture des données (locale ou GitHub) |
| `ui.R` | Interface utilisateur — layout, filtres, composants visuels (bslib, shinyWidgets, plotly, DT) |
| `server.R` | Logique serveur — réactivité des filtres, génération des graphiques et tableaux |

### Source des données

Le tableau de bord lit le fichier `rms_monitoring_datamanager.xlsx` depuis :
1. En priorité : le **dépôt GitHub public** configuré (mise à jour automatique à chaque exécution du monitoring)
2. En fallback : le répertoire local `data/rms_monitoring_datamanager.xlsx`

---

## Prérequis et installation

### Environnement R

- **R** ≥ 4.2.0
- **RStudio** (recommandé)

### Packages requis

**`rms_monitoring`**

```r
install.packages("pacman")
pacman::p_load(
  tidyverse, dplyr, tidyr, rlang, purrr, magrittr,
  expss, srvyr, readr, labelled, pastecs, psych,
  tableone, ggplot2, unhcrthemes, scales, gt, webshot2,
  sjlabelled, waffle, writexl, haven, readxl, dm,
  janitor, visdat, DiagrammeR, robotoolbox, remotes,
  openxlsx, rio, git2r, glue, lubridate, stringr, cli
)
```

**`rms_dashboard`**

```r
install.packages(c(
  "shiny", "shinyWidgets", "bslib", "readxl",
  "dplyr", "tidyr", "lubridate", "plotly",
  "DT", "scales", "stringr", "bsicons", "htmltools"
))
```

> **Note :** Le package `unhcrthemes` n'est pas sur le CRAN. Installez-le depuis GitHub :
> ```r
> remotes::install_github("unhcr-dataviz/unhcrthemes")
> ```

---

## Configuration

### Variables d'environnement

Chaque progiciel utilise un fichier **`.Renviron`** à placer à la racine du projet. **Ce fichier ne doit jamais être versionné** (ajoutez-le à votre `.gitignore`).

**`rms_monitoring/.Renviron`**

```ini
# Connexion KoboToolbox
KOBOTOOLBOX_TOKEN="votre_token_kobo"
KOBOTOOLBOX_URL="https://kobo.unhcr.org/"
form_id="identifiant_du_formulaire"
form_version=""

# Connexion GitHub (pour publication des données)
GITHUB_PAT="votre_token_github"
GITHUB_REPO_URL="https://github.com/votre-org/votre-depot.git"
USER_NAME="votre_nom_utilisateur"
USER_EMAIL="votre@email.com"
```

**`rms_dashboard/.Renviron`**

```ini
# Connexion GitHub (pour lecture des données publiées)
GITHUB_PAT="votre_token_github"
GITHUB_REPO_URL="https://github.com/votre-org/votre-depot.git"
USER_NAME="votre_nom_utilisateur"
USER_EMAIL="votre@email.com"
```

### Fichiers de paramétrage (`rms_monitoring/local/input/`)

| Fichier | Description |
|---|---|
| `params.xlsx` | Plan d'échantillonnage (`sheet="sample"`) et déploiement des enquêteurs (`sheet="deploiement_quanti"`) |
| `choices.xlsx` | Listes de choix du formulaire KoboToolbox et renommage des variables (`sheet="rename_vars"`) |
| `survey_form.xlsx` | Structure du formulaire KoboToolbox |
| `calc_ponderation.xlsx` | Calcul des pondérations par cluster (`sheet="data_sample"`) |
| `cleaning_instructions.xlsx` | Instructions de nettoyage et métadonnées des indicateurs (`sheet="indicators"`) |

### Paramètre de chemin local

Dans `monitoring_utilities.R`, ajustez la variable `gh_local_path` dans l'objet `config` pour pointer vers votre dépôt GitHub local :

```r
gh_local_path = "/chemin/vers/votre/depot/local/datasets"
```

---

## Utilisation

### 1. Suivi de la collecte (pendant la collecte)

```r
# Depuis rms_monitoring/
# Option A — Lancer le monitoring seul
source("lauch_monitoring.R")

# Option B — Lancer le monitoring ET l'analyse en une seule commande
source("start_program.R")
```

À chaque exécution, le programme :
1. Télécharge les dernières soumissions depuis KoboToolbox
2. Effectue les contrôles qualité
3. Exporte `rms_monitoring_datamanager.xlsx`
4. Publie automatiquement le fichier sur le dépôt GitHub public
5. Le tableau de bord `rms_dashboard` se met alors à jour automatiquement

### 2. Visualisation en temps réel (pendant la collecte)

```r
# Depuis rms_dashboard/
shiny::runApp()
```

Ou ouvrez `rms_dashboard.Rproj` dans RStudio et cliquez sur **Run App**.

### 3. Production des résultats finaux (après la collecte)

```r
# Depuis rms_monitoring/
source("launch_analysis.R")
```

Cette commande enchaîne :
1. `data_preparation.R` — Préparation et nettoyage complets des données
2. `launch_calculations.R` — Calcul de tous les indicateurs RMS avec pondération
3. Export des résultats dans `local/output/` (tableaux par enquêteur et par zone) et `local/database/` (indicateurs combinés pondérés)

---

## Structure des fichiers

```
rms_unhcr/
│
├── rms_monitoring/
│   ├── .Renviron                        # Variables d'environnement (⚠ ne pas versionner)
│   ├── rms_monitoring.Rproj             # Projet RStudio
│   ├── start_program.R                  # Point d'entrée principal
│   ├── init_monitoring.R                # Initialisation du monitoring
│   ├── lauch_monitoring.R               # Lancement rapide du monitoring
│   ├── monitoring_utilities.R           # Fonctions et configuration globale
│   ├── init_data_analysis.R             # Initialisation de l'analyse
│   ├── launch_analysis.R                # Lancement rapide de l'analyse
│   ├── data_preparation.R               # Préparation des données
│   ├── launch_calculations.R            # Calcul des indicateurs (orchestrateur)
│   ├── indicator_calculations.R         # Calcul détaillé des indicateurs RMS
│   ├── analysis_utilities.R             # Fonctions statistiques
│   ├── debriefing_graphiques.R          # Graphiques de débrief
│   ├── graphiques_complémentaires.R     # Visualisations complémentaires
│   └── local/
│       ├── input/
│       │   ├── params.xlsx              # Plan d'échantillonnage et déploiement
│       │   ├── choices.xlsx             # Listes de choix KoboToolbox
│       │   ├── survey_form.xlsx         # Structure du formulaire
│       │   ├── calc_ponderation.xlsx    # Calcul des pondérations
│       │   └── cleaning_instructions.xlsx # Instructions de nettoyage
│       ├── output/
│       │   ├── rms_monitoring.xlsx              # Base de monitoring complète
│       │   ├── rms_monitoring_datamanager.xlsx  # Données pour le tableau de bord
│       │   ├── ADRE/                            # Résultats par zone
│       │   ├── FARCHANA/
│       │   └── ...
│       └── database/
│           ├── Weighted_Combined_Indicators.xlsx
│           └── Weighted_pvalues.xlsx
│
└── rms_dashboard/
    ├── .Renviron                        # Variables d'environnement (⚠ ne pas versionner)
    ├── rms_dashboard.Rproj              # Projet RStudio
    ├── global.R                         # Configuration et chargement des données
    ├── ui.R                             # Interface utilisateur Shiny
    ├── server.R                         # Logique serveur Shiny
    └── data/
        └── rms_monitoring_datamanager.xlsx  # Données locales (fallback)
```

---

## Flux de données

### Structure du fichier `rms_monitoring_datamanager.xlsx`

Ce fichier est l'interface entre les deux progiciels. Il contient les feuilles suivantes :

| Feuille | Contenu |
|---|---|
| `missing` | Questionnaires avec valeurs manquantes détectées |
| `error` | Questionnaires avec erreurs de cohérence |
| `other` | Modalités "autres à préciser" nécessitant une révision |
| `evaluation` | Scores de qualité par questionnaire et par enquêteur |
| `performance_zone` | Indicateurs de performance agrégés par zone |
| `performance_localite` | Indicateurs de performance agrégés par localité |
| `performance_enumerator` | Indicateurs de performance par enquêteur |
| `performance_population` | Indicateurs de performance par groupe de population |
| `silhouette` | Scores de silhouette pour la détection d'anomalies |

### Variables de désagrégation standard RMS

| Variable | Description |
|---|---|
| `citizenship` | Pays d'origine |
| `HH07_cat` / `HH07_cat2` | Catégories d'âge |
| `HH04` | Genre |
| `pop_groups` | Groupe de population (Réfugiés/DA, PDI, Communautés d'accueil, Retournés) |
| `disability` | Statut de handicap |

---

## Production des résultats

Le module d'analyse de `rms_monitoring` produit les résultats conformément à la méthodologie RMS du HCR :

- **Pondération complexe** — application des poids d'échantillonnage (`poids_eff_echantillon`) et des poids effectifs réels (`poids_eff_reel`) via le package `srvyr`
- **Gestion des PSU uniques** — option `survey.lonely.psu = "adjust"` pour les strates à PSU unique
- **Tests statistiques** — calcul des p-values pour la comparaison entre groupes de population
- **Export structuré** — tableaux combinés d'indicateurs pondérés (`Weighted_Combined_Indicators.xlsx`) et tableaux de p-values (`Weighted_pvalues.xlsx`)
- **Résultats individuels** — un fichier Excel par enquêteur, classé par zone, dans `local/output/<ZONE>/`

---

## Contribution

Les contributions sont les bienvenues. Merci de respecter les conventions suivantes :

1. Créez une branche dédiée (`git checkout -b feature/ma-fonctionnalite`)
2. Documentez les nouvelles fonctions avec des commentaires en français
3. Testez avec les fichiers d'exemple avant de soumettre une Pull Request
4. N'incluez **jamais** de fichier `.Renviron`, de tokens ou de données sensibles dans vos commits

### Contact

Pour toute question sur les outils ou la méthodologie RMS, contactez l'auteur principal.

---

## Licence

Ce projet est développé dans le cadre des opérations du HCR. Veuillez vous référer aux termes d'utilisation du HCR pour toute réutilisation ou adaptation de ces outils.

---

> **⚠ Sécurité** : Les fichiers `.Renviron` contiennent des tokens d'accès sensibles (KoboToolbox, GitHub). Assurez-vous qu'ils sont bien listés dans votre `.gitignore` et ne les partagez jamais publiquement.
>
> ```
> # .gitignore
> .Renviron
> *.RData
> local/input/
> local/output/
> local/database/
> ```
