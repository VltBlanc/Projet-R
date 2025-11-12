#
# Fichier: global.R
# Rôle: Charger les bibliothèques, charger et préparer les données.
# S'exécute une seule fois au démarrage de l'application.
#

# --- 1. Charger les bibliothèques ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)       # Pour les tableaux interactifs

# --- 2. Charger et préparer les données ---

file_path <- "Paris2024-annexe-statistique-diffusion.csv"

if (!file.exists(file_path)) {
  print("ERREUR : Fichier 'Paris2024-annexe-statistique-diffusion.csv' non trouvé.")
  print("Veuillez placer le fichier CSV dans le même dossier que l'application")
  
  # Créer un jeu de données factice en cas d'erreur
  data <- data.frame(
    périmètre = "Erreur", type = "Fichier", nop = "Non", epco = "Trouvé",
    Q1 = 0, Médiane = 0, Q3 = 0, Moyenne = 0,
    Q1.1 = 0, Médiane.1 = 0, Q3.1 = 0, Moyenne.1 = 0,
    obs = 0,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
} else {
  # 1. Lire le fichier avec l'encodage latin1
  data <- read.csv(
    file_path,  
    sep = ";",  
    header = TRUE,  
    check.names = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "latin1"
  )
  
  # 2. Remplacer les chaînes vides par NA
  data[data == ""] <- NA
}

# --- 3. Nettoyer et renommer les colonnes ---
data <- data %>%
  # Renommer les colonnes pour plus de clarté
  rename(
    Zone = périmètre,
    Type = type,
    Pieces = nop,
    Epoque_Construction = epco,
    Loyer_m2_Q1 = Q1,
    Loyer_m2_Mediane = Médiane,
    Loyer_m2_Q3 = Q3,
    Loyer_m2_Moyenne = Moyenne,
    Loyer_Total_Q1 = Q1.1,
    Loyer_Total_Mediane = Médiane.1,
    Loyer_Total_Q3 = Q3.1,
    Loyer_Total_Moyenne = Moyenne.1
  ) %>%
  # Convertir les colonnes de loyer et d'observations en numérique
  mutate(
    across(starts_with("Loyer_") | "obs", as.numeric)
  )

# Remplacer les NA dans les colonnes de *filtre* (texte)
data <- data %>%
  mutate(
    Zone = ifelse(is.na(Zone), "(Non spécifié)", Zone),
    Type = ifelse(is.na(Type), "(Non spécifié)", Type),
    Pieces = ifelse(is.na(Pieces), "(Non spécifié)", Pieces),
    Epoque_Construction = ifelse(is.na(Epoque_Construction), "(Non spécifié)", Epoque_Construction)
  )

# --- 4. Créer des listes pour les menus de filtre ---
# Ces objets seront disponibles dans ui.R et server.R
zone_choices <- unique(data$Zone)
zone_choices <- zone_choices[zone_choices != "(Non spécifié)"] # Retirer le "Non spécifié" des choix

all_types <- c("Tous" = "Tous", unique(data$Type))
all_pieces <- c("Tous" = "Tous", "1-3P" = "1-3P", unique(data$Pieces))
all_epoque <- c("Tous" = "Tous", unique(data$Epoque_Construction))