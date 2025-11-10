#
# Application Shiny pour les loyers - Version 7
# - Suppression des graphiques "Epoque" et "Pièces"
# - Ajout de commentaires détaillés
# - Simplification du code (fonction helper pour les classements)
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
  print("Veuillez placer le fichier CSV dans le même dossier que app.R")
  
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
# Cela évite d'avoir des filtres "NA" non sélectionnables
data <- data %>%
  mutate(
    Zone = ifelse(is.na(Zone), "(Non spécifié)", Zone),
    Type = ifelse(is.na(Type), "(Non spécifié)", Type),
    Pieces = ifelse(is.na(Pieces), "(Non spécifié)", Pieces),
    Epoque_Construction = ifelse(is.na(Epoque_Construction), "(Non spécifié)", Epoque_Construction)
  )

# --- 4. Créer des listes pour les menus de filtre ---
zone_choices <- unique(data$Zone)
zone_choices <- zone_choices[zone_choices != "(Non spécifié)"] # Retirer le "Non spécifié" des choix

all_types <- c("Tous" = "Tous", unique(data$Type))
all_pieces <- c("Tous" = "Tous", "1-3P" = "1-3P", unique(data$Pieces))
all_epoque <- c("Tous" = "Tous", unique(data$Epoque_Construction))


# ==================================================================
# --- 5. Définir l'Interface Utilisateur (UI) ---
# ==================================================================
ui <- fluidPage(
  titlePanel("Analyse des Loyers - Paris 2024"),
  
  # Disposition avec une barre latérale et un panneau principal
  sidebarLayout(
    
    # --- Panneau de la barre latérale (Filtres) ---
    sidebarPanel(
      width = 3,
      h4("Filtres de sélection"),
      
      # Filtre pour la Zone (obligatoire)
      selectInput("zoneFilter", "Périmètre (Zone):",  
                  choices = zone_choices,
                  selected = "zone 1",
                  multiple = TRUE),
      
      # Filtre pour le Type de logement
      selectInput("typeFilter", "Type de logement:",  
                  choices = all_types, selected = "Tous"),
      
      # Filtre pour le Nombre de pièces
      selectInput("piecesFilter", "Nombre de pièces:",  
                  choices = all_pieces, selected = "Tous"),
      
      # Filtre pour l'Époque de construction
      selectInput("epoqueFilter", "Époque de construction:",  
                  choices = all_epoque, selected = "Tous")
    ),
    
    # --- Panneau principal (Onglets) ---
    mainPanel(
      width = 9,
      # Crée la structure des onglets
      tabsetPanel(
        
        # Onglet 1: Définitions
        tabPanel("Définitions",
                 br(),
                 fluidRow(
                   column(10, offset = 1,
                          h3("Définition des Loyers"),
                          p("L'application présente deux types de statistiques de loyer, toutes deux issues de l'annexe statistique de l'OLAP :"),
                          HTML("<ul>
                                     <li><strong>Loyer au m² :</strong> (Colonnes Q1, Médiane, Q3) Représente le loyer mensuel médian **par mètre carré**. C'est un indicateur clé du prix du marché immobilier.</li>
                                     <li><strong>Loyer Total :</strong> (Colonnes Q1.1, Médiane.1, Q3.1) Représente le loyer mensuel **total** (le loyer 'nu') payé par le locataire.</li>
                                   </ul>"),
                          p(strong("Important :"), "Ces loyers sont mesurés", strong("hors charges"), "(eau, chauffage collectif, entretien...). Le budget mensuel réel du locataire sera donc supérieur (Loyer Total + Provisions pour charges)."),
                          
                          hr(),
                          
                          h3("Composition des 7 Zones (Source: OLAP)"),
                          p("Le zonage regroupe des communes/arrondissements de l'agglomération parisienne ayant des niveaux de loyers privés similaires."),
                          HTML("<ul>
                                     <li><strong>Zone 1 :</strong> Les secteurs les plus chers. Comprend les <strong>Paris 6e, 7e, 8e, 16e</strong> et <strong>Neuilly-sur-Seine</strong>.</li>
                                     <br>
                                     <li><strong>Zone 2 :</strong> Le reste de Paris intra-muros (sauf 13e, 19e, 20e) et quelques communes très proches et chères. Comprend les <strong>Paris 1er, 2e, 3e, 4e, 5e, 9e, 10e, 11e, 12e, 14e, 15e, 17e, 18e</strong>, ainsi que <strong>Boulogne-Billancourt, Levallois-Perret, Saint-Mandé</strong> et <strong>Vincennes</strong>.</li>
                                     <br>
                                     <li><strong>Zone 3 :</strong> Les arrondissements de Paris restants. Comprend les <strong>Paris 13e, 19e</strong> et <strong>20e</strong>.</li>
                                     <br>
                                     <li><strong>Zone 4 :</strong> Un grand nombre de communes de la petite couronne et de la grande couronne proche (ex: <strong>Versailles, Asnières-sur-Seine, Clichy, Saint-Maur-des-Fossés, Rueil-Malmaison...</strong>).</li>
                                     <br>
                                     <li><strong>Zone 5 :</strong> Des communes de la grande couronne, un peu plus éloignées (ex: <strong>Melun, Champs-sur-Marne, Achères...</strong>).</li>
                                     <br>
                                     <li><strong>Zones 6 et 7 :</strong> Elles suivent la même logique et regroupent les communes de la grande couronne les plus éloignées ou celles où les loyers sont les plus modérés.</li>
                                   </ul>")
                   )
                 )
        ),
        
        # Onglet 2: Statistiques Résumées
        tabPanel("Statistiques Résumées",
                 br(),
                 # Bouton pour choisir la métrique (m² ou Total)
                 radioButtons("statsMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 hr(),
                 # L'interface utilisateur est générée dans le serveur (dynamique)
                 uiOutput("summaryStats")
        ),
        
        # Onglet 3: Classements
        tabPanel("Classements",
                 br(),
                 fluidRow(
                   column(5,
                          # Bouton pour choisir la métrique de classement
                          radioButtons("rankMetric", "Classer selon:",
                                       choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                                       selected = "m2", inline = TRUE)
                   ),
                   column(4,
                          # Champ numérique pour choisir le nombre de résultats
                          numericInput("rankN", "Nombre de résultats à afficher:",
                                       value = 10, min = 3, max = 100, step = 1, width = "200px")
                   )
                 ),
                 hr(),
                 fluidRow(
                   # Colonne pour les plus chers
                   column(6,
                          h4(textOutput("rankExpensiveTitle", inline = TRUE)),
                          uiOutput("topExpensiveUI") # Interface générée dans le serveur
                   ),
                   # Colonne pour les moins chers
                   column(6,
                          h4(textOutput("rankCheapTitle", inline = TRUE)),
                          uiOutput("topCheapUI") # Interface générée dans le serveur
                   )
                 )
        ),
        
        # Onglet 4: Distribution des Loyers (Graphique)
        tabPanel("Distribution des Loyers",
                 br(),
                 radioButtons("distribMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 hr(),
                 plotOutput("distribPlot", height = "600px")
        ),
        
        # --- ONGLETS SUPPRIMÉS ---
        # "Loyer / Époque" et "Loyer / Pièces" ont été retirés
        
        # Onglet 5: Dispersion EIQ (Graphique)
        tabPanel("Dispersion EIQ",
                 br(),
                 radioButtons("dispersionMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 hr(),
                 plotOutput("dispersionPlot", height = "600px")
        ),
        
        # Onglet 6: Simulateur de Loyer
        tabPanel("Simulateur de Loyer",
                 br(),
                 fluidRow(
                   column(4,
                          h4("Paramètres"),
                          radioButtons("simMetric", "Simuler sur:",
                                       choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                                       selected = "total", inline = TRUE),
                          hr(),
                          uiOutput("sliderUI"), # Slider dynamique généré par le serveur
                          hr(),
                          p(strong("Mode d'emploi:")),
                          p("1. Choisissez vos filtres (Zone, Type...) dans le panneau de gauche."),
                          p("2. Sélectionnez la simulation (m² ou Total)."),
                          p("3. Déplacez le slider pour définir votre loyer médian maximum."),
                          p("4. Le tableau à droite affiche les combinaisons correspondantes.")
                   ),
                   column(8,
                          h4("Logements correspondants (Loyer médian <= votre choix)"),
                          DT::dataTableOutput("simResultsTable") # Tableau de résultats
                   )
                 )
        ),
        
        # Onglet 7: Données filtrées (Tableau brut)
        tabPanel("Données filtrées",
                 br(),
                 DT::dataTableOutput("rentTable")
        )
      ) # fin tabsetPanel
    ) # fin mainPanel
  ) # fin sidebarLayout
) # fin fluidPage


# ==================================================================
# --- 6. Définir la logique du Serveur ---
# ==================================================================
server <- function(input, output, session) {
  
  # --- RÉACTIF 1: Données Filtrées (LA BASE) ---
  # Ce bloc 'reactive' est le cœur de l'application.
  # Il s'exécute à chaque fois qu'un filtre de la barre latérale est modifié.
  # Il renvoie un dataframe filtré que *tous les autres outputs* peuvent utiliser.
  filteredData <- reactive({
    req(data) # S'assurer que 'data' est chargé
    df <- data
    
    # Le filtre de zone est requis (pas de "Tous")
    req(input$zoneFilter) 
    df <- df %>% filter(Zone %in% input$zoneFilter)
    
    # Appliquer le filtre "Type" s'il n'est pas "Tous"
    if (input$typeFilter != "Tous") {
      df <- df %>% filter(Type == input$typeFilter)
    }
    
    # Appliquer le filtre "Pièces" (logique spéciale pour "1-3P")
    if (input$piecesFilter == "1-3P") {
      df <- df %>% filter(Pieces %in% c("1P", "2P", "3P"))
    } else if (input$piecesFilter != "Tous") {
      df <- df %>% filter(Pieces == input$piecesFilter)
    }
    
    # Appliquer le filtre "Epoque" s'il n'est pas "Tous"
    if (input$epoqueFilter != "Tous") {
      df <- df %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    
    # Renvoyer le dataframe filtré
    return(df)
  })
  
  # --- SORTIE: Statistiques Résumées (Onglet 2) ---
  output$summaryStats <- renderUI({
    # Dépend des données filtrées et du choix de métrique (m²/total)
    req(filteredData(), input$statsMetric) 
    
    df_plot <- filteredData() # Récupérer les données filtrées
    
    # --- 1. Calcul spécial du nombre de contrats ---
    # Ce calcul est complexe car il doit ignorer certains filtres
    # pour trouver la bonne ligne "globale" dans les données.
    df_for_contracts <- data 
    df_for_contracts <- df_for_contracts %>% filter(Zone %in% input$zoneFilter)
    
    if (input$typeFilter == "Tous") {
      df_for_contracts <- df_for_contracts %>% filter(Type == "ensemble")
    } else {
      df_for_contracts <- df_for_contracts %>% filter(Type == input$typeFilter)
    }
    
    if (input$epoqueFilter == "Tous") {
      df_for_contracts <- df_for_contracts %>% filter(Epoque_Construction == "(Non spécifié)")
    } else {
      df_for_contracts <- df_for_contracts %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    
    if (input$piecesFilter == "Tous") {
      df_for_contracts <- df_for_contracts %>% filter(Pieces == "(Non spécifié)")
    } else if (input$piecesFilter == "1-3P") {
      df_for_contracts <- df_for_contracts %>% filter(Pieces %in% c("1P", "2P", "3P"))
    } else {
      df_for_contracts <- df_for_contracts %>% filter(Pieces == input$piecesFilter)
    }
    total_contracts <- sum(df_for_contracts$obs, na.rm = TRUE)
    
    
    # --- 2. Définition des variables dynamiques ---
    # Choisir la bonne colonne (sym) et le bon libellé (label)
    # en fonction du choix de l'utilisateur (m² ou total)
    metric_col_sym <- if (input$statsMetric == "m2") sym("Loyer_m2_Mediane") else sym("Loyer_Total_Mediane")
    unit_label <- if (input$statsMetric == "m2") "€/m²" else "€"
    title_label <- if (input$statsMetric == "m2") paste0("Loyer Moyen par Zone (", unit_label, ")") else paste0("Loyer Total Moyen par Zone (", unit_label, ")")
    
    # --- 3. Calculs statistiques dynamiques ---
    df_stats <- df_plot %>% filter(!is.na(!!metric_col_sym)) # Filtrer les NA pour la métrique choisie
    
    # S'il n'y a pas de données, afficher un message
    if (nrow(df_stats) == 0) {
      return(HTML(paste0(
        "<div style='padding: 20px;'>",
        "...", # Message d'erreur (raccourci)
        "</div>"
      )))
    }
    
    # Calculer les stats (Moy, Min, Max) par zone
    stats_by_zone <- df_stats %>%
      group_by(Zone) %>%
      summarise(
        Loyer_Moyen = round(mean(!!metric_col_sym, na.rm = TRUE), 2),
        Loyer_Min = round(min(!!metric_col_sym, na.rm = TRUE), 2),
        Loyer_Max = round(max(!!metric_col_sym, na.rm = TRUE), 2),
        N_Observations = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Loyer_Moyen))
    
    # Calculer la complétude des données
    total_combinations <- nrow(df_plot)
    complete_combinations <- nrow(df_stats)
    completeness <- if (total_combinations > 0) round(complete_combinations / total_combinations * 100, 1) else 0
    
    # --- 4. Génération HTML (simplifiée) ---
    # Construit la sortie HTML en utilisant les variables calculées
    HTML(paste0(
      "<div style='padding: 20px;'>",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>Statistiques Générales</h3>",
      "<div style='background: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px;'>",
      "<p><strong>Nombre total de contrats de locations (base):</strong> ", format(total_contracts, big.mark = " "), "</p>",
      "<p><strong>Nombre de combinaisons statistiques (filtres):</strong> ", total_combinations, "</p>",
      "<p><strong>Combinaisons avec loyer (", input$statsMetric, "):</strong> ", complete_combinations, " (", completeness, "%)</p>", 
      "</div>",
      
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>", title_label, "</h3>", 
      
      "<table style='width: 100%; border-collapse: collapse; margin-bottom: 30px;'>",
      "<tr style='background: #3498db; color: white;'>",
      "<th style='padding: 10px; text-align: left;'>Zone</th>",
      "<th style='padding: 10px; text-align: right;'>Loyer Moyen</th>",
      "<th style='padding: 10px; text-align: right;'>Min</th>",
      "<th style='padding: 10px; text-align: right;'>Max</th>",
      "<th style='padding: 10px; text-align: right;'>N° Obs</th>",
      "</tr>",
      # Appliquer une fonction pour créer une ligne <tr> pour chaque zone
      paste0(apply(stats_by_zone, 1, function(row) {
        paste0("<tr style='border-bottom: 1px solid #ddd;'>",
               "<td style='padding: 8px;'>", row['Zone'], "</td>",
               "<td style='padding: 8px; text-align: right;'>", row['Loyer_Moyen'], " ", unit_label, "</td>", 
               "<td style='padding: 8px; text-align: right;'>", row['Loyer_Min'], " ", unit_label, "</td>", 
               "<td style='padding: 8px; text-align: right;'>", row['Loyer_Max'], " ", unit_label, "</td>", 
               "<td style='padding: 8px; text-align: right;'>", row['N_Observations'], "</td>",
               "</tr>")
      }), collapse = ""),
      "</table>",
      "</div>"
    ))
  })
  
  # --- SORTIES: Classements (Onglet 3) ---
  
  # Réactif 2: Données pour le classement
  # Prépare les données pour les deux listes "Top"
  rankData <- reactive({
    req(filteredData(), input$rankMetric)
    
    # Choisir la colonne de métrique
    metric_col_sym <- if (input$rankMetric == "m2") sym("Loyer_m2_Mediane") else sym("Loyer_Total_Mediane")
    
    # Filtrer les NA et créer l'étiquette "Config"
    df <- filteredData() %>%
      filter(!is.na(!!metric_col_sym)) %>%
      mutate(Config = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    # Renvoyer une liste contenant le dataframe et le symbole de la colonne
    list(df = df, metric_col_sym = metric_col_sym)
  })
  
  # Titres dynamiques (Top 10, Top 20...)
  output$rankExpensiveTitle <- renderText({
    req(input$rankN)
    paste("Top", input$rankN, "- Configurations les Plus Chères")
  })
  
  output$rankCheapTitle <- renderText({
    req(input$rankN)
    paste("Top", input$rankN, "- Configurations les Moins Chères")
  })
  
  # --- HELPER (SIMPLIFICATION): Fonction pour créer les listes HTML ---
  # Cette fonction est appelée par topExpensiveUI et topCheapUI
  # pour éviter de dupliquer le code de génération HTML.
  buildRankHTML <- function(data_frame, unit_label, background_color) {
    if (nrow(data_frame) == 0) {
      return(p("Aucune donnée à afficher."))
    }
    
    # Construire les items de la liste <li>
    list_items <- apply(data_frame, 1, function(row) {
      paste0("<li><strong>", row['Config'], "</strong>: ", 
             round(as.numeric(row['Value']), 2), " ", unit_label, "</li>")
    })
    
    # Retourner le HTML complet <ol>...</ol>
    HTML(paste0(
      "<ol style='background: ", background_color, "; padding: 20px 20px 20px 40px; border-radius: 5px; margin-bottom: 20px;'>",
      paste(list_items, collapse = ""),
      "</ol>"
    ))
  }
  
  # Sortie pour la liste des "Plus Chers"
  output$topExpensiveUI <- renderUI({
    req(rankData(), input$rankN)
    rank_data_list <- rankData()
    unit_label <- if (input$rankMetric == "m2") "€/m²" else "€"
    
    # Trier par ordre décroissant
    top_expensive <- rank_data_list$df %>%
      arrange(desc(!!rank_data_list$metric_col_sym)) %>%
      head(input$rankN) %>%
      mutate(Value = !!rank_data_list$metric_col_sym) 
    
    # Appel de la fonction helper
    buildRankHTML(top_expensive, unit_label, "#fadbd8") # Fond rouge
  })
  
  # Sortie pour la liste des "Moins Chers"
  output$topCheapUI <- renderUI({
    req(rankData(), input$rankN)
    rank_data_list <- rankData()
    unit_label <- if (input$rankMetric == "m2") "€/m²" else "€"
    
    # Trier par ordre croissant
    top_cheap <- rank_data_list$df %>%
      arrange(!!rank_data_list$metric_col_sym) %>% 
      head(input$rankN) %>%
      mutate(Value = !!rank_data_list$metric_col_sym) 
    
    # Appel de la fonction helper
    buildRankHTML(top_cheap, unit_label, "#d5f4e6") # Fond vert
  })
  
  
  # --- SORTIES: Graphiques (Onglets 4 & 5) ---
  
  # GRAPHIQUE 1: Distribution (PointRange)
  output$distribPlot <- renderPlot({
    req(filteredData(), input$distribMetric)
    plot_data_base <- filteredData()
    
    warning_text <- NULL
    
    # Logique pour Loyer / m²
    if (input$distribMetric == "m2") {
      plot_data <- plot_data_base %>%
        filter(!is.na(Loyer_m2_Mediane) & !is.na(Loyer_m2_Q1) & !is.na(Loyer_m2_Q3))
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      plot_data <- plot_data %>% mutate(GroupLabel = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
      
      # Limiter à 50 lignes pour éviter un graphique illisible
      if(nrow(plot_data) > 50) {
        plot_data <- plot_data %>% arrange(Loyer_m2_Mediane) %>% slice(1:50)
        warning_text <- "Note: Affichage des 50 premières lignes (triées). Affinez vos filtres."
      }
      
      ggplot(plot_data, aes(x = reorder(GroupLabel, Loyer_m2_Mediane), y = Loyer_m2_Mediane, color = Zone)) +
        geom_pointrange(aes(ymin = Loyer_m2_Q1, ymax = Loyer_m2_Q3), size = 0.8) +
        coord_flip() +  # Basculer les axes x et y
        labs(title = "Distribution des Loyers au m² (Médiane, Q1, Q3)", subtitle = warning_text,
             x = "Catégorie", y = "Loyer / m² (€)", color = "Zone") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 9), legend.position = "bottom")
      
      # Logique pour Loyer Total
    } else {
      plot_data <- plot_data_base %>%
        filter(!is.na(Loyer_Total_Mediane) & !is.na(Loyer_Total_Q1) & !is.na(Loyer_Total_Q3))
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      plot_data <- plot_data %>% mutate(GroupLabel = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
      if(nrow(plot_data) > 50) {
        plot_data <- plot_data %>% arrange(Loyer_Total_Mediane) %>% slice(1:50)
        warning_text <- "Note: Affichage des 50 premières lignes (triées). Affinez vos filtres."
      }
      
      ggplot(plot_data, aes(x = reorder(GroupLabel, Loyer_Total_Mediane), y = Loyer_Total_Mediane, color = Zone)) +
        geom_pointrange(aes(ymin = Loyer_Total_Q1, ymax = Loyer_Total_Q3), size = 0.8) +
        coord_flip() +  
        labs(title = "Distribution des Loyers Totaux (Médiane, Q1, Q3)", subtitle = warning_text,
             x = "Catégorie", y = "Loyer Total Mensuel (€)", color = "Zone") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 9), legend.position = "bottom")
    }
  })
  
  # --- BLOCS GRAPHIQUES SUPPRIMÉS ---
  # output$eraPlot et output$loyerPiecesPlot ont été retirés
  
  # GRAPHIQUE 2: Dispersion (Barres)
  output$dispersionPlot <- renderPlot({
    req(filteredData(), input$dispersionMetric)
    
    # Logique pour m²
    if (input$dispersionMetric == "m2") {
      plot_data <- filteredData() %>%
        filter(!is.na(Loyer_m2_Q1), !is.na(Loyer_m2_Q3), Zone != "(Non spécifié)") %>%
        mutate(EIQ_m2 = Loyer_m2_Q3 - Loyer_m2_Q1) # Calcul de l'Écart InterQuartile
      
      # Agréger par zone
      plot_summary <- plot_data %>% group_by(Zone) %>%
        summarise(Loyer_Med_Moyen = mean(Loyer_m2_Mediane, na.rm = TRUE), EIQ_Moyen = mean(EIQ_m2, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(EIQ_Moyen))
      
      if (nrow(plot_summary) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      # Graphique en barres
      ggplot(plot_summary, aes(x = EIQ_Moyen, y = reorder(Zone, EIQ_Moyen), fill = Loyer_Med_Moyen)) +
        geom_col() + 
        labs(title = "Dispersion des Loyers (Écart InterQuartile au m²)", x = "Écart InterQuartile Moyen au m² (€)", y = "Zone", fill = "Loyer Médian\nMoyen (€/m²)") +
        scale_fill_gradient(low = "#fee8c8", high = "#e34a33") + 
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
      # Logique pour Total
    } else {
      plot_data <- filteredData() %>%
        filter(!is.na(Loyer_Total_Q1), !is.na(Loyer_Total_Q3), Zone != "(Non spécifié)") %>%
        mutate(EIQ_Total = Loyer_Total_Q3 - Loyer_Total_Q1) # EIQ Total
      
      plot_summary <- plot_data %>% group_by(Zone) %>%
        summarise(Loyer_Med_Moyen = mean(Loyer_Total_Mediane, na.rm = TRUE), EIQ_Moyen = mean(EIQ_Total, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(EIQ_Moyen))
      
      if (nrow(plot_summary) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      ggplot(plot_summary, aes(x = EIQ_Moyen, y = reorder(Zone, EIQ_Moyen), fill = Loyer_Med_Moyen)) +
        geom_col() + 
        labs(title = "Dispersion des Loyers (Écart InterQuartile Total)", x = "Écart InterQuartile Moyen Total (€)", y = "Zone", fill = "Loyer Total\nMédian Moyen (€)") +
        scale_fill_gradient(low = "#fee8c8", high = "#e34a33") + 
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
    }
  })
  
  
  # --- SORTIES: Simulateur (Onglet 6) ---
  
  # 1. Générer le Slider dynamique
  output$sliderUI <- renderUI({
    req(filteredData(), input$simMetric)
    df <- filteredData()
    
    # Définir les variables en fonction du choix m²/total
    if (input$simMetric == "m2") {
      col_data <- df$Loyer_m2_Mediane
      label_text <- "Votre loyer maximum (€/m²):"
      step_val <- 0.5
    } else {
      col_data <- df$Loyer_Total_Mediane
      label_text <- "Votre loyer total maximum (€):"
      step_val <- 25
    }
    
    col_data <- col_data[!is.na(col_data)] # Retirer les NA
    
    # Définir les min/max du slider à partir des données filtrées
    if (length(col_data) == 0) {
      min_val <- 0
      max_val <- if (input$simMetric == "m2") 50 else 3000
      start_val <- if (input$simMetric == "m2") 25 else 1000
    } else {
      min_val <- floor(min(col_data))
      max_val <- ceiling(max(col_data))
      start_val <- round(median(col_data))
    }
    
    # Créer le slider
    sliderInput("simSlider",
                label = label_text,
                min = min_val,
                max = max_val,
                value = start_val,
                step = step_val,
                width = "100%")
  })
  
  # 2. Générer la table de résultats du simulateur
  output$simResultsTable <- DT::renderDataTable({
    # Nécessite que le slider soit prêt
    req(filteredData(), input$simSlider, input$simMetric)
    
    df <- filteredData()
    slider_val <- input$simSlider # Récupérer la valeur du slider
    
    # Filtrer les données en fonction de la valeur du slider
    if (input$simMetric == "m2") {
      results <- df %>%
        filter(Loyer_m2_Mediane <= slider_val) %>%
        arrange(desc(Loyer_m2_Mediane)) %>%
        select(Zone, Type, Pieces, Epoque_Construction, Loyer_m2_Mediane, Loyer_Total_Mediane, obs)
      cols_to_format <- c('Loyer_m2_Mediane', 'Loyer_Total_Mediane')
      
    } else {
      results <- df %>%
        filter(Loyer_Total_Mediane <= slider_val) %>%
        arrange(desc(Loyer_Total_Mediane)) %>%
        select(Zone, Type, Pieces, Epoque_Construction, Loyer_Total_Mediane, Loyer_m2_Mediane, obs)
      cols_to_format <- c('Loyer_Total_Mediane', 'Loyer_m2_Mediane')
    }
    
    # Afficher le tableau de résultats
    DT::datatable(
      results,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE, filter = 'none', class = 'stripe hover'
    ) %>%
      DT::formatRound(columns = cols_to_format, digits = 2) %>%
      DT::formatRound(columns = 'obs', digits = 0)
    
  })
  
  
  # --- SORTIE: Tableau de données brutes (Onglet 7) ---
  output$rentTable <- DT::renderDataTable({
    req(filteredData())
    
    # Sélectionner les colonnes à afficher
    display_data <- filteredData() %>%
      select(Zone, Type, Pieces, Epoque_Construction, Loyer_m2_Mediane, Loyer_m2_Q1, Loyer_m2_Q3, Loyer_Total_Mediane, Loyer_Total_Q1, Loyer_Total_Q3, obs)
    
    # Afficher le tableau avec filtres en haut des colonnes
    DT::datatable(
      display_data,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(width = '100px', targets = c(0,1,2,3)))),
      rownames = FALSE, filter = 'top', class = 'stripe hover'
    ) %>%
      DT::formatRound(columns = c(
        'Loyer_m2_Mediane', 'Loyer_m2_Q1', 'Loyer_m2_Q3',
        'Loyer_Total_Mediane', 'Loyer_Total_Q1', 'Loyer_Total_Q3'
      ), digits = 2) %>%
      DT::formatRound(columns = 'obs', digits = 0)
  })
}

# ==================================================================
# --- 7. Lancer l'application ---
# ==================================================================
shinyApp(ui = ui, server = server)