#
# Application Shiny pour les loyers - Version 3
# Regroupement des graphiques (Loyer m² vs Loyer Total) via radioButtons
# Ajout d'un onglet "Simulateur de Loyer" avec slider dynamique
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

# Nettoyer et renommer les colonnes
data <- data %>%
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
    # 'obs' est déjà au bon nom si 'check.names = TRUE' est utilisé
  ) %>%
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

# Créer des listes pour les menus de filtre
zone_choices <- unique(data$Zone)
zone_choices <- zone_choices[zone_choices != "(Non spécifié)"]

all_types <- c("Tous" = "Tous", unique(data$Type))
all_pieces <- c("Tous" = "Tous", "1-3P" = "1-3P", unique(data$Pieces))
all_epoque <- c("Tous" = "Tous", unique(data$Epoque_Construction))


# --- 3. Définir l'Interface Utilisateur (UI) ---
ui <- fluidPage(
  titlePanel("Analyse des Loyers - Paris 2024"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      h4("Filtres de sélection"),
      
      selectInput("zoneFilter", "Périmètre (Zone):",  
                  choices = zone_choices,
                  selected = "zone 1",
                  multiple = TRUE),
      
      selectInput("typeFilter", "Type de logement:",  
                  choices = all_types, selected = "Tous"),
      
      selectInput("piecesFilter", "Nombre de pièces:",  
                  choices = all_pieces, selected = "Tous"),
      
      selectInput("epoqueFilter", "Époque de construction:",  
                  choices = all_epoque, selected = "Tous")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # --- Onglet Définitions (inchangé) ---
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
        tabPanel("Statistiques Résumées",
                 br(),
                 uiOutput("summaryStats")
        ),
        
        # --- GRAPHIQUES MODIFIÉS ---
        tabPanel("Distribution des Loyers",
                 br(),
                 radioButtons("distribMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 hr(),
                 plotOutput("distribPlot", height = "600px")
        ),
        
        tabPanel("Loyer / Époque",
                 br(),
                 radioButtons("eraMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 hr(),
                 plotOutput("eraPlot", height = "600px")
        ),
        
        tabPanel("Loyer / Pièces",
                 br(),
                 radioButtons("piecesMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "total", inline = TRUE),
                 hr(),
                 plotOutput("loyerPiecesPlot", height = "600px")
        ),
        
        tabPanel("Dispersion EIQ",
                 br(),
                 radioButtons("dispersionMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 hr(),
                 plotOutput("dispersionPlot", height = "600px")
        ),
        
        # --- NOUVEL ONGLET : SIMULATEUR ---
        tabPanel("Simulateur de Loyer",
                 br(),
                 fluidRow(
                   column(4,
                          h4("Paramètres"),
                          radioButtons("simMetric", "Simuler sur:",
                                       choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                                       selected = "total", inline = TRUE),
                          hr(),
                          uiOutput("sliderUI"),
                          hr(),
                          p(strong("Mode d'emploi:")),
                          p("1. Choisissez vos filtres (Zone, Type...) dans le panneau de gauche."),
                          p("2. Sélectionnez la simulation (m² ou Total)."),
                          p("3. Déplacez le slider pour définir votre loyer médian maximum."),
                          p("4. Le tableau à droite affiche les combinaisons correspondantes.")
                   ),
                   column(8,
                          h4("Logements correspondants (Loyer médian <= votre choix)"),
                          DT::dataTableOutput("simResultsTable")
                   )
                 )
        ),
        
        tabPanel("Données filtrées",
                 br(),
                 DT::dataTableOutput("rentTable")
        )
      )
    )
  )
)

# --- 4. Définir la logique du Serveur ---
server <- function(input, output, session) {
  
  # Expression réactive pour les filtres standards
  filteredData <- reactive({
    req(data)  
    df <- data
    req(input$zoneFilter) 
    df <- df %>% filter(Zone %in% input$zoneFilter)
    
    if (input$typeFilter != "Tous") {
      df <- df %>% filter(Type == input$typeFilter)
    }
    
    if (input$piecesFilter == "1-3P") {
      df <- df %>% filter(Pieces %in% c("1P", "2P", "3P"))
    } else if (input$piecesFilter != "Tous") {
      df <- df %>% filter(Pieces == input$piecesFilter)
    }
    
    if (input$epoqueFilter != "Tous") {
      df <- df %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    return(df)
  })
  
  # Sortie: Statistiques Résumées (basé sur le m² par défaut)
  output$summaryStats <- renderUI({
    req(filteredData())
    
    df_plot <- filteredData()
    
    # Calculer le nombre total de CONTRATS
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
      total_contracts <- sum(df_for_contracts$obs, na.rm = TRUE)
      
    } else if (input$piecesFilter == "1-3P") {
      df_for_contracts <- df_for_contracts %>% filter(Pieces %in% c("1P", "2P", "3P"))
      total_contracts <- sum(df_for_contracts$obs, na.rm = TRUE)
      
    } else {
      df_for_contracts <- df_for_contracts %>% filter(Pieces == input$piecesFilter)
      total_contracts <- sum(df_for_contracts$obs, na.rm = TRUE)
    }
    
    
    # Calculer les statistiques pour les classements (basé m²)
    stats_by_zone <- df_plot %>%
      filter(!is.na(Loyer_m2_Mediane)) %>%
      group_by(Zone) %>%
      summarise(
        Loyer_Moyen = round(mean(Loyer_m2_Mediane, na.rm = TRUE), 2),
        Loyer_Min = round(min(Loyer_m2_Mediane, na.rm = TRUE), 2),
        Loyer_Max = round(max(Loyer_m2_Mediane, na.rm = TRUE), 2),
        N_Observations = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(Loyer_Moyen))
    
    top_expensive <- df_plot %>%
      filter(!is.na(Loyer_m2_Mediane)) %>%
      arrange(desc(Loyer_m2_Mediane)) %>%
      head(5) %>%
      mutate(Config = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    top_cheap <- df_plot %>%
      filter(!is.na(Loyer_m2_Mediane)) %>%
      arrange(Loyer_m2_Mediane) %>%
      head(5) %>%
      mutate(Config = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    total_combinations <- nrow(df_plot)
    complete_combinations <- sum(!is.na(df_plot$Loyer_m2_Mediane))
    completeness <- if (total_combinations > 0) round(complete_combinations / total_combinations * 100, 1) else 0
    
    # Créer l'HTML (inchangé)
    HTML(paste0(
      "<div style='padding: 20px;'>",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>Statistiques Générales</h3>",
      "<div style='background: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px;'>",
      "<p><strong>Nombre total de contrats de locations (base):</strong> ", format(total_contracts, big.mark = " "), "</p>",
      "<p><strong>Nombre de combinaisons statistiques:</strong> ", total_combinations, "</p>",
      "<p><strong>Combinaisons avec loyer:</strong> ", complete_combinations, " (", completeness, "%)</p>",
      "</div>",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>Loyer Moyen par Zone (€/m²)</h3>",
      "<table style='width: 100%; border-collapse: collapse; margin-bottom: 30px;'>",
      "<tr style='background: #3498db; color: white;'>",
      "<th style='padding: 10px; text-align: left;'>Zone</th>",
      "<th style='padding: 10px; text-align: right;'>Loyer Moyen</th>",
      "<th style='padding: 10px; text-align: right;'>Min</th>",
      "<th style='padding: 10px; text-align: right;'>Max</th>",
      "<th style='padding: 10px; text-align: right;'>N° Obs</th>",
      "</tr>",
      paste0(apply(stats_by_zone, 1, function(row) {
        paste0("<tr style='border-bottom: 1px solid #ddd;'>",
               "<td style='padding: 8px;'>", row['Zone'], "</td>",
               "<td style='padding: 8px; text-align: right;'>", row['Loyer_Moyen'], " €</td>",
               "<td style='padding: 8px; text-align: right;'>", row['Loyer_Min'], " €</td>",
               "<td style='padding: 8px; text-align: right;'>", row['Loyer_Max'], " €</td>",
               "<td style='padding: 8px; text-align: right;'>", row['N_Observations'], "</td>",
               "</tr>")
      }), collapse = ""),
      "</table>",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;'>Top 5 - Configurations les Plus Chères (au m²)</h3>",
      "<ol style='background: #fadbd8; padding: 20px; border-radius: 5px; margin-bottom: 20px;'>",
      paste0(apply(top_expensive, 1, function(row) {
        paste0("<li><strong>", row['Config'], "</strong>: ", round(as.numeric(row['Loyer_m2_Mediane']), 2), " €/m²</li>")
      }), collapse = ""),
      "</ol>",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #27ae60; padding-bottom: 10px;'>Top 5 - Configurations les Moins Chères (au m²)</h3>",
      "<ol style='background: #d5f4e6; padding: 20px; border-radius: 5px;'>",
      paste0(apply(top_cheap, 1, function(row) {
        paste0("<li><strong>", row['Config'], "</strong>: ", round(as.numeric(row['Loyer_m2_Mediane']), 2), " €/m²</li>")
      }), collapse = ""),
      "</ol>",
      "</div>"
    ))
  })
  
  # --- GRAPHIQUE 1: PointRange (Distributions) ---
  output$distribPlot <- renderPlot({
    req(filteredData(), input$distribMetric)
    plot_data_base <- filteredData()
    
    warning_text <- NULL
    
    if (input$distribMetric == "m2") {
      plot_data <- plot_data_base %>%
        filter(!is.na(Loyer_m2_Mediane) & !is.na(Loyer_m2_Q1) & !is.na(Loyer_m2_Q3))
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher pour cette sélection."), size = 6) + theme_void())
      
      plot_data <- plot_data %>% mutate(GroupLabel = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
      if(nrow(plot_data) > 50) {
        plot_data <- plot_data %>% arrange(Loyer_m2_Mediane) %>% slice(1:50)
        warning_text <- "Note: Affichage des 50 premières lignes (triées). Affinez vos filtres."
      }
      
      ggplot(plot_data, aes(x = reorder(GroupLabel, Loyer_m2_Mediane), y = Loyer_m2_Mediane, color = Zone)) +
        geom_pointrange(aes(ymin = Loyer_m2_Q1, ymax = Loyer_m2_Q3), size = 0.8) +
        coord_flip() +  
        labs(title = "Distribution des Loyers au m² (Médiane, Q1, Q3)", subtitle = warning_text,
             x = "Catégorie", y = "Loyer / m² (€)", color = "Zone") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 9), legend.position = "bottom")
      
    } else {
      # Loyer Total
      plot_data <- plot_data_base %>%
        filter(!is.na(Loyer_Total_Mediane) & !is.na(Loyer_Total_Q1) & !is.na(Loyer_Total_Q3))
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher pour cette sélection."), size = 6) + theme_void())
      
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
  
  # --- GRAPHIQUE 2: Loyer par Époque ---
  output$eraPlot <- renderPlot({
    req(filteredData(), input$eraMetric)
    
    if (input$eraMetric == "m2") {
      plot_data <- filteredData() %>%
        filter(Epoque_Construction != "(Non spécifié)", !is.na(Loyer_m2_Mediane))
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      ggplot(plot_data, aes(x = Epoque_Construction, y = Loyer_m2_Mediane, fill = Epoque_Construction)) +
        geom_boxplot(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~ Zone, scales = "free_y") + 
        labs(title = "Distribution des Loyers au m² par Époque", x = "Époque de Construction", y = "Loyer Médian / m² (€)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
    } else {
      # Loyer Total
      plot_data <- filteredData() %>%
        filter(Epoque_Construction != "(Non spécifié)", !is.na(Loyer_Total_Mediane))
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      ggplot(plot_data, aes(x = Epoque_Construction, y = Loyer_Total_Mediane, fill = Epoque_Construction)) +
        geom_boxplot(alpha = 0.8, show.legend = FALSE) +
        facet_wrap(~ Zone, scales = "free_y") + 
        labs(title = "Distribution des Loyers Totaux par Époque", x = "Époque de Construction", y = "Loyer Total Médian (€)") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # --- GRAPHIQUE 3: Loyer / Pièces ---
  output$loyerPiecesPlot <- renderPlot({
    req(filteredData(), input$piecesMetric)
    
    if (input$piecesMetric == "m2") {
      plot_data <- filteredData() %>%
        filter(Pieces %in% c("1P", "2P", "3P", "4P+"), !is.na(Loyer_m2_Mediane), Zone != "(Non spécifié)")
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      plot_data$Pieces <- factor(plot_data$Pieces, levels = c("1P", "2P", "3P", "4P+"))
      
      ggplot(plot_data, aes(x = Pieces, y = Loyer_m2_Mediane, group = Zone, color = Zone)) +
        geom_line(size = 1.2, alpha = 0.8) + geom_point(size = 3, alpha = 0.8) +
        labs(title = "Loyer Médian au m² par Nombre de Pièces et Zone", x = "Nombre de Pièces", y = "Loyer Médian / m² (€)", color = "Zone") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
      
    } else {
      # Loyer Total
      plot_data <- filteredData() %>%
        filter(Pieces %in% c("1P", "2P", "3P", "4P+"), !is.na(Loyer_Total_Mediane), Zone != "(Non spécifié)")
      
      if (nrow(plot_data) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      plot_data$Pieces <- factor(plot_data$Pieces, levels = c("1P", "2P", "3P", "4P+"))
      
      ggplot(plot_data, aes(x = Pieces, y = Loyer_Total_Mediane, group = Zone, color = Zone)) +
        geom_line(size = 1.2, alpha = 0.8) + geom_point(size = 3, alpha = 0.8) +
        labs(title = "Loyer Total Médian par Nombre de Pièces et Zone", x = "Nombre de Pièces", y = "Loyer Total Médian (€)", color = "Zone") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
    }
  })
  
  # --- GRAPHIQUE 4: Dispersion EIQ ---
  output$dispersionPlot <- renderPlot({
    req(filteredData(), input$dispersionMetric)
    
    if (input$dispersionMetric == "m2") {
      plot_data <- filteredData() %>%
        filter(!is.na(Loyer_m2_Q1), !is.na(Loyer_m2_Q3), Zone != "(Non spécifié)") %>%
        mutate(EIQ_m2 = Loyer_m2_Q3 - Loyer_m2_Q1) 
      
      plot_summary <- plot_data %>% group_by(Zone) %>%
        summarise(Loyer_Med_Moyen = mean(Loyer_m2_Mediane, na.rm = TRUE), EIQ_Moyen = mean(EIQ_m2, na.rm = TRUE), .groups = 'drop') %>%
        filter(!is.na(EIQ_Moyen))
      
      if (nrow(plot_summary) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      ggplot(plot_summary, aes(x = EIQ_Moyen, y = reorder(Zone, EIQ_Moyen), fill = Loyer_Med_Moyen)) +
        geom_col() + 
        labs(title = "Dispersion des Loyers (Écart InterQuartile au m²)", x = "Écart InterQuartile Moyen au m² (€)", y = "Zone", fill = "Loyer Médian\nMoyen (€/m²)") +
        scale_fill_gradient(low = "#fee8c8", high = "#e34a33") + 
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
    } else {
      # Loyer Total
      plot_data <- filteredData() %>%
        filter(!is.na(Loyer_Total_Q1), !is.na(Loyer_Total_Q3), Zone != "(Non spécifié)") %>%
        mutate(EIQ_Total = Loyer_Total_Q3 - Loyer_Total_Q1) # Calcul de l'Écart InterQuartile Total
      
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
  
  
  # --- LOGIQUE DU SIMULATEUR ---
  
  # 1. Générer le Slider (UI dynamique)
  output$sliderUI <- renderUI({
    req(filteredData(), input$simMetric)
    df <- filteredData()
    
    label_text <- ""
    step_val <- 1
    
    if (input$simMetric == "m2") {
      col_data <- df$Loyer_m2_Mediane
      label_text <- "Votre loyer maximum (€/m²):"
      step_val <- 0.5
    } else {
      col_data <- df$Loyer_Total_Mediane
      label_text <- "Votre loyer total maximum (€):"
      step_val <- 25
    }
    
    col_data <- col_data[!is.na(col_data)]
    
    if (length(col_data) == 0) {
      # Valeurs par défaut si aucune donnée
      min_val <- 0
      max_val <- if (input$simMetric == "m2") 50 else 3000
      start_val <- if (input$simMetric == "m2") 25 else 1000
    } else {
      min_val <- floor(min(col_data))
      max_val <- ceiling(max(col_data))
      start_val <- round(median(col_data))
    }
    
    sliderInput("simSlider",
                label = label_text,
                min = min_val,
                max = max_val,
                value = start_val,
                step = step_val,
                width = "100%")
  })
  
  # 2. Générer la table de résultats (DataTable)
  output$simResultsTable <- DT::renderDataTable({
    # Requiert que le slider soit prêt
    req(filteredData(), input$simSlider, input$simMetric)
    
    df <- filteredData()
    slider_val <- input$simSlider
    
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
    
    DT::datatable(
      results,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
      rownames = FALSE, filter = 'none', class = 'stripe hover'
    ) %>%
      DT::formatRound(columns = cols_to_format, digits = 2) %>%
      DT::formatRound(columns = 'obs', digits = 0)
    
  })
  
  
  # Sortie: Tableau de données brutes
  output$rentTable <- DT::renderDataTable({
    req(filteredData())
    
    display_data <- filteredData() %>%
      select(Zone, Type, Pieces, Epoque_Construction, Loyer_m2_Mediane, Loyer_m2_Q1, Loyer_m2_Q3, Loyer_Total_Mediane, Loyer_Total_Q1, Loyer_Total_Q3, obs)
    
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

# --- 5. Lancer l'application ---
shinyApp(ui = ui, server = server)