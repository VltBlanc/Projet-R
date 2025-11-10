#
# Application Shiny pour les loyers - Version mise à jour
# Suppression des graphiques araignée et ajout de nouvelles analyses.
#

# --- 1. Charger les bibliothèques ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)        # Pour les tableaux interactifs

# --- 2. Charger et préparer les données ---

file_path <- "Paris2024-annexe-statistique-diffusion.csv"

if (!file.exists(file_path)) {
  print("ERREUR : Fichier 'Paris2024-annexe-statistique-diffusion.csv' non trouvé.")
  print("Veuillez placer le fichier CSV dans le même dossier que app.R")
  
  data <- data.frame(
    périmètre = "Erreur", type = "Fichier", nop = "Non", epco = "Trouvé",
    Q1 = 0, Médiane = 0, Q3 = 0, Moyenne = 0,
    Q1.1 = 0, Médiane.1 = 0, Q3.1 = 0, Moyenne.1 = 0,
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
  ) %>%
  mutate(
    across(starts_with("Loyer_"), as.numeric)
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
all_zones <- c("Tous" = "Tous", unique(data$Zone))
all_types <- c("Tous" = "Tous", unique(data$Type))
all_pieces <- c("Tous" = "Tous", unique(data$Pieces))
all_epoque <- c("Tous" = "Tous", unique(data$Epoque_Construction))


# --- 3. Définir l'Interface Utilisateur (UI) ---
ui <- fluidPage(
  titlePanel("Analyse des Loyers - Paris 2024"),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      h4("Filtres de sélection"),
      
      selectInput("zoneFilter", "Périmètre (Zone):",  
                  choices = all_zones, selected = "Tous"),
      
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
        tabPanel("Statistiques Résumées",
                 br(),
                 uiOutput("summaryStats")
        ),
        tabPanel("Graphique (Loyer / m²)",
                 br(),
                 plotOutput("rentPlot", height = "600px")
        ),
        tabPanel("Loyer / Époque", # NOUVEAU GRAPHIQUE
                 br(),
                 plotOutput("eraPlot", height = "600px")
        ),
        tabPanel("Total vs m²", # NOUVEAU GRAPHIQUE
                 br(),
                 plotOutput("scatterPlot", height = "600px")
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
  
  # Expression réactive pour les filtres standards (Graphique 1 et Tableau)
  filteredData <- reactive({
    req(data)  
    df <- data
    
    if (input$zoneFilter != "Tous") {
      df <- df %>% filter(Zone == input$zoneFilter)
    }
    if (input$typeFilter != "Tous") {
      df <- df %>% filter(Type == input$typeFilter)
    }
    if (input$piecesFilter != "Tous") {
      df <- df %>% filter(Pieces == input$piecesFilter)
    }
    if (input$epoqueFilter != "Tous") {
      df <- df %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    return(df)
  })
  
  # Sortie: Statistiques Résumées
  output$summaryStats <- renderUI({
    df <- filteredData()
    
    # Calculer les statistiques
    stats_by_zone <- df %>%
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
    
    # Top 5 configurations les plus chères
    top_expensive <- df %>%
      filter(!is.na(Loyer_m2_Mediane)) %>%
      arrange(desc(Loyer_m2_Mediane)) %>%
      head(5) %>%
      mutate(Config = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    # Top 5 configurations les moins chères
    top_cheap <- df %>%
      filter(!is.na(Loyer_m2_Mediane)) %>%
      arrange(Loyer_m2_Mediane) %>%
      head(5) %>%
      mutate(Config = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    # Statistiques globales
    total_obs <- nrow(df)
    complete_obs <- sum(!is.na(df$Loyer_m2_Mediane))
    completeness <- round(complete_obs / total_obs * 100, 1)
    
    # Créer l'HTML
    HTML(paste0(
      "<div style='padding: 20px;'>",
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>Statistiques Générales</h3>",
      "<div style='background: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px;'>",
      "<p><strong>Nombre total d'observations:</strong> ", total_obs, "</p>",
      "<p><strong>Observations complètes (avec loyer):</strong> ", complete_obs, " (", completeness, "%)</p>",
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
      
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #e74c3c; padding-bottom: 10px;'>Top 5 - Configurations les Plus Chères</h3>",
      "<ol style='background: #fadbd8; padding: 20px; border-radius: 5px; margin-bottom: 20px;'>",
      paste0(apply(top_expensive, 1, function(row) {
        paste0("<li><strong>", row['Config'], "</strong>: ", round(as.numeric(row['Loyer_m2_Mediane']), 2), " €/m²</li>")
      }), collapse = ""),
      "</ol>",
      
      "<h3 style='color: #2c3e50; border-bottom: 2px solid #27ae60; padding-bottom: 10px;'>Top 5 - Configurations les Moins Chères</h3>",
      "<ol style='background: #d5f4e6; padding: 20px; border-radius: 5px;'>",
      paste0(apply(top_cheap, 1, function(row) {
        paste0("<li><strong>", row['Config'], "</strong>: ", round(as.numeric(row['Loyer_m2_Mediane']), 2), " €/m²</li>")
      }), collapse = ""),
      "</ol>",
      "</div>"
    ))
  })
  
  # Sortie: Graphique 1 (PointRange)
  output$rentPlot <- renderPlot({
    
    plot_data <- filteredData() %>%
      filter(!is.na(Loyer_m2_Mediane) & !is.na(Loyer_m2_Q1) & !is.na(Loyer_m2_Q3))
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
    }
    
    plot_data <- plot_data %>% mutate(GroupLabel = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    warning_text <- NULL
    if(nrow(plot_data) > 50) {
      plot_data <- plot_data %>% slice(1:50)
      warning_text <- "Note: Affichage des 50 premières lignes. Affinez vos filtres."
    }
    
    ggplot(plot_data, aes(x = reorder(GroupLabel, Loyer_m2_Mediane), y = Loyer_m2_Mediane, color = Zone)) +
      geom_pointrange(aes(ymin = Loyer_m2_Q1, ymax = Loyer_m2_Q3), size = 0.8) +
      coord_flip() +  
      labs(title = "Distribution des Loyers au m² (Médiane, Q1, Q3)", subtitle = warning_text,
           x = "Catégorie", y = "Loyer / m² (€)", color = "Zone") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 9), legend.position = "bottom")
  })
  
  # --- NOUVEAU GRAPHIQUE 1 : Loyer par Époque ---
  output$eraPlot <- renderPlot({
    plot_data <- filteredData() %>%
      filter(Epoque_Construction != "(Non spécifié)", !is.na(Loyer_m2_Mediane))
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + 
               labs(title = "Veuillez désélectionner '(Non spécifié)' pour l'époque de construction.") +
               theme_void())
    }
    
    ggplot(plot_data, aes(x = Epoque_Construction, y = Loyer_m2_Mediane, fill = Epoque_Construction)) +
      geom_boxplot(alpha = 0.8, show.legend = FALSE) +
      labs(title = "Distribution des Loyers au m² par Époque de Construction",
           subtitle = "Basé sur les filtres actuels",
           x = "Époque de Construction",
           y = "Loyer Médian / m² (€)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # --- NOUVEAU GRAPHIQUE 2 : Scatter Plot Total vs m² ---
  output$scatterPlot <- renderPlot({
    plot_data <- filteredData() %>%
      filter(!is.na(Loyer_m2_Mediane), !is.na(Loyer_Total_Mediane)) %>%
      filter(Pieces != "(Non spécifié)", Zone != "(Non spécifié)")
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + 
               labs(title = "Veuillez sélectionner des filtres valides.") +
               theme_void())
    }
    
    ggplot(plot_data, aes(x = Loyer_m2_Mediane, y = Loyer_Total_Mediane)) +
      geom_point(aes(color = Zone, shape = Pieces), alpha = 0.8, size = 3.5) +
      labs(title = "Loyer Total vs. Loyer au m²",
           subtitle = "Coloré par Zone, Forme par Nombre de Pièces",
           x = "Loyer Médian / m² (€)",
           y = "Loyer Médian Total (€)",
           color = "Zone",
           shape = "Pièces") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", legend.box = "vertical", plot.title = element_text(face = "bold"))
  })
  
  # Sortie: Tableau
  output$rentTable <- DT::renderDataTable({
    
    display_data <- filteredData() %>%
      select(Zone, Type, Pieces, Epoque_Construction, Loyer_m2_Mediane, Loyer_m2_Q1, Loyer_m2_Q3, Loyer_m2_Moyenne,
             Loyer_Total_Mediane, Loyer_Total_Q1, Loyer_Total_Q3, Loyer_Total_Moyenne, obs)
    
    DT::datatable(
      display_data,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE,
                     columnDefs = list(list(width = '100px', targets = c(0,1,2,3)))),
      rownames = FALSE, filter = 'top', class = 'stripe hover'
    ) %>%
      DT::formatRound(columns = c(
        'Loyer_m2_Mediane', 'Loyer_m2_Q1', 'Loyer_m2_Q3', 'Loyer_m2_Moyenne',
        'Loyer_Total_Mediane', 'Loyer_Total_Q1', 'Loyer_Total_Q3', 'Loyer_Total_Moyenne'
      ), digits = 2)
  })
}

# --- 5. Lancer l'application ---
shinyApp(ui = ui, server = server)