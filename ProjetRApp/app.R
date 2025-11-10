#
# Application Shiny pour les loyers - Version mise à jour
# Ajout de l'option de filtre "1-3P" pour les pièces
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
zone_choices <- unique(data$Zone)
zone_choices <- zone_choices[zone_choices != "(Non spécifié)"]

all_types <- c("Tous" = "Tous", unique(data$Type))

# --- MODIFIÉ : Ajout de l'option "1-3P" ---
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
      
      # Le selectInput utilise maintenant la nouvelle liste "all_pieces"
      selectInput("piecesFilter", "Nombre de pièces:",  
                  choices = all_pieces, selected = "Tous"),
      
      selectInput("epoqueFilter", "Époque de construction:",  
                  choices = all_epoque, selected = "Tous")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Définition des Zones",
                 br(),
                 fluidRow(
                   column(10, offset = 1,
                          h3("Composition des 7 Zones (Source: OLAP)"),
                          p("Le zonage regroupe des communes/arrondissements de l'agglomération parisienne ayant des niveaux de loyers privés similaires."),
                          br(),
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
        tabPanel("Graphique (Loyer / m²)",
                 br(),
                 plotOutput("rentPlot", height = "600px")
        ),
        tabPanel("Loyer / Époque",
                 br(),
                 plotOutput("eraPlot", height = "600px")
        ),
        tabPanel("Loyer / Pièces",
                 br(),
                 plotOutput("loyerPiecesPlot", height = "600px")
        ),
        tabPanel("Dispersion (EIQ)",
                 br(),
                 plotOutput("dispersionPlot", height = "600px")
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
    
    # Filtre Zone (multi-sélection)
    req(input$zoneFilter) 
    df <- df %>% filter(Zone %in% input$zoneFilter)
    
    # Filtre Type
    if (input$typeFilter != "Tous") {
      df <- df %>% filter(Type == input$typeFilter)
    }
    
    # --- MODIFIÉ : Logique de filtre pour Pièces ---
    if (input$piecesFilter == "1-3P") {
      # Cas spécial pour 1-3 Pièces
      df <- df %>% filter(Pieces %in% c("1P", "2P", "3P"))
    } else if (input$piecesFilter != "Tous") {
      # Cas standard (si "Tous" n'est pas sélectionné)
      df <- df %>% filter(Pieces == input$piecesFilter)
    }
    # Si "Tous" est sélectionné, aucun filtre n'est appliqué (comportement correct)
    
    # Filtre Epoque
    if (input$epoqueFilter != "Tous") {
      df <- df %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    return(df)
  })
  
  # Sortie: Statistiques Résumées
  output$summaryStats <- renderUI({
    req(filteredData())
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
    completeness <- if (total_obs > 0) round(complete_obs / total_obs * 100, 1) else 0
    
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
    req(filteredData())
    plot_data <- filteredData() %>%
      filter(!is.na(Loyer_m2_Mediane) & !is.na(Loyer_m2_Q1) & !is.na(Loyer_m2_Q3))
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher pour cette sélection."), size = 6) + theme_void())
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
  
  # --- Graphique : Loyer par Époque ---
  output$eraPlot <- renderPlot({
    req(filteredData())
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
      facet_wrap(~ Zone, scales = "free_y") + 
      labs(title = "Distribution des Loyers au m² par Époque de Construction",
           subtitle = "Basé on les filtres actuels, un graphique par zone sélectionnée",
           x = "Époque de Construction",
           y = "Loyer Médian / m² (€)") +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # --- Graphique : Loyer / Pièces (Effet de taille) ---
  output$loyerPiecesPlot <- renderPlot({
    req(filteredData())
    
    # La logique de filtre pour '1-3P' est déjà appliquée dans filteredData()
    # Donc nous avons juste besoin de filtrer pour les graphiques
    plot_data <- filteredData() %>%
      filter(
        Pieces %in% c("1P", "2P", "3P", "4P+"), 
        !is.na(Loyer_m2_Mediane),
        Zone != "(Non spécifié)"
      )
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + 
               labs(title = "Veuillez sélectionner des données valides (ex: 1P, 2P, 1-3P...)") +
               theme_void())
    }
    
    # Assurer l'ordre correct des pièces sur l'axe X
    plot_data$Pieces <- factor(plot_data$Pieces, levels = c("1P", "2P", "3P", "4P+"))
    
    ggplot(plot_data, aes(x = Pieces, y = Loyer_m2_Mediane, group = Zone, color = Zone)) +
      geom_line(size = 1.2, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.8) +
      labs(title = "Loyer Médian au m² par Nombre de Pièces et Zone",
           subtitle = "Permet de visualiser l'effet de taille (dégressivité du prix au m²)",
           x = "Nombre de Pièces",
           y = "Loyer Médian / m² (€)",
           color = "Zone") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  })
  
  # --- Graphique : Dispersion (EIQ) ---
  output$dispersionPlot <- renderPlot({
    req(filteredData())
    plot_data <- filteredData() %>%
      filter(
        !is.na(Loyer_m2_Q1), 
        !is.na(Loyer_m2_Q3),
        Zone != "(Non spécifié)"
      ) %>%
      mutate(EIQ_m2 = Loyer_m2_Q3 - Loyer_m2_Q1) 
    
    plot_summary <- plot_data %>%
      group_by(Zone) %>%
      summarise(
        Loyer_Med_Moyen = mean(Loyer_m2_Mediane, na.rm = TRUE),
        EIQ_Moyen = mean(EIQ_m2, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(!is.na(EIQ_Moyen))
    
    if (nrow(plot_summary) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + 
               labs(title = "Veuillez sélectionner des données valides.") +
               theme_void())
    }
    
    ggplot(plot_summary, aes(x = EIQ_Moyen, y = reorder(Zone, EIQ_Moyen), fill = Loyer_Med_Moyen)) +
      geom_col() + 
      labs(title = "Dispersion des Loyers (Écart InterQuartile Moyen)",
           subtitle = "Une barre plus longue = un marché plus hétérogène (grands écarts de prix)",
           x = "Écart InterQuartile Moyen au m² (€)",
           y = "Zone",
           fill = "Loyer Médian\nMoyen (€/m²)") +
      scale_fill_gradient(low = "#fee8c8", high = "#e34a33") + 
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
  })
  
  # Sortie: Tableau
  output$rentTable <- DT::renderDataTable({
    req(filteredData())
    
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