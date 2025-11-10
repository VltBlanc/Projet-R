#
# Application Shiny pour les loyers - Version mise à jour
# Chargement avec fileEncoding = "latin1" et remplacement des "" par NA
#

# --- 1. Charger les bibliothèques ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)

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
  # --- DÉBUT DE LA MODIFICATION UTILISATEUR ---
  
  # 1. Lire le fichier avec l'encodage latin1
  data <- read.csv(
    file_path, 
    sep = ";", 
    header = TRUE, 
    check.names = TRUE, # Important pour les noms de colonnes dupliqués
    stringsAsFactors = FALSE,
    fileEncoding = "latin1" # Tel que demandé
  )
  
  # 2. Remplacer les chaînes vides par NA
  data[data == ""] <- NA # Tel que demandé
  
  # --- FIN DE LA MODIFICATION UTILISATEUR ---
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
  # Convertir les colonnes de loyer en numérique
  # (les NA seront gérés correctement)
  mutate(
    across(starts_with("Loyer_"), as.numeric)
  )

# Remplacer les NA dans les colonnes de *filtre* (texte)
# pour une meilleure sélection dans les menus déroulants.
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
# (Aucun changement ici)
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
        tabPanel("Graphique (Loyer / m²)", 
                 br(),
                 plotOutput("rentPlot", height = "600px")
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
  
  # Expression réactive pour filtrer les données en fonction des entrées
  filteredData <- reactive({
    req(data) 
    
    df <- data
    
    # Appliquer les filtres de l'UI
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
  
  # Sortie: Graphique
  output$rentPlot <- renderPlot({
    
    # 3. Ignorer les lignes avec des données manquantes *pour le graphique*
    plot_data <- filteredData() %>%
      filter(
        !is.na(Loyer_m2_Mediane) & 
          !is.na(Loyer_m2_Q1) & 
          !is.na(Loyer_m2_Q3)
      )
    
    if (nrow(plot_data) == 0) {
      return(
        ggplot() + 
          geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher pour cette sélection."), size = 6) +
          theme_void()
      )
    }
    
    plot_data <- plot_data %>%
      mutate(GroupLabel = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    warning_text <- NULL
    if(nrow(plot_data) > 50) {
      plot_data <- plot_data %>% slice(1:50)
      warning_text <- "Note: Affichage des 50 premières lignes. Affinez vos filtres pour plus de détails."
    }
    
    ggplot(plot_data, aes(x = reorder(GroupLabel, Loyer_m2_Mediane), 
                          y = Loyer_m2_Mediane, 
                          color = Zone)) +
      geom_pointrange(aes(ymin = Loyer_m2_Q1, ymax = Loyer_m2_Q3), size = 0.8) +
      coord_flip() + 
      labs(
        title = "Distribution des Loyers au m² (Médiane, Q1, Q3)",
        subtitle = warning_text,
        x = "Catégorie (Zone / Type / Pièces / Époque)",
        y = "Loyer / m² (€)",
        color = "Zone"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.y = element_text(size = 9),
        legend.position = "bottom"
      )
  })
  
  # Sortie: Tableau
  output$rentTable <- DT::renderDataTable({
    
    # Le tableau utilise les données filtrées, mais *n'ignore pas* les lignes
    # avec des NA, pour que l'utilisateur puisse tout voir.
    display_data <- filteredData() %>%
      select(
        Zone, Type, Pieces, Epoque_Construction, 
        Loyer_m2_Mediane, Loyer_m2_Q1, Loyer_m2_Q3, Loyer_m2_Moyenne,
        Loyer_Total_Mediane, Loyer_Total_Q1, Loyer_Total_Q3, Loyer_Total_Moyenne,
        obs
      )
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(width = '100px', targets = c(0,1,2,3)))
      ),
      rownames = FALSE,
      filter = 'top',
      class = 'stripe hover'
    ) %>%
      DT::formatRound(columns = c(
        'Loyer_m2_Mediane', 'Loyer_m2_Q1', 'Loyer_m2_Q3', 'Loyer_m2_Moyenne',
        'Loyer_Total_Mediane', 'Loyer_Total_Q1', 'Loyer_Total_Q3', 'Loyer_Total_Moyenne'
      ), digits = 2)
  })
  
}

# --- 5. Lancer l'application ---
shinyApp(ui = ui, server = server)