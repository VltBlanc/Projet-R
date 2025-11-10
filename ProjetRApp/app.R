#
# Application Shiny pour les loyers - Version mise à jour
# Ajout des couleurs de remplissage (pfcol) au graphique fmsb.
#

# --- 1. Charger les bibliothèques ---
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)           # Pour les tableaux interactifs
library(fmsb)         # Pour le nouveau graphique araignée
library(tidyr)        # Pour pivot_wider et replace_na
library(RColorBrewer) # Pour les palettes de couleurs

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
        tabPanel("Graphique (Loyer / m²)", 
                 br(),
                 plotOutput("rentPlot", height = "600px")
        ),
        tabPanel("Graphique Araignée (fmsb)",
                 br(),
                 plotOutput("spiderPlot_fmsb", height = "600px")
        ),
        tabPanel("Graphique Araignée (ggplot)",
                 br(),
                 plotOutput("spiderPlot_ggplot", height = "600px")
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
      df <- df %>% filter(Type == inputinput$typeFilter)
    }
    if (input$piecesFilter != "Tous") {
      df <- df %>% filter(Pieces == input$piecesFilter)
    }
    if (input$epoqueFilter != "Tous") {
      df <- df %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    return(df)
  })
  
  # Données réactives pour les DEUX graphiques araignée
  radarData <- reactive({
    req(data)
    df <- data
    
    # 1. Appliquer les filtres
    if (input$typeFilter != "Tous") {
      df <- df %>% filter(Type == input$typeFilter)
    }
    if (input$epoqueFilter != "Tous") {
      df <- df %>% filter(Epoque_Construction == input$epoqueFilter)
    }
    if (input$zoneFilter != "Tous") {
      df <- df %>% filter(Zone == input$zoneFilter)
    }
    
    # 2. Préparer les données
    df_prepared <- df %>%
      filter(Pieces %in% c("1P", "2P", "3P", "4P+")) %>%
      filter(Zone != "(Non spécifié)") %>%
      filter(!is.na(Loyer_m2_Mediane)) %>%
      
      # S'il y a plusieurs entrées (ex: Type="Tous"),
      # on prend la moyenne pour éviter les duplicats.
      group_by(Zone, Pieces) %>%
      summarise(
        Loyer_m2_Mediane = mean(Loyer_m2_Mediane, na.rm = TRUE),
        .groups = 'drop' # Enlève le groupement
      ) %>%
      
      # S'assurer que les pièces sont des facteurs dans le bon ordre
      mutate(Pieces = factor(Pieces, levels = c("1P", "2P", "3P", "4P+")))
    
    return(df_prepared)
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
  
  # --- Sortie pour le GRAPHIQUE fmsb (MISE À JOUR) ---
  output$spiderPlot_fmsb <- renderPlot({
    
    plot_data_tidy <- radarData()
    
    if (nrow(plot_data_tidy) == 0) {
      return(plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Aucune donnée", axes=FALSE, xlab="", ylab=""))
    }
    
    # 1. Transformer les données : de long à large
    df_wide <- plot_data_tidy %>%
      tidyr::pivot_wider(
        names_from = Pieces,
        values_from = Loyer_m2_Mediane
      )
    
    # 2. Isoler les noms et les données numériques
    zone_names <- df_wide$Zone
    df_numeric <- df_wide %>% 
      select("1P", "2P", "3P", "4P+") %>%
      # Remplacer les NA (si une zone n'a pas de 4P par ex.) par 0
      mutate(across(everything(), ~tidyr::replace_na(., 0)))
    
    # 3. Gérer le cas où il n'y a pas de données après pivot
    if (nrow(df_numeric) == 0) {
      return(plot(NULL, xlim=c(0,1), ylim=c(0,1), main="Aucune donnée à afficher", axes=FALSE, xlab="", ylab=""))
    }
    
    # 4. Créer les lignes min/max requises par fmsb
    max_val <- ceiling(max(df_numeric) * 1.1)
    if (max_val == 0) max_val <- 10 # Éviter un graphique plat si 0
    min_val <- 0
    
    df_fmsb <- rbind(
      rep(max_val, 4), # Ligne 1: Max
      rep(min_val, 4), # Ligne 2: Min
      df_numeric       # Lignes 3..n: Données
    )
    
    # 5. Couleurs
    num_zones <- nrow(df_numeric)
    colors_border <- if(num_zones > 0) brewer.pal(min(max(3, num_zones), 9), "Set1")[1:num_zones] else "black"
    
    # --- AJOUT DES COULEURS DE REMPLISSAGE ---
    # On utilise adjustcolor (de base R) pour ajouter de la transparence
    colors_fill <- if(num_zones > 0) adjustcolor(colors_border, alpha.f = 0.2) else NA
    
    # 6. Dessiner le graphique
    par(mar=c(1, 1, 1, 1)) # Ajuster les marges
    radarchart(
      df_fmsb,
      axistype = 1, # Étiquettes d'axe au centre
      
      # --- Style des polygones ---
      pcol = colors_border,      # Couleur des lignes
      pfcol = colors_fill,       # <--- AJOUT ICI (Couleur de remplissage)
      plwd = 2,                  # Épaisseur des lignes
      plty = 1,                  # Type de ligne
      
      # --- Style de la grille ---
      cglcol = "grey",           # Couleur de la grille
      cglty = 1,                 # Type de ligne de la grille
      axislabcol = "grey",       # Couleur des étiquettes d'axe
      cglwd = 0.8,               # Épaisseur de la grille
      
      # --- Style des étiquettes (1P, 2P...) ---
      vlcex = 1.2                # Taille de la police
    )
    
    # 7. Ajouter la légende
    legend(
      x = 1.0, # Ajuster la position
      y = 1.2,
      legend = zone_names,
      bty = "n", 
      pch = 20, 
      col = colors_border, 
      cex = 1.2,
      pt.cex = 2
    )
  })
  
  # Sortie: Graphique 2 (ggplot Araignée)
  output$spiderPlot_ggplot <- renderPlot({
    
    plot_data <- radarData()
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
    }
    
    active_filters <- paste("Filtres actifs: Type =", input$typeFilter, "| Epoque =", input$epoqueFilter)
    
    ggplot(plot_data, aes(x = Pieces, y = Loyer_m2_Mediane, group = Zone, color = Zone)) +
      geom_polygon(aes(fill = Zone), alpha = 0.2, size = 1.2) + # ggplot gère alpha directement
      geom_point(size = 3) +
      coord_polar(start = 0) +
      ylim(0, NA) + # L'échelle commence à 0
      labs(title = "Loyer Médian (€/m²) par Zone et Nombre de Pièces", subtitle = active_filters,
           x = "", y = "Loyer Médian / m² (€)", color = "Zone", fill = "Zone") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold"), axis.text.x = element_text(face = "bold"))
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