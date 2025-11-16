# Fonction pour créer les listes HTML
buildRankHTML <- function(data_frame, unit_label, background_color) {
  if (nrow(data_frame) == 0) {
    return(p("Aucune donnée à afficher."))
  }
  
  # Construire les éléments de la liste <li>
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


# ==================================================================
#6. Définir le fonctionnement du Serveur 
# ==================================================================
server <- function(input, output, session) {
  
  # Données Filtrées (LA BASE) 

  filteredData <- reactive({
    # Data provient de global.R
    req(data) # Vérifie que 'data' est chargé
    df <- data
    
    # Le filtre de zone est requis (pas de "Tous")
    req(input$zoneFilter) 
    df <- df |> filter(Zone %in% input$zoneFilter)
    
    # Appliquer le filtre "Type" s'il est different de "Tous"
    if (input$typeFilter != "Tous") {
      df <- df |> filter(Type == input$typeFilter)
    }
    
    # Appliquer le filtre "Pièces" (pour "1-3P")
    if (input$piecesFilter == "1-3P") {
      df <- df |> filter(Pieces %in% c("1P", "2P", "3P"))
    } else if (input$piecesFilter != "Tous") {
      df <- df |> filter(Pieces == input$piecesFilter)
    }
    
    # Appliquer le filtre "Epoque" s'il est différent de "Tous"
    if (input$epoqueFilter != "Tous") {
      df <- df |> filter(Epoque_Construction == input$epoqueFilter)
    }
    
    # Renvoi le dataframe filtré
    return(df)
  })
  
  #SORTIE: Statistiques Résumées (Onglet 2) 
  output$summaryStats <- renderUI({
    # Dépend des données filtrées et du choix de métrique (m²/total)
    req(filteredData(), input$statsMetric) 
    
    df_plot <- filteredData() # Récupérer les données filtrées
    
    #1. Calcul spécial du nombre de contrats
    df_for_contracts <- data #data provient de global.R
    df_for_contracts <- df_for_contracts |> filter(Zone %in% input$zoneFilter)
    
    if (input$typeFilter == "Tous") {
      df_for_contracts <- df_for_contracts |> filter(Type == "ensemble")
    } else {
      df_for_contracts <- df_for_contracts |> filter(Type == input$typeFilter)
    }
    
    if (input$epoqueFilter == "Tous") {
      df_for_contracts <- df_for_contracts |> filter(Epoque_Construction == "(Non spécifié)")
    } else {
      df_for_contracts <- df_for_contracts |> filter(Epoque_Construction == input$epoqueFilter)
    }
    
    if (input$piecesFilter == "Tous") {
      df_for_contracts <- df_for_contracts |> filter(Pieces == "(Non spécifié)")
    } else if (input$piecesFilter == "1-3P") {
      df_for_contracts <- df_for_contracts |> filter(Pieces %in% c("1P", "2P", "3P"))
    } else {
      df_for_contracts <- df_for_contracts |> filter(Pieces == input$piecesFilter)
    }
    total_contracts <- sum(df_for_contracts$obs, na.rm = TRUE)
    
    
    #2. Définition des variables dynamiques
    metric_col_sym <- if (input$statsMetric == "m2") sym("Loyer_m2_Mediane") else sym("Loyer_Total_Mediane")
    unit_label <- if (input$statsMetric == "m2") "€/m²" else "€"
    title_label <- if (input$statsMetric == "m2") paste0("Loyer Moyen par Zone (", unit_label, ")") else paste0("Loyer Total Moyen par Zone (", unit_label, ")")
    
    #3. Calculs statistiques dynamiques
    df_stats <- df_plot |> filter(!is.na(!!metric_col_sym)) 
    
    if (nrow(df_stats) == 0) {
      return(HTML(paste0(
        "<div style='padding: 20px;'>",
        "<h3 style='color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;'>Statistiques Générales</h3>",
        "<div style='background: #ecf0f1; padding: 15px; border-radius: 5px; margin-bottom: 20px;'>",
        "<p><strong>Nombre total de contrats de locations (base):</strong> ", format(total_contracts, big.mark = " "), "</p>",
        "</div>",
        "<p>Aucune donnée statistique à afficher pour cette sélection (", unit_label, ").</p>",
        "</div>"
      )))
    }
    
    stats_by_zone <- df_stats |> 
      group_by(Zone) |> 
      summarise(
        Loyer_Moyen = round(mean(!!metric_col_sym, na.rm = TRUE), 2),
        Loyer_Min = round(min(!!metric_col_sym, na.rm = TRUE), 2),
        Loyer_Max = round(max(!!metric_col_sym, na.rm = TRUE), 2),
        N_Observations = n(),
        .groups = 'drop'
      ) |> 
      arrange(desc(Loyer_Moyen))
    
    total_combinations <- nrow(df_plot)
    complete_combinations <- nrow(df_stats)
    completeness <- if (total_combinations > 0) round(complete_combinations / total_combinations * 100, 1) else 0
    
    #4. Génération HTML (simplifiée)
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
  
  #SORTIES: Classements (Onglet 3)
  
  #Réactif 2: Données pour le classement
  rankData <- reactive({
    req(filteredData(), input$rankMetric)
    metric_col_sym <- if (input$rankMetric == "m2") sym("Loyer_m2_Mediane") else sym("Loyer_Total_Mediane")
    
    df <- filteredData() |> 
      filter(!is.na(!!metric_col_sym)) |> 
      mutate(Config = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    list(df = df, metric_col_sym = metric_col_sym)
  })
  
  output$rankExpensiveTitle <- renderText({
    paste("Top", 10, "- Configurations les Plus Chères")
  })
  
  output$rankCheapTitle <- renderText({
    paste("Top", 10, "- Configurations les Moins Chères")
  })
  
  #Sortie pour la liste des "Plus Chers"
  output$topExpensiveUI <- renderUI({
    req(rankData(), 10)
    rank_data_list <- rankData()
    unit_label <- if (input$rankMetric == "m2") "€/m²" else "€"
    
    top_expensive <- rank_data_list$df |> 
      arrange(desc(!!rank_data_list$metric_col_sym)) |> 
      head(10) |> 
      mutate(Value = !!rank_data_list$metric_col_sym) 
    
    buildRankHTML(top_expensive, unit_label, "#fadbd8")
  })
  
  # Sortie pour la liste des "Moins Chers"
  output$topCheapUI <- renderUI({
    req(rankData(), 10)
    rank_data_list <- rankData()
    unit_label <- if (input$rankMetric == "m2") "€/m²" else "€"
    
    top_cheap <- rank_data_list$df |> 
      arrange(!!rank_data_list$metric_col_sym) |> 
      head(10) |> 
      mutate(Value = !!rank_data_list$metric_col_sym) 
    
    buildRankHTML(top_cheap, unit_label, "#d5f4e6")
  })
  
  
  # SORTIES: Graphiques (Onglets 4 & 5)
  
  # GRAPHIQUE 1: Distribution (PointRange)
  output$distribPlot <- renderPlot({
    req(filteredData(), input$distribMetric)
    plot_data_base <- filteredData()
    warning_text <- NULL
    
    # --- 1. Définir les variables dynamiques ---
    # On définit les noms de colonnes et les libellés en fonction de l'input
    if (input$distribMetric == "m2") {
      med_col_sym <- sym("Loyer_m2_Mediane")
      q1_col_sym  <- sym("Loyer_m2_Q1")
      q3_col_sym  <- sym("Loyer_m2_Q3")
      title_lab   <- "Distribution des Loyers au m² (Médiane, Q1, Q3)"
      y_lab       <- "Loyer / m² (€)"
    } else {
      med_col_sym <- sym("Loyer_Total_Mediane")
      q1_col_sym  <- sym("Loyer_Total_Q1")
      q3_col_sym  <- sym("Loyer_Total_Q3")
      title_lab   <- "Distribution des Loyers Totaux (Médiane, Q1, Q3)"
      y_lab       <- "Loyer Total Mensuel (€)"
    }
    
    # --- 2. Logique de filtrage (commune) ---
    # On utilise !! (bang-bang) pour que R utilise la *valeur* de nos variables (ex: Loyer_m2_Mediane)
    plot_data <- plot_data_base |> 
      filter(!is.na(!!med_col_sym) & !is.na(!!q1_col_sym) & !is.na(!!q3_col_sym))
    
    if (nrow(plot_data) == 0) {
      return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
    }
    
    plot_data <- plot_data |> 
      mutate(GroupLabel = paste(Zone, Type, Pieces, Epoque_Construction, sep = " / "))
    
    if(nrow(plot_data) > 50) {
      plot_data <- plot_data |> arrange(!!med_col_sym) |> slice(1:50)
      warning_text <- "Note: Affichage des 50 premières lignes (triées). Affinez vos filtres."
    }
    
    # --- 3. Logique de graphique (commune) ---
    ggplot(plot_data, aes(x = reorder(GroupLabel, !!med_col_sym), y = !!med_col_sym, color = Zone)) +
      geom_pointrange(aes(ymin = !!q1_col_sym, ymax = !!q3_col_sym), size = 0.8) +
      coord_flip() +  
      labs(title = title_lab, subtitle = warning_text,
           x = "Catégorie", y = y_lab, color = "Zone") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"), axis.text.y = element_text(size = 9), legend.position = "bottom")
  })
  
  # GRAPHIQUE 2: Dispersion (Barres)
  output$dispersionPlot <- renderPlot({
    req(filteredData(), input$dispersionMetric)
    
    if (input$dispersionMetric == "m2") {
      plot_data <- filteredData() |> 
        filter(!is.na(Loyer_m2_Q1), !is.na(Loyer_m2_Q3), Zone != "(Non spécifié)") |> 
        mutate(EIQ_m2 = Loyer_m2_Q3 - Loyer_m2_Q1) 
      
      plot_summary <- plot_data |> group_by(Zone) |> 
        summarise(Loyer_Med_Moyen = mean(Loyer_m2_Mediane, na.rm = TRUE), EIQ_Moyen = mean(EIQ_m2, na.rm = TRUE), .groups = 'drop') |> 
        filter(!is.na(EIQ_Moyen))
      
      if (nrow(plot_summary) == 0) return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      
      ggplot(plot_summary, aes(x = EIQ_Moyen, y = reorder(Zone, EIQ_Moyen), fill = Loyer_Med_Moyen)) +
        geom_col() + 
        labs(title = "Dispersion des Loyers (Écart InterQuartile au m²)", x = "Écart InterQuartile Moyen au m² (€)", y = "Zone", fill = "Loyer Médian\nMoyen (€/m²)") +
        scale_fill_gradient(low = "#fee8c8", high = "#e34a33") + 
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(face = "bold"))
      
    } else {
      plot_data <- filteredData() |> 
        filter(!is.na(Loyer_Total_Q1), !is.na(Loyer_Total_Q3), Zone != "(Non spécifié)") |> 
        mutate(EIQ_Total = Loyer_Total_Q3 - Loyer_Total_Q1) 
      
      plot_summary <- plot_data |> group_by(Zone) |> 
        summarise(Loyer_Med_Moyen = mean(Loyer_Total_Mediane, na.rm = TRUE), EIQ_Moyen = mean(EIQ_Total, na.rm = TRUE), .groups = 'drop') |> 
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
    # ==========================================================
    # NOUVELLE FENETRE 1: Loyers Moyens par Zone
    # ==========================================================
    output$avgZonePlot <- renderPlot({
      req(filteredData(), input$avgMetric)
      
      # 1. Définir les variables
      if (input$avgMetric == "m2") {
        metric_col_sym <- sym("Loyer_m2_Moyenne")
        title_lab <- "Loyer Moyen au m² (pondéré)"
        x_lab <- "Loyer Moyen / m² (€)"
      } else {
        metric_col_sym <- sym("Loyer_Total_Moyenne")
        title_lab <- "Loyer Moyen Total (pondéré)"
        x_lab <- "Loyer Moyen Total (€)"
      }
      
      # 2. Préparer les données
      plot_data <- filteredData() |> 
        filter(!is.na(!!metric_col_sym) & !is.na(obs))
      
      if (nrow(plot_data) == 0) {
        return(ggplot() + geom_text(aes(x = 0, y = 0, label = "Aucune donnée à afficher."), size = 6) + theme_void())
      }
      
      # Agréger par Zone en utilisant une moyenne pondérée
      plot_summary <- plot_data |> 
        group_by(Zone) |> 
        summarise(Loyer_Agg = weighted.mean(!!metric_col_sym, w = obs, na.rm = TRUE),
                  .groups = 'drop') |> 
        filter(!is.na(Loyer_Agg))
      
      # 3. Graphique
      ggplot(plot_summary, aes(x = Loyer_Agg, y = reorder(Zone, Loyer_Agg))) +
        geom_col(aes(fill = Zone), alpha = 0.8) +
        geom_text(aes(label = round(Loyer_Agg, 1)), hjust = -0.2, size = 4) +
        labs(title = title_lab,
             subtitle = "Moyenne des catégories filtrées, pondérée par le nb. d'observations",
             x = x_lab, y = "Zone") +
        scale_fill_viridis_d() + 
        theme_minimal(base_size = 14) +
        # MODIFIÉ: Supprime la légende de couleur car les zones sont sur l'axe Y
        theme(plot.title = element_text(face = "bold"),
              legend.position = "none")
    })
  
  
  # SORTIES: Simulateur (Onglet 6)
  
  #1. Générer le Slider dynamique
  output$sliderUI <- renderUI({
    req(filteredData(), input$simMetric)
    df <- filteredData()
    
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
                width = "100%")
  })
  
  # 2. Générer la table de résultats du simulateur
  output$simResultsTable <- DT::renderDataTable({
    req(filteredData(), input$simSlider, input$simMetric)
    
    df <- filteredData()
    slider_val <- input$simSlider 
    
    if (input$simMetric == "m2") {
      results <- df |> 
        filter(Loyer_m2_Mediane <= slider_val) |> 
        arrange(desc(Loyer_m2_Mediane)) |> 
        select(Zone, Type, Pieces, Epoque_Construction, Loyer_m2_Mediane, Loyer_Total_Mediane, obs)
      cols_to_format <- c('Loyer_m2_Mediane', 'Loyer_Total_Mediane')
      
    } else {
      results <- df |> 
        filter(Loyer_Total_Mediane <= slider_val) |> 
        arrange(desc(Loyer_Total_Mediane)) |> 
        select(Zone, Type, Pieces,  Loyer_Total_Mediane , obs, Epoque_Construction, Loyer_m2_Mediane)
      cols_to_format <- c('Loyer_Total_Mediane', 'Loyer_m2_Mediane')
    }
    
    DT::datatable(
      results,
      options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE,searching = FALSE),
      rownames = FALSE, filter = 'none', class = 'stripe hover'
    ) |> 
      DT::formatRound(columns = cols_to_format, digits = 2) |> 
      DT::formatRound(columns = 'obs', digits = 0)
    
  })
  
  # SORTIE: Graphique nombre de pièce par zone d'habitation
  output$piecesPlot <- renderPlot({
    df <- filteredData()
    if (is.null(df) || nrow(df) == 0) {
      return(ggplot() + 
               geom_text(aes(x = 0.5, y = 0.5, label = "Aucune donnée disponible"), size = 6) +
               theme_void())
    }
    plot_df <- df |>
      group_by(Zone, Pieces) |>
      summarise(N = sum(obs, na.rm = TRUE), .groups = "drop")
    ggplot(plot_df, aes(x = Zone, y = N, fill = Pieces)) +
      geom_col(position = "fill") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        title = "Répartition du nombre de pièces par zone",
        x = "Zone",
        y = "Proportion de logements",
        fill = "Nombre de pièces"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  
  
  
}