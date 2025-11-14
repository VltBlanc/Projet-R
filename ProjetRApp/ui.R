fluidPage(
  titlePanel("Analyse des Loyers - Paris 2024"),
  # Disposition avec une barre latérale et un panneau principal
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtres de sélection"),
      # les varaibles all viennent de global
      # Filtre pour la zone (obligatoire)
      selectInput("zoneFilter", "Périmètre (Zone):",  
                  choices = zone_choices,
                  selected = "zone 1",
                  multiple = TRUE),
      # Filtre pour le type de logement
      selectInput("typeFilter", "Type de logement:",  
                  choices = all_types,
                  selected = "Tous"),
      # Filtre pour le nombre de pièces
      selectInput("piecesFilter", "Nombre de pièces:",  
                  choices = all_pieces,
                  selected = "Tous"),
      # Filtre pour l'époque de construction
      selectInput("epoqueFilter", "Époque de construction:",  
                  choices = all_epoque,
                  selected = "Tous")
    ),
    
    # Panneau principal 
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
                                     <li><strong>Loyer au m² :</strong> (Colonnes Q1, Médiane, Q3) Représente le loyer mensuel médian par mètre carré. C'est un indicateur clé du prix du marché immobilier.</li>
                                     <li><strong>Loyer Total :</strong> (Colonnes Q1.1, Médiane.1, Q3.1) Représente le loyer mensuel total (le loyer 'nu') payé par le locataire.</li>
                                   </ul>"),
                          p(strong("Important :"), "Ces loyers sont mesurés", strong("hors charges"), "(eau, chauffage collectif, entretien...). Le budget mensuel réel du locataire sera donc supérieur (Loyer Total + Provisions pour charges)."),
                          
                          br(),
                          
                          h3("Composition des 7 Zones (Source: OLAP)"),
                          p("Le zonage reggroupe des communes/arrondissements de l'agglomération parisienne ayant des niveaux de loyers privés similaires."),
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
                 # Bouton pour choisir m² ou Total
                 radioButtons("statsMetric", "Choisir:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 br(),
                 uiOutput("summaryStats")
        ),
        
        # Onglet 3: Classements
        tabPanel("Classements",
                 br(),
                 fluidRow(
                   column(5,
                          radioButtons("rankMetric", "Classer selon:",
                                       choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                                       selected = "m2", inline = TRUE)
                   ),
                 ),
                 br(),
                 fluidRow(
                   # Colonne pour les plus chers
                   column(6,
                          h4(textOutput("rankExpensiveTitle", inline = TRUE)),
                          uiOutput("topExpensiveUI") # Interface générée dans le serveur (inclut la bannière)
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
                 # Ajout de la bannière dans l'interface utilisateur pour être au-dessus du graphique
                 uiOutput("simulatorBannerDistrib"), # Bannière des contrats
                 radioButtons("distribMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 br(),
                 plotOutput("distribPlot", height = "600px")
        ),
        
        # Onglet 5: Dispersion EIQ (Graphique)
        tabPanel("Dispersion EIQ",
                 br(),
                 # Ajout de la bannière dans l'interface utilisateur pour être au-dessus du graphique
                 uiOutput("simulatorBannerDispersion"), # Bannière des contrats
                 radioButtons("dispersionMetric", "Choisir la métrique:",
                              choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                              selected = "m2", inline = TRUE),
                 br(),
                 plotOutput("dispersionPlot", height = "600px")
        ),
        
        # Onglet 6: Simulateur de Loyer
        tabPanel("Simulateur de Loyer",
                 br(),
                 uiOutput("simulatorBanner"), # Ajout de la bannière des contrats
                 fluidRow(
                   column(4,
                          h4("Paramètres"),
                          radioButtons("simMetric", "Simuler sur:",
                                       choices = c("Loyer / m²" = "m2", "Loyer Total" = "total"),
                                       selected = "total", inline = TRUE),
                          br(),
                          uiOutput("sliderUI"), # Slider dynamique généré par le serveur
                          br(),
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
                 uiOutput("rentTableBanner"), # Ajout de la bannière des contrats
                 DT::dataTableOutput("rentTable")
        )
      ) # fin tabsetPanel
    ) # fin mainPanel
  ) # fin sidebarLayout
) # fin fluidPage