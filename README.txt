Voici le Projet final de R
Le projet est composé de :
  le raport en Markdown
  l'app dans un dossier à part
  la base de donnée trouvée sur le site de l'observatoir des loyes de l'aglomération parisienne (OLAP) https://www.observatoire-des-loyers.fr/

//Tuto pour l'application//
Tous les fichiers se trouvent dans le dossier "ProjetRApp", vous y trouverez 3 fichiers : 
    - un global.R pour définir des variables globales utiles pour la logique du serveur ainsi que quelques modifications afin de rendre le dataset plus utilisable (noms de colonnes, cellules vide, etc...)
    - un server.R pour la logique du serveur
    - un ui.R pour l'interface graphique des fenêtres de l'application
on a utilisé les librairies : shiny, ggplot2, dplyr et DT (pour faire des tableaux intéractifs)
le dataset est à la fois dans le dossier principla et dans le dossier ProjetRApp et se nomme : "Paris2024-annexe-statistique-diffusion.csv"



Informations pour la base de données : 

Le zonage reggroupe des communes/arrondissements de l'agglomération parisienne ayant des niveaux de loyers privés similaires.

Zone 1 : Les secteurs les plus chers. Comprend les Paris 6e, 7e, 8e, 16e et Neuilly-sur-Seine.

Zone 2 : Le reste de Paris intra-muros (sauf 13e, 19e, 20e) et quelques communes très proches et chères. Comprend les Paris 1er, 2e, 3e, 4e, 5e, 9e, 10e, 11e, 12e, 14e, 15e, 17e, 18e, ainsi que Boulogne-Billancourt, Levallois-Perret, Saint-Mandé et Vincennes.

Zone 3 : Les arrondissements de Paris restants. Comprend les Paris 13e, 19e et 20e.

Zone 4 : Un grand nombre de communes de la petite couronne et de la grande couronne proche (ex: Versailles, Asnières-sur-Seine, Clichy, Saint-Maur-des-Fossés, Rueil-Malmaison...).

Zone 5 : Des communes de la grande couronne, un peu plus éloignées (ex: Melun, Champs-sur-Marne, Achères...).

Zones 6 et 7 : Elles suivent la même logique et regroupent les communes de la grande couronne les plus éloignées ou celles où les loyers sont les plus modérés.
