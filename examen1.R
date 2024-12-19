#Écrire une fonction dans R qui lit les 60 fichiers csv avec les données de ventes
#du FXD JET 100 entre 2020-01-01 et 2024-12-31. Assurez vous que cette fonction
#peut être réutilisée pour lire les fichiers csv en utilisant des dates différentes que
#celles spécifiées ci-dessus comme 2020-01-01 et 2022-12-31 ou similaire.


# Chargement des bibliothèques nécessaires
library(dplyr)
library(readr)

read_sales_data <- function(directory, date_debut, date_fin) {
  # Vérifier les dates
  date_debut <- as.Date(date_debut)
  date_fin <- as.Date(date_fin)
  
  if (is.na(date_debut) | is.na(date_fin)) {
    stop("Les dates doivent être au format YYYY-MM-DD.")
  }
  
  if (date_debut > date_fin) {
    stop("La date de début doit être antérieure ou égale à la date de fin.")
  }
  
  
  # Obtenir la liste des fichiers CSV dans le répertoire
  dossier <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
  
  
  # Lire et combiner les fichiers
  combined_data <- dossier %>%
    lapply(read_csv) %>%   # Lire chaque fichier CSV
    bind_rows()            # Combiner toutes les données
  
  # Filtrer les données par les dates spécifiées
  filtered_data <- combined_data %>%
    mutate(date = as.Date(date)) %>%  # Convertir la colonne `date` au format Date
    filter(date >= date_debut & date <= date_fin) # Filtrer par les dates
  
  return(filtered_data)
}


directory_path <- ("//home//UCA/jengoma//Téléchargements//exam_data")

date_debut <- "2020-01-01"
date_fin <- "2024-12-31"

sales_data <- read_sales_data(directory_path, date_debut ,date_fin )

# Afficher un aperçu des données
head(sales_data)


library(ggplot2)

# Filtrer les données pour la plage de dates
filtered_data <- combined_data %>%
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2024-12-31"))


# Créer le graphique
sales_plot <- ggplot(filtered_data, aes(x = date, y = sales)) +
  geom_line(color = "blue", size = 1) +  # Courbe des ventes
  geom_point(color = "darkblue", size = 1) +  # Points pour chaque observation
  labs(
    title = "Évolution des ventes de FXD JET 100 (2020-2024)",
    subtitle = "Données entre le 1er janvier 2020 et le 31 décembre 2024",
    x = "Date",
    y = "Ventes (unités ou montant)",
    caption = "Source : Données internes"
  ) +
  theme_minimal(base_size = 12) +  # Thème épuré
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centrer le titre
    plot.subtitle = element_text(hjust = 0.5, size = 12),  # Centrer le sous-titre
    axis.text.x = element_text(angle = 45, hjust = 1)  # Incliner les labels des dates
  )

# Afficher le graphique
print(sales_plot)
