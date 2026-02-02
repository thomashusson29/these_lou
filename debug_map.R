# Script de débogage pour la normalisation des noms de villes
options(warn = -1)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(stringr)

# Recréer les objets de données comme dans le .qmd
data_raw <- readr::read_delim(
  "Q1_Q2_apparie.csv",
  delim = ";",
  locale = locale(encoding = "UTF-8"),
  show_col_types = FALSE,
  trim_ws = TRUE
)
data <- data_raw %>%
  mutate(
    has_match = !is.na(Q2_row) & Q2_row != "",
  )

geojson_url <- "https://geo.api.gouv.fr/communes?codeDepartement=974&format=geojson"
reunion_map <- tryCatch(sf::read_sf(geojson_url), error = function(e) NULL)

# Logique de normalisation extraite du .qmd
if (!is.null(reunion_map)) {
  city_counts <- data %>%
    filter(!is.na(`Q1__Dans quelle ville habitez-vous ?`)) %>%
    count(`Q1__Dans quelle ville habitez-vous ?`, name = "n_participants")
  
  normalize_city_name <- function(name) {
    name %>%
      tolower() %>%
      iconv(to = "ASCII//TRANSLIT") %>%
      str_replace_all("saint", "st") %>%
      str_replace_all("[^a-z]", "")
  }
  
  city_counts <- city_counts %>%
    mutate(city_norm = normalize_city_name(`Q1__Dans quelle ville habitez-vous ?`))
  
  reunion_map <- reunion_map %>%
    mutate(city_norm = normalize_city_name(nom))
  
  # --- DÉBOGAGE ---
  cat("--- Clés de jointure (données des répondantes) ---
")
  print(sort(unique(city_counts$city_norm)))
  cat("\n--- Clés de jointure (fond de carte) ---
")
  print(sort(unique(reunion_map$city_norm)))
  
  matches <- intersect(city_counts$city_norm, reunion_map$city_norm)
  cat("\n--- Correspondances trouvées ---
")
  print(matches)

} else {
  cat("Téléchargement du fond de carte échoué.\n")
}
