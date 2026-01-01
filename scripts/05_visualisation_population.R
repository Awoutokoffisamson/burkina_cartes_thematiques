################################################################################
# SCRIPT 05: VISUALISATION POPULATION ET DENSITÉ

################################################################################

# 1. Chargement des bibliothèques
suppressPackageStartupMessages({
    library(sf)
    library(dplyr)
    library(readxl)
    library(ggplot2)
    library(ggrepel)
    library(scales)
    library(stringi)
    library(ggspatial) # Pour la flèche du nord et l'échelle
})

# Définir le répertoire de base
if (basename(getwd()) == "scripts") {
    setwd("..")
}


# 2. Chargement des données
shp_file <- "data/processed/BFA_subdivision_2025/BFA_niveau3_communes_2025.shp"
excel_file <- "outputs/rapports/BFA_communes_population_superficie_2025.xlsx"

if (!file.exists(shp_file)) stop("Shapefile introuvable")
if (!file.exists(excel_file)) stop("Fichier Excel introuvable")

cat("Chargement du shapefile et des données Excel...\n")
communes_shp <- st_read(shp_file, quiet = TRUE)
# Calcul de la superficie dans le shapefile si elle n'existe pas
if (!"Superficie_km2" %in% names(communes_shp)) {
    communes_shp$Superficie_km2 <- as.numeric(st_area(communes_shp)) / 10^6
}

communes_data <- read_excel(excel_file)

# 3. Nettoyage pour jointure robuste (Commune + Province)
clean_names <- function(x) {
    x <- stri_trans_general(x, "Latin-ASCII")
    x <- tolower(x)
    x <- gsub("-", " ", x)
    x <- gsub("'", "", x)
    x <- trimws(x)
    return(x)
}

communes_shp <- communes_shp %>%
    mutate(
        commune_clean = clean_names(NAME_3),
        province_clean = clean_names(nvll_pr)
    )

communes_data <- communes_data %>%
    mutate(
        commune_clean = clean_names(Commune),
        province_clean = clean_names(Province)
    )

# Jointure sur Commune ET Province pour éviter les doublons (ex: Boussouma)
cat("Jointure des données (Géométrie + Population)...\n")
communes_full <- communes_shp %>%
    left_join(communes_data, by = c("commune_clean", "province_clean"))

# Vérification des ratés
missing <- communes_full %>% filter(is.na(Population_2019))
if (nrow(missing) > 0) {
    cat(" ATTENTION :", nrow(missing), "communes n'ont pas matché (Géométrie sans Data) !\n")
    print(missing$NAME_3)
} else {
    cat("Jointure parfaite (Toutes les géométries ont des données).\n")
}

# Gestion des colonnes dupliquées (Superficie_km2.x vs .y)
# On prend .x (shapefile) par défaut, ou Superficie_km2 si pas de duplication
if ("Superficie_km2.x" %in% names(communes_full)) {
    communes_full$Superficie_Use <- communes_full$Superficie_km2.x
} else if ("Superficie_km2" %in% names(communes_full)) {
    communes_full$Superficie_Use <- communes_full$Superficie_km2
} else {
    stop("Colonne Superficie introuvable après jointure")
}

# ==============================================================================
# CARTE 1 : RÉGIONS (POPULATION)
# ==============================================================================
cat("\nCréation de la carte des Régions (Population)...\n")

# Agrégation par Région (On utilise nvll_rg du Shapefile pour la géométrie)
regions_agg <- communes_full %>%
    group_by(nvll_rg) %>%
    summarise(
        Population = sum(Population_2019, na.rm = TRUE),
        geometry = st_union(geometry)
    ) %>%
    mutate(
        Label = paste0(nvll_rg, "\n", format(Population, big.mark = " "))
    )

# Carte
p1 <- ggplot(data = regions_agg) +
    geom_sf(aes(fill = Population), color = "white", size = 0.5) +
    # Utilisation de geom_label_repel pour le "joli cadre"
    geom_label_repel(
        aes(label = Label, geometry = geometry),
        stat = "sf_coordinates",
        size = 3,
        fontface = "bold",
        color = "black",
        fill = alpha("white", 0.8), # Fond blanc légèrement transparent
        box.padding = 0.5,
        segment.color = "grey50"
    ) +
    # Changement de palette pour YlOrRd (Direction 1 pour clair -> foncé)
    scale_fill_distiller(palette = "YlOrRd", direction = 1, name = "Population (2019)", labels = comma) +
    # Flèche du Nord et Échelle
    annotation_north_arrow(
        location = "tl", which_north = "true",
        pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_minimal
    ) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    labs(
        title = "Population par Région (Burkina Faso, 2019)",
        subtitle = "Découpage administratif 2025",
        caption = "Source: RGPH 2019 (INSD) / Traitement CityPopulation\nPar AWOUTO K. Samson, élève ingénieur statisticien économiste"
    ) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 1, size = 10, face = "italic"),
        legend.position = "right"
    )

ggsave("outputs/cartes/BFA_Regions_Population_2019.png", plot = p1, width = 12, height = 10, bg = "white")

# ==============================================================================
# CARTE 2 : PROVINCES (DENSITÉ)
# ==============================================================================

# Agrégation par Province (On utilise nvll_pr du Shapefile)
provinces_agg <- communes_full %>%
    group_by(nvll_pr) %>%
    summarise(
        Population = sum(Population_2019, na.rm = TRUE),
        Superficie = sum(Superficie_Use, na.rm = TRUE),
        geometry = st_union(geometry)
    ) %>%
    mutate(
        Densite = Population / Superficie
    )

# Création des intervalles (classes) pour le contraste
breaks <- c(0, 25, 50, 75, 100, 150, 300, Inf)
labels <- c("< 25", "25 - 50", "50 - 75", "75 - 100", "100 - 150", "150 - 300", "> 300")

provinces_agg$Densite_Class <- cut(
    provinces_agg$Densite,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE
)

# Carte
p2 <- ggplot(data = provinces_agg) +
    geom_sf(aes(fill = Densite_Class), color = "white", size = 0.2) +
    # Ajout des contours de région pour le contexte
    geom_sf(data = regions_agg, fill = NA, color = "black", size = 0.8) +
    # Ajout des étiquettes de PROVINCES (avec cadre)
    geom_label_repel(
        aes(label = nvll_pr, geometry = geometry),
        stat = "sf_coordinates",
        size = 2.5,
        fontface = "italic",
        color = "black",
        fill = alpha("white", 0.7), # Fond blanc semi-transparent
        box.padding = 0.3,
        max.overlaps = 30
    ) +
    scale_fill_brewer(
        palette = "YlOrRd",
        name = "Densité (hab/km²)"
    ) +
    # Flèche du Nord et Échelle
    annotation_north_arrow(
        location = "tl", which_north = "true",
        pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
        style = north_arrow_minimal
    ) +
    annotation_scale(location = "bl", width_hint = 0.3) +
    labs(
        title = "Densité de Population par Province (2019)",
        subtitle = "Contraste de densité (Habitants par km²)",
        caption = "Source: RGPH 2019 (INSD)\nPar AWOUTO K. Samson, élève ingénieur statisticien économiste"
    ) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.caption = element_text(hjust = 1, size = 10, face = "italic"),
        legend.position = "right",
        legend.title = element_text(face = "bold")
    )

ggsave("outputs/cartes/BFA_Provinces_Densite_2019.png", plot = p2, width = 12, height = 10, bg = "white")
