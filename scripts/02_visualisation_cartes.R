################################################################################
# SCRIPT 02: VISUALISATION DES CARTES
#
# Ce script génère les cartes de la nouvelle subdivision administrative.
#
# Entrées:
# - data/processed/BFA_subdivision_2025/ : Shapefiles générés par le script 01
#
# Sorties:
# - outputs/cartes/ : Cartes au format PNG
################################################################################

# 1. Chargement des bibliothèques
suppressPackageStartupMessages({
    library(sf)
    library(ggplot2)
    library(dplyr)
    library(ggrepel)
    library(readr)
})

# Définir le répertoire de base (si lancé depuis le dossier scripts)
if (basename(getwd()) == "scripts") {
    setwd("..")
}

# 2. Chargement des données
cat("Chargement des shapefiles traités...\n")

shp_dir <- "data/processed/BFA_subdivision_2025"
if (!dir.exists(shp_dir)) stop("Dossier de données introuvable: ", shp_dir)

communes_new <- st_read(file.path(shp_dir, "BFA_niveau3_communes_2025.shp"), quiet = TRUE)
provinces_new <- st_read(file.path(shp_dir, "BFA_niveau2_provinces_2025.shp"), quiet = TRUE)
regions_new <- st_read(file.path(shp_dir, "BFA_niveau1_regions_2025.shp"), quiet = TRUE)

# 3. Configuration de la sortie
output_dir <- "outputs/cartes"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 4. Thème graphique
theme_carte <- function() {
    theme_minimal() +
        theme(
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 12, hjust = 0.5),
            legend.position = "none"
        )
}

# ==============================================================================
# CARTE 1: RÉGIONALE
# ==============================================================================
cat("📍 Génération de la carte Régionale...\n")

p1 <- ggplot() +
    geom_sf(data = regions_new, aes(fill = nvll_rg), color = "white", size = 1.2) +
    geom_sf_text(
        data = regions_new, aes(label = nvll_rg),
        size = 3.5, fontface = "bold", color = "black", check_overlap = TRUE
    ) +
    scale_fill_viridis_d(option = "turbo") +
    labs(
        title = "Carte Régionale du Burkina Faso",
        subtitle = "Nouvelle Subdivision (17 Régions) - 2025"
    ) +
    theme_carte()

ggsave(file.path(output_dir, "1_Carte_Regionale_2025.png"),
    plot = p1, width = 14, height = 12, dpi = 300
)

# ==============================================================================
# CARTE 2: PROVINCIALE
# ==============================================================================
cat("📍 Génération de la carte Provinciale...\n")

p2 <- ggplot() +
    geom_sf(
        data = provinces_new, aes(fill = nvll_rg),
        color = "white", size = 0.6
    ) +
    geom_sf_text(
        data = provinces_new, aes(label = nvll_pr),
        size = 2.8, fontface = "bold", color = "black",
        check_overlap = TRUE
    ) +
    scale_fill_viridis_d(option = "plasma") +
    labs(
        title = "Carte Provinciale du Burkina Faso",
        subtitle = "Nouvelle Subdivision (47 Provinces) - 2025"
    ) +
    theme_carte()

ggsave(file.path(output_dir, "2_Carte_Provinciale_2025.png"),
    plot = p2, width = 16, height = 14, dpi = 300
)

# ==============================================================================
# CARTE 3: PROVINCIALE (ÉTIQUETTES OPTIMISÉES)
# ==============================================================================
cat("📍 Génération de la carte Provinciale avec étiquettes optimisées...\n")

centroides_provinces <- st_centroid(provinces_new)

p3 <- ggplot() +
    geom_sf(
        data = provinces_new, aes(fill = nvll_rg),
        color = "white", size = 0.6
    ) +
    geom_text_repel(
        data = centroides_provinces,
        aes(label = nvll_pr, geometry = geometry),
        stat = "sf_coordinates",
        size = 2.5,
        fontface = "bold",
        color = "black",
        bg.color = "white",
        bg.r = 0.15,
        max.overlaps = 30,
        force = 2,
        box.padding = 0.3
    ) +
    scale_fill_viridis_d(option = "plasma", alpha = 0.7) +
    labs(
        title = "Carte Provinciale du Burkina Faso",
        subtitle = "47 Provinces (avec étiquettes optimisées) - 2025"
    ) +
    theme_carte()

ggsave(file.path(output_dir, "3_Carte_Provinciale_Etiquettes_2025.png"),
    plot = p3, width = 16, height = 14, dpi = 300
)

# ==============================================================================
# CARTE 4: HIÉRARCHIE (PROVINCES DANS RÉGIONS)
# ==============================================================================
cat("📍 Génération de la carte Hiérarchie...\n")

p4 <- ggplot() +
    # Fond : Régions colorées
    geom_sf(
        data = regions_new, aes(fill = nvll_rg),
        alpha = 0.4, color = NA
    ) +
    # Contours : Provinces
    geom_sf(
        data = provinces_new, fill = NA, color = "black",
        size = 0.5, linetype = "solid"
    ) +
    # Contours épais : Régions
    geom_sf(data = regions_new, fill = NA, color = "black", size = 1.5) +
    # Étiquettes : Provinces
    geom_text_repel(
        data = st_centroid(provinces_new),
        aes(label = nvll_pr, geometry = geometry),
        stat = "sf_coordinates",
        size = 2.3,
        color = "black",
        fontface = "italic",
        bg.color = "white",
        bg.r = 0.1,
        max.overlaps = 35
    ) +
    scale_fill_viridis_d(option = "turbo", name = "Région") +
    labs(
        title = "Délimitation des Provinces par Région",
        subtitle = "Vue hiérarchique : Régions (couleurs + contours épais) et Provinces (contours fins)"
    ) +
    theme_carte() +
    theme(legend.position = "right")

ggsave(file.path(output_dir, "4_Carte_Hierarchie_Regions_Provinces.png"),
    plot = p4, width = 18, height = 15, dpi = 300
)

# ==============================================================================
# CARTE 5: COMMUNALE
# ==============================================================================
cat("📍 Génération de la carte Communale...\n")

p5 <- ggplot() +
    geom_sf(
        data = communes_new, aes(fill = nvll_rg),
        color = "white", size = 0.05
    ) +
    geom_sf(
        data = provinces_new, fill = NA, color = "black",
        size = 0.4, linetype = "solid"
    ) +
    scale_fill_viridis_d(option = "viridis", alpha = 0.6) +
    labs(
        title = "Carte Communale du Burkina Faso",
        subtitle = "Découpage communal avec limites provinciales"
    ) +
    theme_carte() +
    theme(legend.position = "right")

ggsave(file.path(output_dir, "5_Carte_Communale_2025.png"),
    plot = p5, width = 18, height = 15, dpi = 300
)

cat("\n================================================================================\n")
cat("✓ CARTES GÉNÉRÉES AVEC SUCCÈS DANS 'outputs/cartes'\n")
cat("================================================================================\n")
