################################################################################
# SCRIPT 04: FUSION POPULATION ET SUPERFICIE

################################################################################

# 1. Chargement des bibliothèques
suppressPackageStartupMessages({
    library(sf)
    library(dplyr)
    library(readr)
    library(stringi)
    library(writexl)
})

# Définir le répertoire de base
if (basename(getwd()) == "scripts") {
    setwd("..")
}


# 2. Chargement des données

# Shapefile (Nouvelle subdivision)
shp_file <- "data/processed/BFA_subdivision_2025/BFA_niveau3_communes_2025.shp"
if (!file.exists(shp_file)) stop("Shapefile introuvable: ", shp_file)
communes_shp <- st_read(shp_file, quiet = TRUE)
communes_shp$Superficie_km2 <- as.numeric(st_area(communes_shp)) / 10^6

# Population (CityPopulation - Ancienne subdivision)
pop_file <- "data/raw/population_citypop.csv"
if (!file.exists(pop_file)) stop("Fichier population introuvable: ", pop_file)
communes_pop <- read_delim(pop_file, delim = ";", show_col_types = FALSE)

# Correspondance (Pont entre Ancien et Nouveau)
corr_file <- "data/raw/table_correspondance_communes.csv"
if (!file.exists(corr_file)) stop("Fichier correspondance introuvable: ", corr_file)
correspondance <- read_csv(corr_file, show_col_types = FALSE)

cat("Colonnes correspondance : ", paste(names(correspondance), collapse = ", "), "\n")

# 3. Fonction de nettoyage
clean_names <- function(x) {
    x <- stri_trans_general(x, "Latin-ASCII")
    x <- tolower(x)
    x <- gsub("-", " ", x)
    x <- gsub("'", "", x)
    x <- trimws(x)
    return(x)
}

# 4. Préparation des données pour la jointure

# A. CityPop (Population)
# On applique les corrections manuelles d'orthographe ICI
corrections_communes <- c(
    "karangasso sambla" = "karankasso sambla",
    "karangasso vigue" = "karankasso vigue",
    "samoghohiri" = "samoghohiri",
    "imasgo" = "imasgho",
    "kokologo" = "kokologho",
    "coalla" = "koalla",
    "manni" = "mani",
    "arbinda" = "aribinda",
    "gueguere" = "gueguere"
)

communes_pop <- communes_pop %>%
    mutate(
        commune_clean = clean_names(Commune),
        province_old_clean = clean_names(Province)
    )

communes_pop$commune_clean <- ifelse(
    communes_pop$commune_clean %in% names(corrections_communes),
    corrections_communes[communes_pop$commune_clean],
    communes_pop$commune_clean
)

# Correction Spécifique Province (Guéguéré : Ioba -> Bougouriba)
# Pour matcher la table de correspondance qui attend "Bougouriba"
communes_pop$province_old_clean[communes_pop$commune_clean == "gueguere"] <- "bougouriba"

# Correction forcée pour Samogohiri (au cas où le vecteur échoue)
communes_pop$commune_clean[communes_pop$commune_clean == "samoghohiri"] <- "samogohiri"


# B. Correspondance
correspondance <- correspondance %>%
    mutate(
        commune_clean = clean_names(NAME_3),
        province_old_clean = clean_names(NAME_2),
        province_new_clean = clean_names(nouvelle_province)
    )

# Correction des Provinces dans la table de correspondance
# CityPop a "Komondjari", Correspondance a "Komandjoari"
correspondance$province_old_clean <- ifelse(
    correspondance$province_old_clean == "komandjoari",
    "komondjari",
    correspondance$province_old_clean
)

# C. Shapefile
communes_shp <- communes_shp %>%
    mutate(
        commune_clean = clean_names(NAME_3),
        province_new_clean = clean_names(nvll_pr)
    )

# 5. Jointure en cascade

cat("Fusion 1 : Population + Correspondance (via Ancienne Province)...\n")
# On joint la population à la table de correspondance pour récupérer la "Nouvelle Province"
pop_with_new_prov <- communes_pop %>%
    left_join(correspondance %>% select(commune_clean, province_old_clean, province_new_clean),
        by = c("commune_clean", "province_old_clean")
    )

# Vérification intermédiaire
missing_link <- pop_with_new_prov %>% filter(is.na(province_new_clean))
if (nrow(missing_link) > 0) {
    cat(paste("⚠️", nrow(missing_link), "communes de CityPop n'ont pas trouvé de correspondance (orthographe ?).\n"))
    print(missing_link$commune_clean)
}

# Maintenant on peut joindre avec le shapefile via "Nouvelle Province" + "Commune"
merged_data <- communes_shp %>%
    st_drop_geometry() %>%
    left_join(pop_with_new_prov %>% select(commune_clean, province_new_clean, Population_2019),
        by = c("commune_clean", "province_new_clean")
    ) %>%
    select(
        Region = nvll_rg,
        Province = nvll_pr,
        Commune = NAME_3,
        Population_2019,
        Superficie_km2
    ) %>%
    arrange(Region, Province, Commune)

# 6. Calcul de la densité
merged_data <- merged_data %>%
    mutate(Densite_2019 = round(Population_2019 / Superficie_km2, 1))

# 7. Export Excel
output_file <- "outputs/rapports/BFA_communes_population_superficie_2025.xlsx"
write_xlsx(merged_data, output_file)
