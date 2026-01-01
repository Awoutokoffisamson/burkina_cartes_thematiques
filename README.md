# Burkina Faso - Cartes Thématiques

## Description
Ce dépôt contient les **cartes de visualisation** des subdivisions administratives du Burkina Faso avec les données de population et densité.

## 🗺️ Cartes Disponibles

### Cartes Administratives
| Carte | Description |
|-------|-------------|
| `1_Carte_Regionale_2025.png` | 17 régions avec étiquettes |
| `2_Carte_Provinciale_2025.png` | 47 provinces |
| `3_Carte_Provinciale_Etiquettes_2025.png` | Provinces avec noms |
| `4_Carte_Hierarchie_Regions_Provinces.png` | Hiérarchie administrative |
| `5_Carte_Communale_2025.png` | 351 communes |

### Cartes Thématiques
| Carte | Description |
|-------|-------------|
| `BFA_Regions_Population_2019.png` | Population par région |
| `BFA_Provinces_Densite_2019.png` | Densité par province |

## Structure
```
├── cartes/         # Images PNG des cartes
├── scripts/        # Scripts R de génération
│   ├── 02_visualisation_cartes.R
│   ├── 04_merge_population.R
│   └── 05_visualisation_population.R
└── rapports/       # Rapports et tableaux
```

## Reproduction
Pour régénérer les cartes :
```r
source("scripts/02_visualisation_cartes.R")
source("scripts/05_visualisation_population.R")
```

## Sources
- **Population** : INSD - RGPH 2019
- **Géographie** : Shapefiles 2025

## Auteur
AWOUTO K. Samson - Élève Ingénieur Statisticien Économiste, ENSAE Dakar

## 🛡️ Droits d'Utilisation
Les cartes produites peuvent être utilisées librement dans des rapports académiques ou de recherche, à condition de citer la source. Toute utilisation commerciale est interdite (Licence CC BY-NC-SA 4.0).

## 📜 Citation
> AWOUTO, K. S. (2026). *Cartes Thématiques Burkina Faso 2025*. ENSAE Dakar. https://github.com/Awoutokoffisamson/burkina_cartes_thematiques

