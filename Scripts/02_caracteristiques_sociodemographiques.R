
# ============================================
# TABLEAU 3 - CARACTÉRISTIQUES SOCIODÉMOGRAPHIQUES
# Enquête MFV2 (2020-2021)
# ============================================

library(dplyr)
library(tidyr)
library(haven)

# Vérifier les variables nécessaires
stopifnot(all(c(
  "AGE", "SEXE", "PCS_DOMIPAR_ALT",
  "PFLIN", "territoire", "profil"
) %in% names(mfv_natifs)))

# Préparer les variables
mfv_socio <- mfv_natifs %>%
  mutate(
    sexe_code = as.character(
      haven::zap_labels(SEXE)
    ),
    
    origine_code = as.character(
      haven::zap_labels(PCS_DOMIPAR_ALT)
    ),
    
    origine_sociale = factor(
      origine_code,
      levels = c("1", "2", "3", "4", "5"),
      labels = c(
        "Dominante cadre",
        "Dominante intermédiaire",
        "Dominante employée/ouvrière",
        "Monoactif employé/ouvrier",
        "Ménage inactif"
      )
    )
  )

# Vérifier le nombre d'observations
nrow(mfv_socio)


# ============================================
# TABLEAU 3A - ÂGE ET SEXE
# ============================================

tableau3A <- mfv_socio %>%
  group_by(territoire, profil) %>%
  summarise(
    n = n(),
    
    age_moyen = round(
      weighted.mean(
        AGE,
        w = PFLIN,
        na.rm = TRUE
      ), 1
    ),
    
    femmes_pct = round(
      100 * sum(
        PFLIN[sexe_code == "2"],
        na.rm = TRUE
      ) /
        sum(
          PFLIN[sexe_code %in% c("1", "2")]
        ), 1
    ),
    
    .groups = "drop"
  )

# Afficher le tableau
View(tableau3A)


# ============================================
# TABLEAU 3B - ORIGINE SOCIALE
# ============================================

tableau3B <- mfv_socio %>%
  filter(!is.na(origine_sociale)) %>%
  group_by(
    territoire,
    profil,
    origine_sociale
  ) %>%
  summarise(
    n = n(),
    poids = sum(PFLIN),
    .groups = "drop"
  ) %>%
  group_by(territoire, profil) %>%
  mutate(
    pourcentage = 100 * poids / sum(poids)
  ) %>%
  ungroup()

# Afficher le tableau détaillé
View(tableau3B)



# ============================================
# CONTRÔLE DES INFORMATIONS MANQUANTES
# ============================================

controle_origine <- mfv_socio %>%
  group_by(territoire, profil) %>%
  summarise(
    n_total = n(),
    
    n_non_codable = sum(
      is.na(origine_sociale)
    ),
    
    pct_non_codable = round(
      100 * sum(
        PFLIN[is.na(origine_sociale)]
      ) / sum(PFLIN), 1
    ),
    
    .groups = "drop"
  )

# Vérifier les totaux du tableau 3B
controle_pourcentages_3B <- tableau3B %>%
  group_by(territoire, profil) %>%
  summarise(
    total = round(sum(pourcentage), 1),
    .groups = "drop"
  )

print(controle_pourcentages_3B)


# ============================================
# EXPORTATION DU TABLEAU 3 ET DES CONTRÔLES
# ============================================

# Installer une seule fois si nécessaire
# install.packages("writexl")

library(writexl)

# 1. Choisir le dossier de destination
dossier_export <- choose.dir(
  caption = "Choisir le dossier sécurisé des résultats MFV2"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Regrouper les tableaux
tableaux_3 <- list(
  "Tableau 3A - Age et sexe" = tableau3A,
  "Tableau 3B - Origine sociale" = tableau3B,
  "Controle origine" = controle_origine,
  "Controle pourcentages" = controle_pourcentages_3B
)

# 3. Définir le nom du fichier
fichier_export <- file.path(
  dossier_export,
  "MFV2_Tableau3_Caracteristiques_sociodemographiques.xlsx"
)

# 4. Exporter les tableaux
write_xlsx(
  tableaux_3,
  path = fichier_export
)

# 5. Vérifier l'exportation
file.exists(fichier_export)

# Afficher l'emplacement du fichier
print(fichier_export)


