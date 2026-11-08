
# ============================================
# PROFILS MIGRATOIRES DES NATIFS
# Enquête MFV2 (2020-2021)
# ============================================

library(dplyr)
library(tidyr)
library(haven)

# 1. Préparer et sélectionner la population
mfv_natifs <- ego %>%
  
  
  # ============================================
# TABLEAU 1 - POURCENTAGES PONDÉRÉS
# ============================================

tableau1 <- mfv_natifs %>%
  group_by(territoire, profil) %>%
  summarise(
    effectif_pondere = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(territoire) %>%
  mutate(
    pourcentage = 100 * effectif_pondere /
      sum(effectif_pondere)
  ) %>%
  ungroup() %>%
  select(territoire, profil, pourcentage) %>%
  pivot_wider(
    names_from = profil,
    values_from = pourcentage,
    values_fill = 0
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 1))
  )

# Afficher le tableau dans RStudio
View(tableau1)

  
  # Convertir les variables étiquetées
  mutate(
    TYPOMIG6_code = as.numeric(
      haven::zap_labels(TYPOMIG6)
    ),
    RGES_code = as.numeric(
      haven::zap_labels(RGES)
    )
  ) %>%
  
  # Sélectionner les natifs âgés de 25 à 64 ans
  filter(
    AGE >= 25,
    AGE <= 64,
    TYPOMIG6_code %in% c(1, 2, 3),
    RGES_code %in% c(1, 2, 3, 4),
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  
  # Ajouter les noms des catégories
  mutate(
    territoire = factor(
      RGES_code,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Guadeloupe",
        "Martinique",
        "Guyane",
        "La Réunion"
      )
    ),
    
    profil = factor(
      TYPOMIG6_code,
      levels = c(1, 2, 3),
      labels = c(
        "Jamais quitté",
        "Séjours courts",
        "Natifs de retour"
      )
    )
  )

# 2. Vérifier que la sélection fonctionne
nrow(mfv_natifs)


# ============================================
# CONTRÔLE 1 - EFFECTIFS NON PONDÉRÉS
# ============================================

controle_effectifs <- mfv_natifs %>%
  group_by(territoire, profil, .drop = FALSE) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )

# Voir les effectifs par territoire et profil
View(controle_effectifs)

# Vérifier le total de l'échantillon
sum(controle_effectifs$n)

# Vérifier le nombre de cellules
nrow(controle_effectifs)

# Vérifier qu'aucune cellule n'est vide
all(controle_effectifs$n > 0)


# ============================================
# CONTRÔLE 2 - SOMME DES POURCENTAGES
# ============================================

controle_pourcentages <- tableau1 %>%
  mutate(
    total = `Jamais quitté` +
      `Séjours courts` +
      `Natifs de retour`
  ) %>%
  select(territoire, total)

print(controle_pourcentages)


# ============================================
# TABLEAU 2 - REGROUPEMENT DES DIPLÔMES
# ============================================

mfv_natifs <- mfv_natifs %>%
  mutate(
    DIP14_code = as.character(
      haven::zap_labels(DIP14)
    ),
    
    niveau_diplome = case_when(
      DIP14_code %in% c("60", "70", "71") ~
        "Aucun diplôme / brevet",
      
      DIP14_code %in% c("43", "50") ~
        "CAP / BEP",
      
      DIP14_code %in% c("41", "42", "44") ~
        "Baccalauréat",
      
      DIP14_code %in% c("30", "31", "33") ~
        "Premier cycle / Bac +2",
      
      DIP14_code == "20" ~
        "Deuxième cycle / Supérieur non précisé",
      
      DIP14_code %in% c("10", "12") ~
        "Troisième cycle / Grande école",
      
      TRUE ~ NA_character_
    )
  )

# Vérifier les éventuels diplômes non classés
unique(mfv_natifs$DIP14_code[
  is.na(mfv_natifs$niveau_diplome)
])

# Vérifier le nombre de diplômes non classés
sum(is.na(mfv_natifs$niveau_diplome))



# ============================================
# TABLEAU 2 - DIPLÔME ET PROFIL MIGRATOIRE
# ============================================

tableau2 <- mfv_natifs %>%
  
  # Exclure les diplômes non renseignés
  filter(!is.na(niveau_diplome)) %>%
  
  # Regrouper selon le territoire,
  # le profil migratoire et le diplôme
  group_by(territoire, profil, niveau_diplome) %>%
  
  # Calculer les poids
  summarise(
    effectif_pondere = sum(PFLIN),
    .groups = "drop"
  ) %>%
  
  # Calculer les pourcentages
  # au sein de chaque profil et territoire
  group_by(territoire, profil) %>%
  mutate(
    pourcentage = 100 * effectif_pondere /
      sum(effectif_pondere)
  ) %>%
  ungroup()

# Afficher les résultats
View(tableau2)


# Contrôle des pourcentages par groupe
controle_tableau2 <- tableau2 %>%
  group_by(territoire, profil) %>%
  summarise(
    total = round(sum(pourcentage), 1),
    .groups = "drop"
  )

print(controle_tableau2)


# ============================================
# TABLEAU 2 - MISE EN FORME
# ============================================

# Définir l'ordre des diplômes
ordre_diplomes <- c(
  "Aucun diplôme / brevet",
  "CAP / BEP",
  "Baccalauréat",
  "Premier cycle / Bac +2",
  "Deuxième cycle / Supérieur non précisé",
  "Troisième cycle / Grande école"
)

# Réorganiser le tableau
tableau2_final <- tableau2 %>%
  mutate(
    niveau_diplome = factor(
      niveau_diplome,
      levels = ordre_diplomes
    )
  ) %>%
  select(
    territoire,
    niveau_diplome,
    profil,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = profil,
    values_from = pourcentage
  ) %>%
  arrange(
    territoire,
    niveau_diplome
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )

# Afficher le résultat
View(tableau2_final)


# Contrôle local des effectifs par cellule

controle_diplomes <- mfv_natifs %>%
  filter(!is.na(niveau_diplome)) %>%
  count(
    territoire,
    profil,
    niveau_diplome,
    name = "effectif"
  ) %>%
  tidyr::complete(
    territoire,
    profil,
    niveau_diplome,
    fill = list(effectif = 0)
  )

View(controle_diplomes)

# Effectif minimum
min(controle_diplomes$effectif)


# ============================================
# EXPORT DES TABLEAUX DESCRIPTIFS
# ET DES CONTRÔLES MÉTHODOLOGIQUES
# ============================================

library(writexl)

# Sélectionner un dossier local sécurisé
dossier_export <- choose.dir(
  caption = "Choisir le dossier sécurisé pour les résultats MFV2"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# Réunir les tableaux
tableaux_memoire <- list(
  "Tableau 1 - Mobilite" = tableau1,
  "Tableau 2 - Diplomes" = tableau2_final,
  "Controle effectifs" = controle_effectifs,
  "Controle diplomes" = controle_diplomes,
  "Controle pourcentages T1" = controle_pourcentages,
  "Controle pourcentages T2" = controle_tableau2
)

# Exporter dans un seul fichier Excel
write_xlsx(
  tableaux_memoire,
  path = file.path(
    dossier_export,
    "MFV2_Tableaux_descriptifs_M2.xlsx"
  )
)

message("Exportation terminée.")



