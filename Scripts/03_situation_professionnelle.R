
# ============================================
# TABLEAU 4 - SITUATION PROFESSIONNELLE
# Sept catégories originales de MFV2
# ============================================

library(dplyr)
library(tidyr)
library(haven)

# 1. Préparer la variable

mfv_emploi <- mfv_natifs %>%
  mutate(
    situation_7 = factor(
      as.numeric(haven::zap_labels(SITUA_rec)),
      levels = 1:7,
      labels = c(
        "En emploi",
        "Études / formation / apprentissage",
        "Chômage déclaré",
        "Retraite / préretraite",
        "Au foyer",
        "Inactivité pour invalidité",
        "Autre inactivité"
      )
    )
  )

# Vérification de l'échantillon
nrow(mfv_emploi)

# Vérification des situations manquantes
sum(is.na(mfv_emploi$situation_7))


# 2. Calculer les effectifs et pourcentages

tableau4 <- mfv_emploi %>%
  filter(!is.na(situation_7)) %>%
  group_by(
    territoire,
    profil,
    situation_7,
    .drop = FALSE
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


# 3. Construire le tableau comparatif

tableau4_final <- tableau4 %>%
  select(
    territoire,
    profil,
    situation_7,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = situation_7,
    values_from = pourcentage
  ) %>%
  arrange(territoire, profil) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )

View(tableau4_final)


# 4. Contrôler les résultats

controle_tableau4 <- tableau4 %>%
  group_by(territoire, profil) %>%
  summarise(
    total = round(sum(pourcentage), 1),
    .groups = "drop"
  )

print(controle_tableau4)

# Contrôler les effectifs localement
sum(tableau4$n < 10)
min(tableau4$n)


# ============================================
# CONTRÔLE DES EFFECTIFS - TABLEAU 4
# ============================================

controle_effectifs_4 <- tableau4 %>%
  select(
    territoire,
    profil,
    situation_7,
    n
  )

# Afficher le tableau
View(controle_effectifs_4)


# ============================================
# EXPORTATION DU TABLEAU 4
# Situation professionnelle - MFV2
# ============================================

library(writexl)

# 1. Choisir un dossier sécurisé
dossier_export <- choose.dir(
  caption = "Choisir le dossier sécurisé des résultats MFV2"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Définir le fichier Excel
fichier_export <- file.path(
  dossier_export,
  "MFV2_Tableau4_Situation_professionnelle.xlsx"
)

# 3. Exporter les trois tableaux
write_xlsx(
  list(
    "Tableau 4" = tableau4_final,
    "Controle effectifs" = controle_effectifs_4,
    "Controle pourcentages" = controle_tableau4
  ),
  path = fichier_export
)

# 4. Vérifier que l'exportation a réussi
file.exists(fichier_export)

# Afficher l'emplacement du fichier
print(fichier_export)


# ============================================
# TABLEAU 4 BIS - EMPLOI ET DIPLÔME
# Enquête MFV2 (2020-2021)
# ============================================

# 1. Charger les bibliothèques
library(dplyr)
library(tidyr)
library(haven)

# 2. Vérifier les variables nécessaires
stopifnot(all(c(
  "niveau_diplome",
  "SITUA_rec",
  "territoire",
  "profil",
  "PFLIN"
) %in% names(mfv_natifs)))

# 3. Préparer la variable d'emploi
mfv_croisement <- mfv_natifs %>%
  mutate(
    situa_code = as.numeric(
      haven::zap_labels(SITUA_rec)
    ),
    
    en_emploi = case_when(
      situa_code == 1 ~ 1,
      situa_code %in% 2:7 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # Conserver les diplômes et situations renseignés
  filter(
    !is.na(niveau_diplome),
    !is.na(en_emploi)
  )

# 4. Vérifier la nouvelle population
nrow(mfv_croisement)



# ============================================
# TABLEAU 4 BIS - POURCENTAGES PONDÉRÉS
# Emploi selon le diplôme et le profil migratoire
# ============================================

tableau4bis_detail <- mfv_croisement %>%
  group_by(
    territoire,
    profil,
    niveau_diplome
  ) %>%
  summarise(
    # Effectif non pondéré
    n = n(),
    
    # Effectif des personnes déclarant un emploi
    n_emploi = sum(en_emploi == 1),
    
    # Poids total du groupe
    poids_total = sum(PFLIN),
    
    # Pourcentage pondéré de personnes en emploi
    pourcentage_emploi = 100 * weighted.mean(
      en_emploi,
      w = PFLIN
    ),
    
    .groups = "drop"
  )

# Visualiser les résultats localement
View(tableau4bis_detail)



# Nombre de combinaisons observées
nrow(tableau4bis_detail)

# Vérifier que les effectifs correspondent
sum(tableau4bis_detail$n)

# Vérifier que les pourcentages sont compris
# entre 0 et 100
all(
  tableau4bis_detail$pourcentage_emploi >= 0 &
    tableau4bis_detail$pourcentage_emploi <= 100
)


# ============================================
# TABLEAU 4 BIS - PRÉSENTATION COMPARATIVE
# ============================================

tableau4bis_final <- tableau4bis_detail %>%
  
  # Classer les niveaux de diplôme
  mutate(
    niveau_diplome = factor(
      niveau_diplome,
      levels = c(
        "Aucun diplôme / brevet",
        "CAP / BEP",
        "Baccalauréat",
        "Premier cycle / Bac +2",
        "Deuxième cycle / Supérieur non précisé",
        "Troisième cycle / Grande école"
      )
    )
  ) %>%
  
  # Sélectionner les variables utiles
  select(
    territoire,
    niveau_diplome,
    profil,
    pourcentage_emploi
  ) %>%
  
  # Mettre les profils migratoires en colonnes
  pivot_wider(
    names_from = profil,
    values_from = pourcentage_emploi
  ) %>%
  
  # Organiser les résultats
  arrange(territoire, niveau_diplome) %>%
  
  # Arrondir les pourcentages
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 1)
    )
  )

# Visualiser le tableau
View(tableau4bis_final)


# Vérifier les dimensions
dim(tableau4bis_final)

# Vérifier les noms des colonnes
names(tableau4bis_final)

# Vérifier les éventuelles valeurs manquantes
sum(is.na(tableau4bis_final))

# Vérifier que les 5 283 personnes
# sont toujours comptabilisées
sum(tableau4bis_detail$n)



# ============================================
# ÉTAPE 4 - CONTRÔLES DU TABLEAU 4 BIS
# ============================================

# 1. Identifier les combinaisons manquantes

controle_manquants_4bis <- tableau4bis_final %>%
  pivot_longer(
    cols = c(
      "Jamais quitté",
      "Séjours courts",
      "Natifs de retour"
    ),
    names_to = "profil",
    values_to = "pourcentage_emploi"
  ) %>%
  filter(is.na(pourcentage_emploi)) %>%
  select(
    territoire,
    niveau_diplome,
    profil
  )

# Afficher les combinaisons concernées
print(controle_manquants_4bis)

# Vérifier le recodage des diplômes
sum(is.na(tableau4bis_final$niveau_diplome))


# ============================================
# ÉTAPE 4 - CONTRÔLE DES PETITS EFFECTIFS
# ============================================

controle_effectifs_4bis <- tableau4bis_detail %>%
  select(
    territoire,
    niveau_diplome,
    profil,
    n,
    n_emploi
  ) %>%
  mutate(
    effectif_inferieur_10 = n < 10
  )

# Existe-t-il des groupes de moins de 10 individus ?
any(controle_effectifs_4bis$effectif_inferieur_10)

# Vérifier localement les groupes concernés
View(controle_effectifs_4bis)



# ============================================
# ÉTAPE 5 - DIAGNOSTIC DES PETITS EFFECTIFS
# ============================================

# 1. Compter les groupes de moins de 10 individus
sum(controle_effectifs_4bis$n < 10)

# 2. Identifier les niveaux de diplôme concernés
# On compte les GROUPES, pas les individus

diagnostic_diplomes <- controle_effectifs_4bis %>%
  group_by(niveau_diplome) %>%
  summarise(
    nombre_groupes = n(),
    groupes_moins_10 = sum(n < 10),
    .groups = "drop"
  )

print(diagnostic_diplomes)


# ============================================
# ÉTAPE 6 - REGROUPEMENT DES DIPLÔMES
# Passage de 6 à 5 niveaux
# ============================================

# 1. Créer une nouvelle variable de diplôme

mfv_croisement_5 <- mfv_croisement %>%
  mutate(
    diplome_5 = case_when(
      as.character(niveau_diplome) %in% c(
        "Deuxième cycle / Supérieur non précisé",
        "Troisième cycle / Grande école"
      ) ~ "Supérieur au Bac +2",
      
      TRUE ~ as.character(niveau_diplome)
    ),
    
    diplome_5 = factor(
      diplome_5,
      levels = c(
        "Aucun diplôme / brevet",
        "CAP / BEP",
        "Baccalauréat",
        "Premier cycle / Bac +2",
        "Supérieur au Bac +2"
      )
    )
  )

# 2. Vérifier le recodage

table(mfv_croisement_5$diplome_5)

sum(is.na(mfv_croisement_5$diplome_5))


# 3. Recalculer les pourcentages pondérés

tableau4bis_5 <- mfv_croisement_5 %>%
  group_by(
    territoire,
    profil,
    diplome_5
  ) %>%
  summarise(
    n = n(),
    
    pourcentage_emploi = 100 * weighted.mean(
      en_emploi,
      w = PFLIN
    ),
    
    .groups = "drop"
  )


# 4. Contrôler les petits effectifs

diagnostic_diplomes_5 <- tableau4bis_5 %>%
  group_by(diplome_5) %>%
  summarise(
    nombre_groupes = n(),
    groupes_moins_10 = sum(n < 10),
    .groups = "drop"
  )

print(diagnostic_diplomes_5)

# 5. Vérifier la population totale

sum(tableau4bis_5$n)


# ============================================
# ÉTAPE 7 - REGROUPEMENT EN 4 NIVEAUX
# ============================================

# 1. Créer une nouvelle classification

mfv_croisement_4 <- mfv_croisement_5 %>%
  mutate(
    diplome_4 = case_when(
      as.character(diplome_5) %in% c(
        "Premier cycle / Bac +2",
        "Supérieur au Bac +2"
      ) ~ "Enseignement supérieur",
      
      TRUE ~ as.character(diplome_5)
    ),
    
    diplome_4 = factor(
      diplome_4,
      levels = c(
        "Aucun diplôme / brevet",
        "CAP / BEP",
        "Baccalauréat",
        "Enseignement supérieur"
      )
    )
  )

# 2. Vérifier les valeurs manquantes
sum(is.na(mfv_croisement_4$diplome_4))

# 3. Recalculer les pourcentages pondérés

tableau4bis_4 <- mfv_croisement_4 %>%
  group_by(
    territoire,
    profil,
    diplome_4
  ) %>%
  summarise(
    n = n(),
    
    pourcentage_emploi = 100 * weighted.mean(
      en_emploi,
      w = PFLIN
    ),
    
    .groups = "drop"
  )

# 4. Diagnostiquer les petits effectifs

diagnostic_diplomes_4 <- tableau4bis_4 %>%
  group_by(diplome_4) %>%
  summarise(
    nombre_groupes = n(),
    groupes_moins_10 = sum(n < 10),
    .groups = "drop"
  )

print(diagnostic_diplomes_4)

# 5. Vérifier que la population est conservée
sum(tableau4bis_4$n)



# ============================================
# ÉTAPE 8 - TABLEAU COMPARATIF À 4 DIPLÔMES
# ============================================

# 1. Préparer une version prudente du tableau
# Les groupes de moins de 10 individus sont masqués

tableau4bis_4_final <- tableau4bis_4 %>%
  mutate(
    pourcentage_affiche = if_else(
      n < 10,
      NA_real_,
      round(pourcentage_emploi, 1)
    )
  ) %>%
  
  # 2. Sélectionner les variables
  select(
    territoire,
    diplome_4,
    profil,
    pourcentage_affiche
  ) %>%
  
  # 3. Placer les profils migratoires en colonnes
  pivot_wider(
    names_from = profil,
    values_from = pourcentage_affiche
  ) %>%
  
  # 4. Classer les lignes
  arrange(territoire, diplome_4)

# 5. Visualiser le tableau
View(tableau4bis_4_final)


# Dimensions du tableau
dim(tableau4bis_4_final)

# Nombre de valeurs manquantes ou masquées
sum(is.na(tableau4bis_4_final))

# Vérifier la population analysée
sum(tableau4bis_4$n)


# ============================================
# ÉTAPE 9 - EXPORTATION DU TABLEAU 4 BIS
# ============================================

library(writexl)

# 1. Préparer le contrôle des effectifs
controle_effectifs_4bis_4 <- tableau4bis_4 %>%
  select(
    territoire,
    profil,
    diplome_4,
    n
  ) %>%
  mutate(
    effectif_inferieur_10 = n < 10
  )

# 2. Choisir un dossier local sécurisé
dossier_export <- choose.dir(
  caption = "Choisir le dossier sécurisé des résultats MFV2"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 3. Définir le nom du fichier
fichier_export <- file.path(
  dossier_export,
  "MFV2_Tableau4bis_Emploi_Diplome.xlsx"
)

# 4. Exporter les trois feuilles
write_xlsx(
  list(
    "Tableau 4 bis" = tableau4bis_4_final,
    "Controle effectifs" = controle_effectifs_4bis_4,
    "Diagnostic" = diagnostic_diplomes_4
  ),
  path = fichier_export
)

# 5. Vérifier l'exportation
file.exists(fichier_export)

# 6. Afficher l'emplacement du fichier
print(fichier_export)



