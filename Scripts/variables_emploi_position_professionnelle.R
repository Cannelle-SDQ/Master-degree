
# ============================================
# ÉTAPE 1 - RECHERCHE DES VARIABLES MFV2
# ============================================

library(dplyr)

# 1. Récupérer les intitulés des variables
libelles <- vapply(
  ego,
  function(x) {
    lab <- attr(x, "label")
    if (is.null(lab)) "" else as.character(lab)[1]
  },
  character(1)
)

# 2. Créer un dictionnaire des variables
dictionnaire <- data.frame(
  variable = names(ego),
  libelle = unname(libelles)
)

# 3. Rechercher les variables relatives à l'emploi
variables_emploi <- dictionnaire %>%
  filter(
    grepl(
      "emploi|travail|activité|chômage|profession|apprenti|stage|retraite",
      paste(variable, libelle),
      ignore.case = TRUE
    )
  )

# Consulter les résultats
View(variables_emploi)


# ============================================
# ÉTAPE 2 - SECTEUR ET POSITION PROFESSIONNELLE
# ============================================

variables_position <- dictionnaire %>%
  filter(
    grepl(
      "public|privé|secteur|fonction publique|employeur|cadre|socioprofessionnel|PCS|contrat|statut",
      paste(variable, libelle),
      ignore.case = TRUE
    )
  )

# Consulter les variables
View(variables_position)


# ============================================
# ÉTAPE 3 - MODALITÉS DE SITUA_rec
# ============================================

# Intitulé de la variable
attr(ego$SITUA_rec, "label")

# Modalités et codes
attr(ego$SITUA_rec, "labels")


# ============================================
# ÉTAPE 4 - EXPORTATION DES VARIABLES MFV2
# ============================================

library(writexl)

# 1. Choisir le dossier d'exportation
dossier_export <- choose.dir(
  caption = "Choisir le dossier d'exportation"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Définir le nom du fichier Excel
fichier_export <- file.path(
  dossier_export,
  "MFV2_Dictionnaire_Emploi_Position.xlsx"
)

# 3. Exporter les tableaux
write_xlsx(
  list(
    "Variables emploi" = variables_emploi,
    "Variables position" = variables_position
  ),
  path = fichier_export
)

# 4. Vérifier que le fichier a été créé
file.exists(fichier_export)

# 5. Afficher son emplacement
print(fichier_export)


# ============================================
# ÉTAPE 5 - EXAMINER LES MODALITÉS
# DES VARIABLES PRINCIPALES
# ============================================

# Variables à examiner
variables_cibles <- c(
  "TRAVAIL",
  "SITUA_rec",
  "CS8",
  "CLASSIF_rec",
  "STATUT"
)

# Afficher leurs intitulés et modalités
for (v in variables_cibles) {
  
  cat("\n=========================\n")
  cat("VARIABLE :", v, "\n")
  cat("=========================\n")
  
  # Intitulé
  print(attr(ego[[v]], "label"))
  
  # Modalités et codes
  print(attr(ego[[v]], "labels"))
}


# ============================================
# ÉTAPE 6 - COMPARAISON DES VARIABLES D'EMPLOI
# TRAVAIL et SITUA_rec
# ============================================

library(dplyr)
library(haven)

# 1. Préparer les deux variables

mfv_comparaison_emploi <- mfv_natifs %>%
  mutate(
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    
    situa_code = as.numeric(
      haven::zap_labels(SITUA_rec)
    )
  )

# 2. Croiser les deux variables
# Les effectifs restent dans RStudio

controle_travail_situation <- table(
  mfv_comparaison_emploi$situa_code,
  mfv_comparaison_emploi$travail_code,
  useNA = "ifany"
)

# 3. Afficher le croisement
print(controle_travail_situation)

# 4. Afficher les pourcentages en ligne
round(
  100 * prop.table(
    controle_travail_situation,
    margin = 1
  ),
  1
)


# ============================================
# ÉTAPE 7 - COMPARER DEUX DÉFINITIONS
# DE L'EMPLOI
# ============================================

# 1. Créer les deux indicateurs

mfv_comparaison_emploi <- mfv_comparaison_emploi %>%
  mutate(
    
    # Définition 1 : situation professionnelle
    emploi_situa = case_when(
      situa_code == 1 ~ 1,
      situa_code %in% 2:7 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Définition 2 : travaille actuellement
    emploi_travail = case_when(
      travail_code == "1" ~ 1,
      travail_code == "2" ~ 0,
      TRUE ~ NA_real_
    )
  )

# 2. Calculer les proportions pondérées

comparaison_definitions <- mfv_comparaison_emploi %>%
  summarise(
    
    emploi_situa_pct = 100 * weighted.mean(
      emploi_situa,
      PFLIN,
      na.rm = TRUE
    ),
    
    emploi_travail_pct = 100 * weighted.mean(
      emploi_travail,
      PFLIN,
      na.rm = TRUE
    )
  ) %>%
  mutate(
    difference_points =
      emploi_travail_pct - emploi_situa_pct
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

# 3. Afficher les résultats
print(comparaison_definitions)



# ============================================
# ÉTAPE 8 - COMPARAISON SELON LE PROFIL MIGRATOIRE
# ============================================

comparaison_par_profil <- mfv_comparaison_emploi %>%
  mutate(
    profil_migratoire = as.character(
      haven::as_factor(TYPOMIG6)
    )
  ) %>%
  group_by(profil_migratoire) %>%
  summarise(
    emploi_situa_pct = 100 * weighted.mean(
      emploi_situa, PFLIN, na.rm = TRUE
    ),
    
    emploi_travail_pct = 100 * weighted.mean(
      emploi_travail, PFLIN, na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    difference_points =
      emploi_travail_pct - emploi_situa_pct
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  )

# Afficher les résultats
print(comparaison_par_profil)


# ============================================
# ÉTAPE 9 - ROBUSTESSE SELON LA DÉFINITION
# DE L'EMPLOI
# ============================================

library(survey)
library(haven)

# 1. Ajouter la nouvelle variable d'emploi
# au plan d'analyse déjà utilisé

plan_mfv_age_travail <- update(
  plan_mfv_age,
  
  emploi_travail = dplyr::case_when(
    as.character(haven::zap_labels(TRAVAIL)) == "1" ~ 1,
    as.character(haven::zap_labels(TRAVAIL)) == "2" ~ 0,
    TRUE ~ NA_real_
  )
)

# 2. Réestimer le modèle avec TRAVAIL
# en conservant les mêmes variables explicatives

modele4_travail <- svyglm(
  formula = update(
    formula(modele4_age2),
    emploi_travail ~ .
  ),
  design = plan_mfv_age_travail,
  family = quasibinomial()
)

# 3. Comparer les effectifs utilisés
cat("Modèle SITUA :", nobs(modele4_age2), "\n")
cat("Modèle TRAVAIL :", nobs(modele4_travail), "\n")

# 4. Comparer les odds ratios des profils migratoires

extraire_or_profil <- function(modele) {
  coefficients <- coef(modele)
  
  exp(coefficients[
    grepl("^profil", names(coefficients))
  ])
}

or_situa <- extraire_or_profil(modele4_age2)
or_travail <- extraire_or_profil(modele4_travail)

comparaison_or <- data.frame(
  profil = names(or_situa),
  OR_SITUA = round(as.numeric(or_situa), 3),
  OR_TRAVAIL = round(as.numeric(or_travail), 3)
)

print(comparaison_or)


nobs(modele4_age2)

nobs(modele4_travail)


# ============================================
# ÉTAPE 10 - VÉRIFIER LA VARIABLE STATUT
# CHEZ LES PERSONNES QUI TRAVAILLENT
# ============================================

library(dplyr)
library(haven)

# 1. Conserver les personnes déclarant travailler
mfv_statut_actuel <- mfv_natifs %>%
  mutate(
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    statut_code = as.character(
      haven::zap_labels(STATUT)
    )
  ) %>%
  filter(travail_code == "1")

# 2. Vérifier la disponibilité de STATUT
controle_statut <- mfv_statut_actuel %>%
  summarise(
    total_travailleurs = n(),
    statut_renseigne = sum(!is.na(statut_code)),
    statut_manquant = sum(is.na(statut_code)),
    pct_renseigne = round(
      100 * mean(!is.na(statut_code)), 2
    )
  )

print(controle_statut)

# 3. Examiner les codes de STATUT
# À consulter localement dans RStudio
table(
  mfv_statut_actuel$statut_code,
  useNA = "ifany"
)


# ============================================
# ÉTAPE 11 - CONTRÔLE DES CODES DE STATUT
# ============================================

controle_codes_statut <- mfv_statut_actuel %>%
  summarise(
    total = n(),
    
    # Codes attendus : de 1 à 7
    codes_valides = sum(
      trimws(statut_code) %in% as.character(1:7)
    ),
    
    # Valeurs absentes, vides ou inattendues
    codes_non_valides = sum(
      !(trimws(statut_code) %in% as.character(1:7))
    ),
    
    pct_valides = round(
      100 * codes_valides / total, 2
    )
  )

print(controle_codes_statut)


# ============================================
# ÉTAPE 12 - RÉPARTITION DES 7 STATUTS
# SELON LE PROFIL MIGRATOIRE
# ============================================

library(dplyr)
library(haven)
library(tidyr)

# 1. Conserver tous les statuts professionnels
mfv_statuts <- mfv_statut_actuel %>%
  mutate(
    profil_migratoire = as.character(
      haven::as_factor(TYPOMIG6)
    ),
    
    statut_professionnel = case_when(
      statut_code == "1" ~ "Salarié de l'État",
      statut_code == "2" ~ "Collectivités / HLM / Hôpitaux publics",
      statut_code == "3" ~ "Entreprise / Artisan / Association",
      statut_code == "4" ~ "Salarié d'un particulier",
      statut_code == "5" ~ "Aide familial non rémunéré",
      statut_code == "6" ~ "Chef d'entreprise salarié",
      statut_code == "7" ~ "Indépendant",
      TRUE ~ "Non renseigné / autre code"
    )
  )

# 2. Calculer les proportions pondérées
# à l'intérieur de chaque profil migratoire

tableau_statuts_profils <- mfv_statuts %>%
  group_by(profil_migratoire, statut_professionnel) %>%
  summarise(
    poids_total = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(profil_migratoire) %>%
  mutate(
    pourcentage = round(
      100 * poids_total / sum(poids_total),
      2
    )
  ) %>%
  ungroup() %>%
  select(
    profil_migratoire,
    statut_professionnel,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = profil_migratoire,
    values_from = pourcentage,
    values_fill = 0
  )

# 3. Afficher le tableau
print(tableau_statuts_profils, width = Inf)


# ============================================
# ÉTAPE 13 - STATUTS SELON LE DROM
# ============================================

library(dplyr)
library(haven)
library(tidyr)

# 1. Identifier les territoires
mfv_statuts <- mfv_statuts %>%
  mutate(
    territoire = as.character(
      haven::as_factor(RGES)
    )
  )

# 2. Calculer les répartitions pondérées
tableau_statuts_drom <- mfv_statuts %>%
  group_by(territoire, statut_professionnel) %>%
  summarise(
    n = n(),
    poids_total = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(territoire) %>%
  mutate(
    pourcentage = round(
      100 * poids_total / sum(poids_total),
      2
    )
  ) %>%
  ungroup()

# 3. Tableau de lecture
tableau_statuts_drom_large <- tableau_statuts_drom %>%
  select(territoire, statut_professionnel, pourcentage) %>%
  pivot_wider(
    names_from = territoire,
    values_from = pourcentage
  )

# Consulter localement dans RStudio
View(tableau_statuts_drom_large)


# ============================================
# ÉTAPE 14 - EXPORTATION DU TABLEAU
# DES STATUTS PROFESSIONNELS PAR DROM
# ============================================

library(writexl)

# 1. Choisir le dossier d'exportation
dossier_export <- choose.dir(
  caption = "Choisir le dossier d'exportation"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Définir le nom du fichier
fichier_export <- file.path(
  dossier_export,
  "MFV2_Statuts_Professionnels_par_DROM.xlsx"
)

# 3. Exporter le tableau
write_xlsx(
  tableau_statuts_drom_large,
  path = fichier_export
)

# 4. Vérifier l'exportation
file.exists(fichier_export)

# 5. Afficher l'emplacement
print(fichier_export)


# ============================================
# ÉTAPE 15 - EMPLOI PUBLIC PAR DROM
# ET PROFIL MIGRATOIRE
# ============================================

library(dplyr)
library(haven)

tableau_public_territoire_profil <- mfv_statuts %>%
  mutate(
    territoire = as.character(
      haven::as_factor(RGES)
    ),
    
    profil_migratoire = as.character(
      haven::as_factor(TYPOMIG6)
    )
  ) %>%
  filter(
    statut_code %in% as.character(1:7),
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  group_by(territoire, profil_migratoire) %>%
  summarise(
    # Effectifs pour les contrôles de confidentialité
    n_total = n(),
    n_etat = sum(statut_code == "1"),
    n_collectivites = sum(statut_code == "2"),
    
    # Pourcentages pondérés
    pct_etat = 100 * sum(
      PFLIN[statut_code == "1"]
    ) / sum(PFLIN),
    
    pct_collectivites = 100 * sum(
      PFLIN[statut_code == "2"]
    ) / sum(PFLIN),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("pct_"), ~ round(.x, 2))
  )

# Consultation locale
View(tableau_public_territoire_profil)


# ============================================
# ÉTAPE 16 - EXPORTATION DU TABLEAU
# EMPLOI PUBLIC PAR DROM ET PROFIL MIGRATOIRE
# ============================================

library(writexl)

# 1. Choisir le dossier d'exportation
dossier_export <- choose.dir(
  caption = "Choisir le dossier d'exportation"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Définir le nom du fichier Excel
fichier_export <- file.path(
  dossier_export,
  "MFV2_Emploi_Public_DROM_Profil_Migratoire.xlsx"
)

# 3. Exporter le tableau
write_xlsx(
  tableau_public_territoire_profil,
  path = fichier_export
)

# 4. Vérifier que le fichier a été créé
file.exists(fichier_export)

# 5. Afficher son emplacement
print(fichier_export)


# ============================================
# ÉTAPE 17 - PRÉPARATION DES MODÈLES
# SUR LES STATUTS PROFESSIONNELS
# ============================================

library(dplyr)
library(haven)
library(survey)

# 1. Vérifier la présence des variables
variables_necessaires <- c("TRAVAIL", "STATUT")

if (!all(variables_necessaires %in%
         names(plan_mfv_age$variables))) {
  stop("TRAVAIL ou STATUT est absent du plan d'analyse.")
}

# 2. Récupérer les codes des deux variables
# dans notre plan pondéré existant

plan_statuts <- update(
  plan_mfv_age,
  
  travail_code = as.character(
    haven::zap_labels(TRAVAIL)
  ),
  
  statut_code = trimws(
    as.character(haven::zap_labels(STATUT))
  )
)

# 3. Sélectionner les personnes qui travaillent
# et possèdent un statut valide

plan_statuts <- subset(
  plan_statuts,
  travail_code == "1" &
    statut_code %in% as.character(1:7)
)

# 4. Créer les deux variables binaires

plan_statuts <- update(
  plan_statuts,
  
  # Modèle A : salarié de l'État
  est_etat = as.integer(statut_code == "1"),
  
  # Modèle B : collectivités / HLM / hôpitaux publics
  est_collectivites = as.integer(statut_code == "2")
)

# 5. Vérifier la population retenue
nrow(plan_statuts$variables)


# ============================================
# ÉTAPE 18 - RÉGRESSIONS LOGISTIQUES
# ============================================

# MODÈLE A - Être salarié de l'État

modele_etat <- svyglm(
  formula = update(
    formula(modele4_age2),
    est_etat ~ .
  ),
  design = plan_statuts,
  family = quasibinomial()
)

# MODÈLE B - Être salarié des collectivités,
# HLM ou hôpitaux publics

modele_collectivites <- svyglm(
  formula = update(
    formula(modele4_age2),
    est_collectivites ~ .
  ),
  design = plan_statuts,
  family = quasibinomial()
)

# Vérifier les effectifs utilisés
nobs(modele_etat)
nobs(modele_collectivites)


# ============================================
# ÉTAPE 19 - COMPARAISON DES ODDS RATIOS
# ============================================

# 1. Extraire les odds ratios
or_etat <- exp(coef(modele_etat))

or_collectivites <- exp(coef(modele_collectivites))

# 2. Construire un tableau comparatif
tableau_or_statuts <- data.frame(
  variable = names(or_etat),
  OR_Etat = round(as.numeric(or_etat), 3),
  OR_Collectivites = round(
    as.numeric(or_collectivites), 3
  )
) %>%
  filter(grepl("^profil", variable))

# 3. Afficher les résultats
print(tableau_or_statuts)


# ============================================
# ÉTAPE 20 - COMPARER TOUS LES ODDS RATIOS
# DES DEUX MODÈLES
# ============================================

library(dplyr)

# 1. Extraire les coefficients
coef_etat <- coef(modele_etat)
coef_collectivites <- coef(modele_collectivites)

# 2. Exclure la constante
variables <- setdiff(
  names(coef_etat),
  "(Intercept)"
)

# 3. Construire le tableau comparatif
tableau_or_complet <- data.frame(
  variable = variables,
  
  OR_Etat = round(
    exp(coef_etat[variables]), 3
  ),
  
  OR_Collectivites = round(
    exp(coef_collectivites[variables]), 3
  ),
  
  row.names = NULL
)

# 4. Afficher les résultats
print(tableau_or_complet, row.names = FALSE)


# ============================================
# ÉTAPE 21 - COEFFICIENTS EXACTS DE L'ÂGE
# ============================================

tableau_age_modeles <- data.frame(
  variable = c("age_c", "age_c2"),
  
  coefficient_etat = coef(modele_etat)[
    c("age_c", "age_c2")
  ],
  
  coefficient_collectivites = coef(modele_collectivites)[
    c("age_c", "age_c2")
  ],
  
  row.names = NULL
)

# Afficher 7 décimales
print(
  tableau_age_modeles,
  digits = 7,
  row.names = FALSE
)


# ============================================
# ÉTAPE 22 - PROBABILITÉS PRÉDITES AJUSTÉES
# SELON LE PROFIL MIGRATOIRE
# ============================================

# 1. Base commune aux deux modèles
base_prediction <- plan_statuts$variables

# 2. Les trois profils migratoires
profils <- levels(base_prediction$profil)

print(profils)

# 3. Fonction de calcul des probabilités ajustées
calculer_probabilite <- function(profil_choisi, modele) {
  
  nouvelles_donnees <- base_prediction
  
  # Attribuer le même profil à chaque observation
  nouvelles_donnees$profil <- factor(
    rep(profil_choisi, nrow(nouvelles_donnees)),
    levels = levels(base_prediction$profil)
  )
  
  # Prédire les probabilités individuelles
  predictions <- as.numeric(
    predict(
      modele,
      newdata = nouvelles_donnees,
      type = "response",
      se.fit = FALSE
    )
  )
  
  # Calculer la moyenne pondérée
  100 * weighted.mean(
    predictions,
    w = base_prediction$PFLIN,
    na.rm = TRUE
  )
}

# 4. Comparer les probabilités par profil
tableau_prob_statuts <- data.frame(
  profil_migratoire = profils,
  
  pct_etat = sapply(
    profils,
    calculer_probabilite,
    modele = modele_etat
  ),
  
  pct_collectivites = sapply(
    profils,
    calculer_probabilite,
    modele = modele_collectivites
  ),
  row.names = NULL
)

# 5. Arrondir et afficher
tableau_prob_statuts$pct_etat <-
  round(tableau_prob_statuts$pct_etat, 2)

tableau_prob_statuts$pct_collectivites <-
  round(tableau_prob_statuts$pct_collectivites, 2)

print(tableau_prob_statuts)


# ============================================
# ÉTAPE 23 - MODÈLE COMPLÉMENTAIRE
# SALARIAT PUBLIC OU ASSIMILÉ
# ============================================

# 1. Créer la variable regroupée
plan_statuts <- update(
  plan_statuts,
  est_public_assimile = as.integer(
    statut_code %in% c("1", "2")
  )
)

# 2. Estimer le modèle
modele_public_assimile <- svyglm(
  formula = update(
    formula(modele4_age2),
    est_public_assimile ~ .
  ),
  design = plan_statuts,
  family = quasibinomial()
)

# 3. Vérifier l'effectif
nobs(modele_public_assimile)

# 4. Examiner les odds ratios
or_public <- exp(coef(modele_public_assimile))

print(
  round(
    or_public[grepl("^profil", names(or_public))],
    3
  )
)


# ============================================
# ÉTAPE 24 - COMPARAISON DIRECTE
# ÉTAT VS COLLECTIVITÉS / HLM / HÔPITAUX
# ============================================

library(survey)

# 1. Conserver les personnes des deux catégories
plan_public <- subset(
  plan_statuts,
  statut_code %in% c("1", "2")
)

# 2. Modèle :
# 1 = salarié de l'État
# 0 = collectivité / HLM / hôpital public

modele_etat_parmi_public <- svyglm(
  formula = update(
    formula(modele4_age2),
    est_etat ~ .
  ),
  design = plan_public,
  family = quasibinomial()
)

# 3. Vérifier l'effectif
nobs(modele_etat_parmi_public)

# 4. Extraire les OR des profils migratoires
or_etat_public <- exp(
  coef(modele_etat_parmi_public)
)

print(
  round(
    or_etat_public[
      grepl("^profil", names(or_etat_public))
    ],
    3
  )
)


# ============================================
# ÉTAPE 25 - PROBABILITÉS AJUSTÉES
# ÉTAT VS COLLECTIVITÉS / HLM / HÔPITAUX
# ============================================

# 1. Récupérer la population du modèle
base_public <- plan_public$variables

# 2. Identifier les trois profils migratoires
profils_public <- levels(base_public$profil)

# 3. Fonction de prédiction
calculer_proba_etat <- function(profil_choisi) {
  
  nouvelles_donnees <- base_public
  
  # Attribuer le même profil migratoire
  # à toutes les observations
  nouvelles_donnees$profil <- factor(
    rep(profil_choisi, nrow(nouvelles_donnees)),
    levels = profils_public
  )
  
  # Probabilités prédites
  predictions <- as.numeric(
    predict(
      modele_etat_parmi_public,
      newdata = nouvelles_donnees,
      type = "response"
    )
  )
  
  # Moyenne pondérée
  100 * weighted.mean(
    predictions,
    w = base_public$PFLIN,
    na.rm = TRUE
  )
}

# 4. Construire le tableau
tableau_proba_etat_public <- data.frame(
  profil_migratoire = profils_public,
  
  probabilite_etat = round(
    sapply(profils_public, calculer_proba_etat),
    2
  ),
  row.names = NULL
)

# 5. Afficher
print(tableau_proba_etat_public)


# ============================================
# ÉTAPE 26 - RÔLE DU DIPLÔME
# ÉTAT VS COLLECTIVITÉS / HLM / HÔPITAUX
# ============================================

# 1. Extraire les coefficients du modèle
coefficients_public <- coef(
  modele_etat_parmi_public
)

# 2. Identifier les coefficients du diplôme
variables_diplome <- grep(
  "^diplome_4",
  names(coefficients_public),
  value = TRUE
)

# 3. Calculer les odds ratios du diplôme
tableau_or_diplome_public <- data.frame(
  diplome = variables_diplome,
  
  odds_ratio = round(
    exp(coefficients_public[variables_diplome]),
    3
  ),
  
  row.names = NULL
)

# 4. Afficher les résultats
print(tableau_or_diplome_public)


# ============================================
# ÉTAPE 27 - DIPLÔME ET TYPE D'EMPLOYEUR PUBLIC
# ============================================

library(dplyr)

# 1. Préparer le tableau
tableau_diplome_public <- plan_public$variables %>%
  mutate(
    type_employeur = case_when(
      statut_code == "1" ~ "État",
      statut_code == "2" ~ "Collectivités / HLM / Hôpitaux"
    )
  ) %>%
  group_by(diplome_4, type_employeur) %>%
  summarise(
    effectif = n(),
    poids_total = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(diplome_4) %>%
  mutate(
    pourcentage = round(
      100 * poids_total / sum(poids_total),
      2
    )
  ) %>%
  ungroup()

# 2. Consulter localement les résultats
View(tableau_diplome_public)


# ============================================
# ÉTAPE 28 - EXPORTATION DU TABLEAU
# DIPLÔME ET TYPE D'EMPLOYEUR PUBLIC
# ============================================

library(dplyr)
library(writexl)

# 1. Préparer une version partageable
# Sans effectifs ni poids individuels
# Masquage conservateur si une cellule a moins
# de 10 observations (seuil à confirmer)

tableau_diplome_export <- tableau_diplome_public %>%
  group_by(diplome_4) %>%
  mutate(
    masquer = any(effectif < 10),
    pourcentage = if_else(
      masquer,
      NA_real_,
      pourcentage
    )
  ) %>%
  ungroup() %>%
  select(
    diplome_4,
    type_employeur,
    pourcentage
  )

# 2. Choisir le dossier d'exportation
dossier_export <- choose.dir(
  caption = "Choisir le dossier d'exportation"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 3. Définir le nom du fichier
fichier_export <- file.path(
  dossier_export,
  "MFV2_Diplome_Type_Employeur_Public.xlsx"
)

# 4. Exporter au format Excel
write_xlsx(
  tableau_diplome_export,
  path = fichier_export
)

# 5. Vérifier l'exportation
file.exists(fichier_export)

# 6. Afficher l'emplacement
print(fichier_export)


# ============================================
# ÉTAPE 29 - CATÉGORIES SOCIOPROFESSIONNELLES
# SELON LE TYPE D'EMPLOYEUR PUBLIC
# ============================================

library(dplyr)
library(haven)

# 1. Préparer les variables

mfv_cs8_public <- plan_public$variables %>%
  mutate(
    cs8_code = trimws(
      as.character(haven::zap_labels(CS8))
    ),
    
    type_employeur = case_when(
      statut_code == "1" ~ "État",
      statut_code == "2" ~ "Collectivités / HLM / Hôpitaux"
    ),
    
    categorie_cs8 = case_when(
      cs8_code == "0" ~ "Inconnue",
      cs8_code == "1" ~ "Agriculteurs exploitants",
      cs8_code == "2" ~ "Artisans / commerçants / chefs entreprise",
      cs8_code == "3" ~ "Cadres et professions intellectuelles",
      cs8_code == "4" ~ "Professions intermédiaires",
      cs8_code == "5" ~ "Employés",
      cs8_code == "6" ~ "Ouvriers",
      cs8_code == "7" ~ "Retraités",
      cs8_code == "8" ~ "Autres personnes sans activité",
      TRUE ~ "Non renseigné"
    )
  )

# 2. Vérifier la qualité de CS8

controle_cs8 <- mfv_cs8_public %>%
  group_by(type_employeur) %>%
  summarise(
    total = n(),
    
    cs8_utilisable = sum(
      cs8_code %in% as.character(1:6)
    ),
    
    cs8_non_utilisable = sum(
      !(cs8_code %in% as.character(1:6))
    ),
    
    pct_utilisable = round(
      100 * cs8_utilisable / total,
      2
    ),
    
    .groups = "drop"
  )

print(controle_cs8)


# ============================================
# ÉTAPE 30 - RÉPARTITION PONDÉRÉE DES PCS
# SELON LE TYPE D'EMPLOYEUR PUBLIC
# ============================================

tableau_cs8_public <- mfv_cs8_public %>%
  group_by(type_employeur, categorie_cs8) %>%
  summarise(
    effectif = n(),
    poids_total = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(type_employeur) %>%
  mutate(
    pourcentage = round(
      100 * poids_total / sum(poids_total),
      2
    )
  ) %>%
  ungroup()

# Consulter le tableau dans RStudio
View(tableau_cs8_public)


# ============================================
# ÉTAPE 31 - EXPORTATION DU TABLEAU CS8
# SELON LE TYPE D'EMPLOYEUR PUBLIC
# ============================================

library(writexl)

# 1. Choisir le dossier d'exportation
dossier_export <- choose.dir(
  caption = "Choisir le dossier d'exportation"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Définir le nom du fichier Excel
fichier_export <- file.path(
  dossier_export,
  "MFV2_CS8_Employeurs_Publics.xlsx"
)

# 3. Exporter le tableau complet
write_xlsx(
  tableau_cs8_public,
  path = fichier_export
)

# 4. Vérifier que le fichier existe
file.exists(fichier_export)

# 5. Afficher son emplacement
print(fichier_export)


# ============================================
# ÉTAPE 32 - VÉRIFICATION DE CLASSIF_rec
# SELON LE TYPE D'EMPLOYEUR PUBLIC
# ============================================

library(dplyr)
library(haven)

mfv_classif_public <- plan_public$variables %>%
  mutate(
    classif_code = trimws(
      as.character(
        haven::zap_labels(CLASSIF_rec)
      )
    ),
    
    type_employeur = case_when(
      statut_code == "1" ~ "État",
      statut_code == "2" ~ "Collectivités / HLM / Hôpitaux"
    )
  )

# Vérifier les codes disponibles
controle_classif <- mfv_classif_public %>%
  group_by(type_employeur) %>%
  summarise(
    total = n(),
    
    classif_valide = sum(
      classif_code %in% as.character(1:8)
    ),
    
    classif_non_valide = sum(
      !(classif_code %in% as.character(1:8))
    ),
    
    pct_valide = round(
      100 * classif_valide / total,
      2
    ),
    
    .groups = "drop"
  )

print(controle_classif)


# ============================================
# ÉTAPE 33 - EXAMINER LES CODES
# NON VALIDES DE CLASSIF_rec
# ============================================

library(dplyr)

# 1. Identifier les différents types de valeurs

controle_classif_detail <- mfv_classif_public %>%
  mutate(
    situation_classif = case_when(
      classif_code %in% as.character(1:8) ~
        "Code valide (1 à 8)",
      
      is.na(classif_code) ~
        "Valeur manquante (NA)",
      
      classif_code == "" ~
        "Chaîne vide",
      
      TRUE ~
        "Autre code"
    )
  ) %>%
  group_by(
    type_employeur,
    situation_classif
  ) %>%
  summarise(
    effectif = n(),
    .groups = "drop"
  )

# 2. Afficher le résultat
print(controle_classif_detail, n = Inf)


# ============================================
# ÉTAPE 34 - RECHERCHER LES VARIABLES
# DE CLASSIFICATION PROFESSIONNELLE
# ============================================

# 1. Rechercher toutes les variables CLASSIF
variables_classif <- grep(
  "^CLASSIF",
  names(ego),
  value = TRUE
)

print(variables_classif)

# 2. Vérifier si CLASSIF originale existe
"CLASSIF" %in% names(ego)

# 3. Si elle existe, afficher ses modalités
if ("CLASSIF" %in% names(ego)) {
  
  print(attr(ego$CLASSIF, "label"))
  
  print(attr(ego$CLASSIF, "labels"))
  
}


# ============================================
# ÉTAPE 35 - COMPARER CLASSIF_rec
# ET CLASSIF_ACTANT
# ============================================

library(dplyr)
library(haven)

# 1. Préparer les deux variables
mfv_classif_comparaison <- mfv_classif_public %>%
  mutate(
    rec_code = trimws(
      as.character(haven::zap_labels(CLASSIF_rec))
    ),
    
    actant_code = trimws(
      as.character(haven::zap_labels(CLASSIF_ACTANT))
    ),
    
    rec_valide = rec_code %in% as.character(1:8),
    
    actant_valide = actant_code %in%
      as.character(1:10)
  )

# 2. Comparer leur disponibilité
controle_classif_actant <- mfv_classif_comparaison %>%
  group_by(type_employeur) %>%
  summarise(
    total = n(),
    
    classif_rec_valide = sum(rec_valide),
    
    classif_actant_valide = sum(actant_valide),
    
    rec_absente_actant_present =
      sum(!rec_valide & actant_valide),
    
    deux_absentes =
      sum(!rec_valide & !actant_valide),
    
    .groups = "drop"
  )

# 3. Afficher les résultats
print(controle_classif_actant, width = Inf)


# ============================================
# ÉTAPE 36 - PCS SELON LE PROFIL MIGRATOIRE
# ET LE TYPE D'EMPLOYEUR PUBLIC
# ============================================

library(dplyr)
library(haven)
library(tidyr)

# 1. Reprendre notre population de 1 029 personnes
mfv_cs8_migration <- mfv_cs8_public %>%
  mutate(
    profil_migratoire = as.character(
      haven::as_factor(TYPOMIG6)
    )
  )

# 2. Calculer les répartitions pondérées
tableau_cs8_migration <- mfv_cs8_migration %>%
  group_by(
    type_employeur,
    profil_migratoire,
    categorie_cs8
  ) %>%
  summarise(
    effectif = n(),
    poids_total = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    type_employeur,
    profil_migratoire
  ) %>%
  mutate(
    pourcentage = round(
      100 * poids_total / sum(poids_total),
      2
    )
  ) %>%
  ungroup()

# 3. Consulter le tableau complet localement
View(tableau_cs8_migration)


# ============================================
# ÉTAPE 37 - EXPORTATION DU TABLEAU
# PCS, PROFIL MIGRATOIRE ET EMPLOYEUR PUBLIC
# ============================================

library(writexl)

# 1. Choisir le dossier d'exportation
dossier_export <- choose.dir(
  caption = "Choisir le dossier d'exportation"
)

if (is.na(dossier_export)) {
  stop("Aucun dossier sélectionné.")
}

# 2. Définir le nom du fichier Excel
fichier_export <- file.path(
  dossier_export,
  "MFV2_CS8_Profil_Migratoire_Employeurs_Publics.xlsx"
)

# 3. Exporter le tableau complet
write_xlsx(
  tableau_cs8_migration,
  path = fichier_export
)

# 4. Vérifier l'exportation
file.exists(fichier_export)

# 5. Afficher l'emplacement
print(fichier_export)


# ============================================
# ÉTAPE 38 - RÉGRESSION LOGISTIQUE
# CADRES ET PROFESSIONS INTERMÉDIAIRES
# ============================================

library(survey)
library(haven)

# 1. Créer notre indicateur de position
plan_position <- update(
  plan_public,
  
  cs8_code = as.character(
    haven::zap_labels(CS8)
  ),
  
  type_employeur = factor(
    ifelse(
      statut_code == "1",
      "État",
      "Collectivités / HLM / Hôpitaux"
    ),
    levels = c(
      "Collectivités / HLM / Hôpitaux",
      "État"
    )
  )
)

# 2. Variable binaire :
# 1 = cadre ou profession intermédiaire
# 0 = autre PCS valide

plan_position <- update(
  plan_position,
  
  position_superieure = as.integer(
    cs8_code %in% c("3", "4")
  )
)

# 3. Estimer le modèle
modele_position <- svyglm(
  position_superieure ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur,
  
  design = plan_position,
  family = quasibinomial()
)

# 4. Vérifier l'effectif
nobs(modele_position)

# 5. Examiner les OR du profil migratoire
# et du type d'employeur

or_position <- exp(coef(modele_position))

print(
  round(
    or_position[
      grepl(
        "^profil|^type_employeur",
        names(or_position)
      )
    ],
    3
  )
)


# ============================================
# ÉTAPE 39 - COMPARER LES MODÈLES
# AVEC ET SANS DIPLÔME
# ============================================

library(survey)
library(dplyr)

# 1. Modèle sans diplôme
modele_position_sans_diplome <- svyglm(
  position_superieure ~
    profil +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur,
  
  design = plan_position,
  family = quasibinomial()
)

# 2. Vérifier les effectifs
nobs(modele_position_sans_diplome)
nobs(modele_position)

# 3. Extraire les coefficients des profils
variables_profil <- grep(
  "^profil",
  names(coef(modele_position)),
  value = TRUE
)

# 4. Comparer les odds ratios
tableau_comparaison_diplome <- data.frame(
  profil = variables_profil,
  
  OR_sans_diplome = round(
    exp(coef(modele_position_sans_diplome)[
      variables_profil
    ]),
    3
  ),
  
  OR_avec_diplome = round(
    exp(coef(modele_position)[
      variables_profil
    ]),
    3
  ),
  
  row.names = NULL
)

print(tableau_comparaison_diplome)


# ============================================
# ÉTAPE 40 - DIPLÔME SELON LE PROFIL MIGRATOIRE
# PARMI LES SALARIÉS PUBLICS OU ASSIMILÉS
# ============================================

library(dplyr)

# 1. Répartition pondérée des diplômes
tableau_diplome_profil_public <- plan_public$variables %>%
  group_by(profil, diplome_4) %>%
  summarise(
    effectif = n(),
    poids_total = sum(PFLIN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(profil) %>%
  mutate(
    pourcentage = round(
      100 * poids_total / sum(poids_total),
      2
    )
  ) %>%
  ungroup()

# 2. Consulter le tableau détaillé localement
View(tableau_diplome_profil_public)



# ============================================
# ÉTAPE 41 - CORRECTION VERSION PARTAGEABLE
# ============================================

library(dplyr)
library(writexl)

# 1. Préparer le tableau pour le partage

tableau_diplome_partage <- tableau_diplome_profil_public %>%
  group_by(profil) %>%
  mutate(
    pourcentage_diffusable = if (
      any(effectif < 10, na.rm = TRUE)
    ) {
      rep(NA_real_, n())
    } else {
      pourcentage
    }
  ) %>%
  ungroup() %>%
  select(
    profil,
    diplome_4,
    pourcentage_diffusable
  )

# 2. Exporter le fichier Excel

fichier_partage <- file.path(
  dossier_export,
  "MFV2_Diplome_Profil_Public_Partage.xlsx"
)

write_xlsx(
  tableau_diplome_partage,
  path = fichier_partage
)

# 3. Vérifier l'exportation

file.exists(fichier_partage)

print(fichier_partage)


# ============================================
# ÉTAPE 42 - PROBABILITÉS AJUSTÉES
# AVEC ET SANS DIPLÔME
# ============================================

# 1. Récupérer la population commune
base_position <- plan_position$variables

# Vérifier que les deux modèles utilisent
# exactement le même effectif
stopifnot(
  nobs(modele_position_sans_diplome) ==
    nrow(base_position),
  
  nobs(modele_position) ==
    nrow(base_position)
)

# 2. Identifier les trois profils
profils_position <- levels(base_position$profil)

# 3. Fonction de standardisation marginale
calculer_proba_position <- function(profil_choisi,
                                    modele) {
  
  nouvelles_donnees <- base_position
  
  nouvelles_donnees$profil <- factor(
    rep(profil_choisi, nrow(nouvelles_donnees)),
    levels = profils_position
  )
  
  # Probabilités prédites par le modèle
  predictions <- as.numeric(
    predict(
      modele,
      newdata = nouvelles_donnees,
      type = "response"
    )
  )
  
  # Moyenne pondérée par PFLIN
  100 * weighted.mean(
    predictions,
    w = base_position$PFLIN
  )
}

# 4. Construire le tableau comparatif
tableau_proba_position <- data.frame(
  
  profil_migratoire = profils_position,
  
  sans_diplome = sapply(
    profils_position,
    calculer_proba_position,
    modele = modele_position_sans_diplome
  ),
  
  avec_diplome = sapply(
    profils_position,
    calculer_proba_position,
    modele = modele_position
  ),
  
  row.names = NULL
)

# 5. Calculer la variation
tableau_proba_position$variation_points <-
  tableau_proba_position$avec_diplome -
  tableau_proba_position$sans_diplome

# 6. Arrondir les résultats
tableau_proba_position[-1] <- round(
  tableau_proba_position[-1],
  2
)

# 7. Afficher
print(tableau_proba_position)


# ============================================
# ÉTAPE 43 - INTERACTION ENTRE
# PROFIL MIGRATOIRE ET TYPE D'EMPLOYEUR
# ============================================

library(survey)

# 1. Contrôler localement les effectifs
# des six combinaisons

controle_interaction <- with(
  plan_position$variables,
  table(profil, type_employeur)
)

print(controle_interaction)

# 2. Estimer le modèle avec interaction

modele_position_interaction <- svyglm(
  position_superieure ~
    profil * type_employeur +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_position,
  family = quasibinomial()
)

# 3. Vérifier l'effectif
nobs(modele_position_interaction)

# 4. Examiner les coefficients d'interaction
coeff_interaction <- coef(
  modele_position_interaction
)

tableau_interaction <- data.frame(
  variable = names(coeff_interaction),
  odds_ratio = round(
    exp(coeff_interaction),
    3
  ),
  row.names = NULL
)

# Afficher seulement les interactions
print(
  subset(
    tableau_interaction,
    grepl(":", variable)
  )
)


# ============================================
# ÉTAPE 44 - ODDS RATIOS PAR TYPE D'EMPLOYEUR
# À PARTIR DU MODÈLE AVEC INTERACTION
# ============================================

# 1. Récupérer les coefficients du modèle
b <- coef(modele_position_interaction)

# 2. Coefficients des profils migratoires
b_jamais <- b["profilJamais quitté"]
b_retour <- b["profilNatifs de retour"]

# 3. Coefficients des interactions
i_jamais <- b[
  "profilJamais quitté:type_employeurÉtat"
]

i_retour <- b[
  "profilNatifs de retour:type_employeurÉtat"
]

# 4. Calculer les OR selon l'employeur
tableau_or_interaction <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_collectivites = round(
    exp(c(b_jamais, b_retour)),
    3
  ),
  
  OR_etat = round(
    exp(c(
      b_jamais + i_jamais,
      b_retour + i_retour
    )),
    3
  )
)

# 5. Afficher les résultats
print(tableau_or_interaction)


# ============================================
# ÉTAPE 45 - PROBABILITÉS AJUSTÉES
# PROFIL MIGRATOIRE X TYPE D'EMPLOYEUR
# ============================================

# 1. Récupérer notre population
base_interaction <- plan_position$variables

# 2. Identifier les catégories
profils <- levels(base_interaction$profil)

employeurs <- levels(
  base_interaction$type_employeur
)

# 3. Créer les six combinaisons
grille_prediction <- expand.grid(
  profil = profils,
  type_employeur = employeurs,
  stringsAsFactors = FALSE
)

# 4. Calculer les probabilités ajustées
grille_prediction$probabilite_pct <- sapply(
  seq_len(nrow(grille_prediction)),
  
  function(i) {
    
    nouvelles_donnees <- base_interaction
    
    # Fixer le profil migratoire
    nouvelles_donnees$profil <- factor(
      rep(
        grille_prediction$profil[i],
        nrow(nouvelles_donnees)
      ),
      levels = profils
    )
    
    # Fixer le type d'employeur
    nouvelles_donnees$type_employeur <- factor(
      rep(
        grille_prediction$type_employeur[i],
        nrow(nouvelles_donnees)
      ),
      levels = employeurs
    )
    
    # Probabilités prédites
    predictions <- as.numeric(
      predict(
        modele_position_interaction,
        newdata = nouvelles_donnees,
        type = "response"
      )
    )
    
    # Standardisation marginale pondérée
    100 * weighted.mean(
      predictions,
      w = nouvelles_donnees$PFLIN,
      na.rm = TRUE
    )
  }
)

# 5. Arrondir les résultats
grille_prediction$probabilite_pct <- round(
  grille_prediction$probabilite_pct,
  2
)

# 6. Afficher le tableau
print(grille_prediction)


# ============================================
# ÉTAPE 46 - ÉCARTS DE PROBABILITÉS AJUSTÉES
# SELON LE TYPE D'EMPLOYEUR
# ============================================

library(dplyr)

# 1. Comparer les trois profils
# dans chaque catégorie d'employeur

tableau_ecarts_position <- grille_prediction %>%
  group_by(type_employeur) %>%
  summarise(
    sejours_courts = probabilite_pct[
      profil == "Séjours courts"
    ],
    
    jamais_quitte = probabilite_pct[
      profil == "Jamais quitté"
    ],
    
    natifs_retour = probabilite_pct[
      profil == "Natifs de retour"
    ],
    
    .groups = "drop"
  ) %>%
  mutate(
    ecart_retour_courts = round(
      natifs_retour - sejours_courts, 2
    ),
    
    ecart_jamais_courts = round(
      jamais_quitte - sejours_courts, 2
    )
  )

print(tableau_ecarts_position, width = Inf)

# 2. Différence entre les écarts
# retour - séjours courts selon l'employeur

difference_ecarts <- with(
  tableau_ecarts_position,
  
  ecart_retour_courts[
    type_employeur == "État"
  ] -
    ecart_retour_courts[
      type_employeur ==
        "Collectivités / HLM / Hôpitaux"
    ]
)

print(round(difference_ecarts, 2))


# ============================================
# ÉTAPE 47 - CONTRÔLE AVANT MODÈLE
# SUR LES CADRES UNIQUEMENT
# ============================================

library(survey)

# 1. Créer l'indicateur cadres
# 1 = cadre ou profession intellectuelle supérieure
# 0 = autre PCS valide

plan_cadres <- update(
  plan_position,
  
  est_cadre = as.integer(
    cs8_code == "3"
  )
)

# 2. Contrôler localement les effectifs
# par profil migratoire et employeur

controle_cadres <- with(
  plan_cadres$variables,
  
  table(
    profil,
    type_employeur,
    est_cadre
  )
)

# Conserver ce tableau détaillé dans RStudio

# 3. Préparer un diagnostic agrégé
# sans communiquer les petites cellules

diagnostic_cadres <- data.frame(
  indicateur = c(
    "Nombre total d'observations",
    "Nombre de combinaisons sans cadre",
    "Combinaisons avec moins de 10 cadres"
  ),
  
  resultat = c(
    nrow(plan_cadres$variables),
    sum(controle_cadres[, , "1"] == 0),
    sum(controle_cadres[, , "1"] < 10)
  )
)

print(diagnostic_cadres)


# ============================================
# ÉTAPE 48 - ROBUSTESSE
# CADRES UNIQUEMENT
# ============================================

library(survey)
library(dplyr)

# 1. Estimer le modèle sans interaction

modele_cadres <- svyglm(
  est_cadre ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur,
  
  design = plan_cadres,
  family = quasibinomial()
)

# 2. Vérifier le nombre d'observations

nobs(modele_cadres)

# 3. Vérifier la convergence du modèle

modele_cadres$converged

# 4. Extraire les odds ratios

or_cadres <- exp(coef(modele_cadres))

variables_cibles <- grep(
  "^(profil|type_employeur)",
  names(or_cadres),
  value = TRUE
)

tableau_or_cadres <- data.frame(
  variable = variables_cibles,
  
  OR_cadres = round(
    as.numeric(or_cadres[variables_cibles]),
    3
  )
)

# 5. Afficher les résultats

print(tableau_or_cadres)


# ============================================
# ÉTAPE 49 - PROBABILITÉS AJUSTÉES
# D'ÊTRE CADRE SELON LE PROFIL MIGRATOIRE
# ============================================

library(dplyr)

# 1. Récupérer la population étudiée
base_cadres <- plan_cadres$variables

# 2. Identifier les profils migratoires
profils_cadres <- levels(base_cadres$profil)

# 3. Fonction de prédiction standardisée
calculer_proba_cadre <- function(profil_choisi) {
  
  nouvelles_donnees <- base_cadres
  
  # Attribuer le même profil migratoire
  # à toute la population de référence
  
  nouvelles_donnees$profil <- factor(
    rep(
      profil_choisi,
      nrow(nouvelles_donnees)
    ),
    levels = profils_cadres
  )
  
  # Calculer les probabilités prédites
  predictions <- as.numeric(
    predict(
      modele_cadres,
      newdata = nouvelles_donnees,
      type = "response"
    )
  )
  
  # Moyenne pondérée par PFLIN
  100 * weighted.mean(
    predictions,
    w = nouvelles_donnees$PFLIN
  )
}

# 4. Construire le tableau
tableau_proba_cadres <- data.frame(
  profil = profils_cadres,
  
  probabilite_cadre = sapply(
    profils_cadres,
    calculer_proba_cadre
  ),
  
  row.names = NULL
)

# 5. Arrondir les probabilités
tableau_proba_cadres$probabilite_cadre <-
  round(
    tableau_proba_cadres$probabilite_cadre,
    2
  )

# 6. Afficher les résultats
print(tableau_proba_cadres)

# 7. Calculer les écarts
reference_courts <- tableau_proba_cadres$probabilite_cadre[
  tableau_proba_cadres$profil == "Séjours courts"
]

tableau_proba_cadres$ecart_courts <- round(
  tableau_proba_cadres$probabilite_cadre -
    reference_courts,
  2
)

print(tableau_proba_cadres)


# ============================================
# ÉTAPE 50 - RÉGRESSION
# PROFESSIONS INTERMÉDIAIRES UNIQUEMENT
# ============================================

library(survey)
library(dplyr)

# 1. Créer la variable dépendante
# CS8 = 4 : professions intermédiaires

plan_intermediaires <- update(
  plan_position,
  
  est_intermediaire = as.integer(
    cs8_code == "4"
  )
)

# 2. Contrôler les effectifs en interne

controle_intermediaires <- with(
  plan_intermediaires$variables,
  
  table(
    profil,
    type_employeur,
    est_intermediaire
  )
)

# Nombre de combinaisons comportant
# moins de 10 professions intermédiaires

sum(
  controle_intermediaires[, , "1"] < 10
)

# 3. Estimer la régression logistique
# sans interaction

modele_intermediaires <- svyglm(
  est_intermediaire ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur,
  
  design = plan_intermediaires,
  family = quasibinomial()
)

# 4. Vérifier l'effectif

nobs(modele_intermediaires)

# 5. Vérifier la convergence

modele_intermediaires$converged

# 6. Extraire les odds ratios

or_intermediaires <- exp(
  coef(modele_intermediaires)
)

variables_cibles <- grep(
  "^(profil|type_employeur)",
  names(or_intermediaires),
  value = TRUE
)

tableau_or_intermediaires <- data.frame(
  variable = variables_cibles,
  
  OR_intermediaires = round(
    as.numeric(
      or_intermediaires[variables_cibles]
    ),
    3
  )
)

# 7. Afficher le tableau

print(tableau_or_intermediaires)


# ============================================
# ÉTAPE 51 - PROBABILITÉS AJUSTÉES
# PROFESSIONS INTERMÉDIAIRES UNIQUEMENT
# ============================================

library(dplyr)

# 1. Récupérer la population
base_intermediaires <- plan_intermediaires$variables

# 2. Identifier les profils
profils_intermediaires <- levels(
  base_intermediaires$profil
)

# 3. Fonction de standardisation marginale

calculer_proba_intermediaire <- function(profil_choisi) {
  
  nouvelles_donnees <- base_intermediaires
  
  # Fixer le parcours migratoire
  nouvelles_donnees$profil <- factor(
    rep(
      profil_choisi,
      nrow(nouvelles_donnees)
    ),
    levels = profils_intermediaires
  )
  
  # Calculer les probabilités prédites
  predictions <- as.numeric(
    predict(
      modele_intermediaires,
      newdata = nouvelles_donnees,
      type = "response"
    )
  )
  
  # Calculer la moyenne pondérée
  100 * weighted.mean(
    predictions,
    w = nouvelles_donnees$PFLIN
  )
}

# 4. Construire le tableau

tableau_proba_intermediaires <- data.frame(
  profil = profils_intermediaires,
  
  probabilite_intermediaire = sapply(
    profils_intermediaires,
    calculer_proba_intermediaire
  ),
  
  row.names = NULL
)

# 5. Calculer les écarts avant arrondi

reference_courts <- tableau_proba_intermediaires$
  probabilite_intermediaire[
    tableau_proba_intermediaires$profil ==
      "Séjours courts"
  ]

tableau_proba_intermediaires$ecart_courts <-
  tableau_proba_intermediaires$
  probabilite_intermediaire - reference_courts

# 6. Arrondir les résultats
tableau_proba_intermediaires[
  c("probabilite_intermediaire", "ecart_courts")
] <- round(
  tableau_proba_intermediaires[
    c("probabilite_intermediaire", "ecart_courts")
  ],
  2
)

# 7. Afficher
print(tableau_proba_intermediaires)


# ============================================
# ÉTAPE 52 - SYNTHÈSE DES PROBABILITÉS
# AJUSTÉES PAR POSITION PROFESSIONNELLE
# ============================================

library(dplyr)
library(writexl)

# 1. Rassembler les trois modèles

tableau_synthese_positions <- tableau_proba_cadres %>%
  select(
    profil,
    probabilite_cadre
  ) %>%
  left_join(
    tableau_proba_intermediaires %>%
      select(
        profil,
        probabilite_intermediaire
      ),
    by = "profil"
  ) %>%
  left_join(
    tableau_proba_position %>%
      select(
        profil = profil_migratoire,
        probabilite_cadres_intermediaires = avec_diplome
      ),
    by = "profil"
  )

# 2. Renommer les colonnes

names(tableau_synthese_positions) <- c(
  "Profil migratoire",
  "Cadres (%)",
  "Professions intermédiaires (%)",
  "Cadres et intermédiaires (%)"
)

# 3. Afficher le tableau

print(tableau_synthese_positions)

# 4. Exporter dans le dossier sécurisé
# déjà utilisé pour les tableaux précédents

fichier_synthese <- file.path(
  dossier_export,
  "MFV2_Synthese_Positions_Professionnelles.xlsx"
)

write_xlsx(
  tableau_synthese_positions,
  path = fichier_synthese
)

file.exists(fichier_synthese)


# ============================================
# ÉTAPE 53 - RÉGRESSION MULTINOMIALE
# POSITIONS SOCIOPROFESSIONNELLES
# ============================================

library(dplyr)
library(nnet)

# 1. Préparer la population

base_multinom <- plan_position$variables %>%
  mutate(
    
    # Trois catégories professionnelles
    position_3 = case_when(
      cs8_code == "3" ~ "Cadres",
      cs8_code == "4" ~ "Professions intermédiaires",
      cs8_code %in% c("1", "2", "5", "6") ~ "Autres PCS",
      TRUE ~ NA_character_
    ),
    
    # Catégorie de référence : autres PCS
    position_3 = factor(
      position_3,
      levels = c(
        "Autres PCS",
        "Professions intermédiaires",
        "Cadres"
      )
    ),
    
    # Rééchelonner les variables d'âge
    # pour faciliter l'optimisation numérique
    age_c20 = age_c / 20,
    age_c2_400 = age_c2 / 400,
    
    # Normaliser les poids sans changer
    # leur importance relative
    poids_modele = PFLIN / mean(PFLIN)
  )

# 2. Vérifications préalables

stopifnot(
  nrow(base_multinom) == 1029,
  !anyNA(base_multinom$position_3),
  all(is.finite(base_multinom$poids_modele)),
  all(base_multinom$poids_modele > 0)
)

# 3. Modèle multinomial sans interaction

modele_multinom <- nnet::multinom(
  position_3 ~
    profil +
    diplome_4 +
    age_c20 +
    age_c2_400 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur,
  
  data = base_multinom,
  weights = poids_modele,
  
  trace = FALSE,
  maxit = 500,
  MaxNWts = 10000
)

# 4. Vérifier les observations utilisées

nrow(fitted(modele_multinom))

# 5. Vérifier la convergence
# 0 = convergence obtenue
# 1 = limite d'itérations atteinte

modele_multinom$convergence

# 6. Vérifier les probabilités prédites

predictions_multinom <- as.matrix(
  predict(
    modele_multinom,
    type = "probs"
  )
)

# La somme des trois probabilités
# doit être égale à 1 pour chaque individu

range(rowSums(predictions_multinom))

# 7. Examiner les catégories du modèle

colnames(predictions_multinom)


# ============================================
# ÉTAPE 54 - PROBABILITÉS AJUSTÉES
# MODÈLE MULTINOMIAL
# ============================================

library(dplyr)

# 1. Identifier les profils migratoires

profils_multinom <- levels(
  base_multinom$profil
)

# 2. Identifier les catégories professionnelles

categories_multinom <- levels(
  base_multinom$position_3
)

# 3. Fonction de standardisation marginale

calculer_proba_multinom <- function(profil_choisi) {
  
  nouvelles_donnees <- base_multinom
  
  # Attribuer le même profil migratoire
  # à toutes les observations
  
  nouvelles_donnees$profil <- factor(
    rep(
      profil_choisi,
      nrow(nouvelles_donnees)
    ),
    levels = profils_multinom
  )
  
  # Prédire les trois probabilités
  predictions <- as.matrix(
    predict(
      modele_multinom,
      newdata = nouvelles_donnees,
      type = "probs"
    )
  )
  
  # Standardisation marginale pondérée
  probabilites <- sapply(
    categories_multinom,
    
    function(categorie) {
      
      100 * weighted.mean(
        predictions[, categorie],
        w = nouvelles_donnees$PFLIN
      )
    }
  )
  
  # Construire une ligne de résultats
  data.frame(
    profil = profil_choisi,
    t(probabilites),
    check.names = FALSE,
    row.names = NULL
  )
}

# 4. Calculer les résultats pour les 3 profils

tableau_multinom_brut <- bind_rows(
  lapply(
    profils_multinom,
    calculer_proba_multinom
  )
)

# 5. Vérifier que chaque ligne totalise 100 %

somme_probabilites <- rowSums(
  tableau_multinom_brut[, categories_multinom]
)

print(somme_probabilites)

stopifnot(
  all(abs(somme_probabilites - 100) < 1e-6)
)

# 6. Arrondir pour la présentation

tableau_multinom <- tableau_multinom_brut %>%
  mutate(
    across(
      all_of(categories_multinom),
      ~ round(.x, 2)
    )
  )

# Affichage classique
print(tableau_multinom)

# Affichage complet avec tibble
print(
  tibble::as_tibble(tableau_multinom),
  width = Inf
)

# Ouvrir le tableau dans RStudio
View(tableau_multinom)


# ============================================
# ÉTAPE 55 - ÉCARTS DE PROBABILITÉS
# MODÈLE MULTINOMIAL
# ============================================

# 1. Récupérer les probabilités de référence
# Séjours courts

reference_multinom <- unlist(
  tableau_multinom_brut[
    tableau_multinom_brut$profil == "Séjours courts",
    categories_multinom,
    drop = FALSE
  ],
  use.names = FALSE
)

# 2. Construire le tableau des écarts

tableau_ecarts_multinom <- tableau_multinom_brut

tableau_ecarts_multinom[, categories_multinom] <-
  round(
    sweep(
      as.matrix(
        tableau_multinom_brut[
          , categories_multinom,
          drop = FALSE
        ]
      ),
      2,
      reference_multinom,
      FUN = "-"
    ),
    2
  )

# 3. Afficher les résultats

print(tableau_ecarts_multinom)

# 4. Visualiser dans RStudio

View(tableau_ecarts_multinom)


# ============================================
# ÉTAPE 56 - DIAGNOSTIC DES EFFECTIFS
# MODÈLE MULTINOMIAL
# ============================================

# 1. Tableau d'effectifs interne
# Ne pas exporter ni diffuser

controle_multinom <- with(
  base_multinom,
  table(profil, position_3)
)

# 2. Diagnostic agrégé

diagnostic_multinom <- data.frame(
  indicateur = c(
    "Nombre total d'observations",
    "Nombre de combinaisons profil x PCS",
    "Combinaisons sans observation",
    "Combinaisons avec moins de 10 observations"
  ),
  
  resultat = c(
    nrow(base_multinom),
    length(controle_multinom),
    sum(controle_multinom == 0),
    sum(controle_multinom < 10)
  )
)

# 3. Afficher uniquement le diagnostic

print(diagnostic_multinom)

