
# ============================================
# RÉGRESSION LOGISTIQUE - EMPLOI
# Enquête MFV2 (2020-2021)
# Population : natifs âgés de 25 à 64 ans
# ============================================

library(dplyr)
library(haven)

# Vérifier les variables nécessaires

variables_modele <- c(
  "en_emploi",
  "profil",
  "territoire",
  "diplome_4",
  "AGE",
  "SEXE",
  "PCS_DOMIPAR_ALT",
  "PFLIN"
)

setdiff(
  variables_modele,
  names(mfv_croisement_4)
)


# ============================================
# PRÉPARATION DES VARIABLES DU MODÈLE
# ============================================

mfv_regression <- mfv_croisement_4 %>%
  mutate(
    
    # Profil migratoire
    # Référence : séjours courts
    profil = relevel(
      factor(profil),
      ref = "Séjours courts"
    ),
    
    # Territoire
    # Référence : Guadeloupe
    territoire = relevel(
      factor(territoire),
      ref = "Guadeloupe"
    ),
    
    # Niveau de diplôme
    diplome_4 = relevel(
      factor(diplome_4),
      ref = "Aucun diplôme / brevet"
    ),
    
    # Âge
    age = as.numeric(AGE),
    
    # Sexe
    sexe_code = as.numeric(
      haven::zap_labels(SEXE)
    ),
    
    sexe = factor(
      sexe_code,
      levels = c(1, 2),
      labels = c("Hommes", "Femmes")
    ),
    
    # Origine sociale
    origine_code = as.numeric(
      haven::zap_labels(PCS_DOMIPAR_ALT)
    ),
    
    origine_sociale = factor(
      origine_code,
      levels = 1:5,
      labels = c(
        "Dominante cadre",
        "Dominante intermédiaire",
        "Dominante employée/ouvrière",
        "Monoactif employé/ouvrier",
        "Ménage inactif"
      )
    )
  )

# Vérifier la population initiale
nrow(mfv_regression)


# ============================================
# CONTRÔLE DES VALEURS MANQUANTES
# ============================================

controle_regression <- mfv_regression %>%
  summarise(
    population_initiale = n(),
    
    manquants_emploi = sum(is.na(en_emploi)),
    
    manquants_profil = sum(is.na(profil)),
    
    manquants_diplome = sum(is.na(diplome_4)),
    
    manquants_age = sum(is.na(age)),
    
    manquants_sexe = sum(is.na(sexe)),
    
    manquants_origine = sum(is.na(origine_sociale)),
    
    manquants_territoire = sum(is.na(territoire))
  )

print(controle_regression)


# ============================================
# ÉTAPE 2 - ORIGINE SOCIALE
# ============================================

# Vérifier les codes d'origine sociale
# Les effectifs restent dans RStudio, en local

table(
  mfv_regression$origine_code,
  useNA = "ifany"
)



# ============================================
# ÉTAPE 2 - ÉCHANTILLON DE RÉGRESSION
# ============================================

# Conserver les individus avec des informations
# exploitables pour toutes les variables du modèle

mfv_regression_complete <- mfv_regression %>%
  filter(
    !is.na(en_emploi),
    !is.na(profil),
    !is.na(diplome_4),
    !is.na(age),
    !is.na(sexe),
    !is.na(origine_sociale),
    !is.na(territoire),
    !is.na(PFLIN),
    PFLIN > 0
  )

# Vérifier la taille de l'échantillon
nrow(mfv_regression_complete)

# Vérifier les valeurs manquantes
sum(
  !complete.cases(
    mfv_regression_complete %>%
      select(
        en_emploi,
        profil,
        diplome_4,
        age,
        sexe,
        origine_sociale,
        territoire,
        PFLIN
      )
  )
)


# ============================================
# ÉTAPE 3 - CONTRÔLE DES EXCLUSIONS
# Selon le profil migratoire
# ============================================

controle_exclusions <- mfv_regression %>%
  group_by(profil) %>%
  summarise(
    # Effectifs pour le contrôle interne
    effectif_total = n(),
    
    effectif_exclu = sum(is.na(origine_sociale)),
    
    # Pourcentage non pondéré
    pourcentage_exclu = round(
      100 * mean(is.na(origine_sociale)),
      1
    ),
    
    # Pourcentage pondéré avec PFLIN
    pourcentage_pondere = round(
      100 * sum(
        PFLIN[is.na(origine_sociale)]
      ) / sum(PFLIN),
      1
    ),
    
    .groups = "drop"
  )

# Consulter les résultats en local
print(controle_exclusions)


# ============================================
# ÉTAPE 4 - EXCLUSIONS PONDÉRÉES
# ============================================

controle_exclusions %>%
  select(
    profil,
    pourcentage_exclu,
    pourcentage_pondere
  ) %>%
  print(width = Inf)


# ============================================
# ÉTAPE 4 - VÉRIFICATION DU PLAN DE SONDAGE
# ============================================

# Rechercher les variables potentiellement liées
# au plan de sondage dans la base MFV2

variables_plan <- grep(
  "PFLIN|POID|POND|WEIGHT|STRAT|GRAP|CLUST|PSU|SOND",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

# Afficher uniquement les noms de variables
print(variables_plan)

# Vérifier si le package survey est installé
requireNamespace("survey", quietly = TRUE)


# ============================================
# ÉTAPE 5 - PRÉPARER LA RÉGRESSION PONDÉRÉE
# ============================================

# Installer survey uniquement si nécessaire
if (!requireNamespace("survey", quietly = TRUE)) {
  install.packages("survey")
}

# Charger le package
library(survey)

# Vérifier la version installée
packageVersion("survey")


# ============================================
# PLAN D'ANALYSE PROVISOIRE
# ============================================

plan_mfv_provisoire <- svydesign(
  ids = ~1,
  weights = ~PFLIN,
  data = mfv_regression_complete
)

# Vérifier que l'objet a été créé
class(plan_mfv_provisoire)


# ============================================
# ÉTAPE 6 - PREMIÈRE RÉGRESSION LOGISTIQUE
# Modèle 1 : profil migratoire uniquement
# ============================================

# Estimer la régression logistique pondérée

modele1 <- svyglm(
  en_emploi ~ profil,
  design = plan_mfv_provisoire,
  family = quasibinomial()
)

# Afficher les résultats du modèle
summary(modele1)


# ============================================
# MODÈLE 1 - ODDS RATIOS
# ============================================

# Calculer les odds ratios
odds_ratios_M1 <- exp(coef(modele1))

# Afficher les résultats
print(round(odds_ratios_M1, 3))


# ============================================
# ÉTAPE 7 - MODÈLE 2
# Profil migratoire + niveau de diplôme
# ============================================

modele2 <- svyglm(
  en_emploi ~ profil + diplome_4,
  design = plan_mfv_provisoire,
  family = quasibinomial()
)

# Afficher les résultats
summary(modele2)


# ============================================
# MODÈLE 2 - ODDS RATIOS
# ============================================

odds_ratios_M2 <- exp(coef(modele2))

print(round(odds_ratios_M2, 3))


# ============================================
# COMPARAISON M1 / M2
# ============================================

comparaison_M1_M2 <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_M1 = round(
    exp(coef(modele1)[2:3]), 3
  ),
  
  OR_M2 = round(
    exp(coef(modele2)[2:3]), 3
  )
)

print(comparaison_M1_M2)


# ============================================
# ÉTAPE 8 - MODÈLE 3
# Profil + diplôme + âge + sexe + territoire
# ============================================

# 1. Estimer le modèle 3

modele3 <- svyglm(
  en_emploi ~ profil +
    diplome_4 +
    age +
    sexe +
    territoire,
  design = plan_mfv_provisoire,
  family = quasibinomial()
)

# 2. Afficher les résultats
summary(modele3)


# ============================================
# MODÈLE 3 - ODDS RATIOS
# ============================================

odds_ratios_M3 <- exp(coef(modele3))

# Afficher les odds ratios
print(round(odds_ratios_M3, 3))


# ============================================
# ÉTAPE 9 - MODÈLE 4
# Ajout de l'origine sociale
# ============================================

modele4 <- svyglm(
  en_emploi ~ profil +
    diplome_4 +
    age +
    sexe +
    territoire +
    origine_sociale,
  design = plan_mfv_provisoire,
  family = quasibinomial()
)

# Afficher les résultats
summary(modele4)


# ============================================
# MODÈLE 4 - ODDS RATIOS
# ============================================

odds_ratios_M4 <- exp(coef(modele4))

print(round(odds_ratios_M4, 3))


# ============================================
# ÉTAPE 10 - COMPARAISON DES QUATRE MODÈLES
# ============================================

# 1. Rassembler les modèles

modeles <- list(
  M1 = modele1,
  M2 = modele2,
  M3 = modele3,
  M4 = modele4
)

# 2. Récupérer toutes les variables
# présentes dans les modèles

termes <- unique(
  unlist(
    lapply(modeles, function(m) names(coef(m)))
  )
)

# 3. Créer le tableau comparatif

comparaison_OR <- data.frame(
  variable = termes
)

for (nom in names(modeles)) {
  
  or <- exp(coef(modeles[[nom]]))
  
  comparaison_OR[[nom]] <- round(
    unname(or[match(termes, names(or))]),
    3
  )
}

# 4. Visualiser les résultats

View(comparaison_OR)


# Vérifier le nombre d'observations utilisées
sapply(modeles, nobs)

# Vérifier les dimensions du tableau
dim(comparaison_OR)



# ============================================
# ÉTAPE 11 - TABLEAU DE RÉGRESSION LISIBLE
# ============================================

library(dplyr)

# 1. Définir les intitulés des variables

etiquettes <- c(
  "profilJamais quitté" =
    "Profil : Jamais quitté",
  
  "profilNatifs de retour" =
    "Profil : Natifs de retour",
  
  "diplome_4CAP / BEP" =
    "Diplôme : CAP / BEP",
  
  "diplome_4Baccalauréat" =
    "Diplôme : Baccalauréat",
  
  "diplome_4Enseignement supérieur" =
    "Diplôme : Enseignement supérieur",
  
  "age" = "Âge (en années)",
  
  "sexeFemmes" = "Sexe : Femmes",
  
  "territoireMartinique" =
    "Territoire : Martinique",
  
  "territoireGuyane" =
    "Territoire : Guyane",
  
  "territoireLa Réunion" =
    "Territoire : La Réunion",
  
  "origine_socialeDominante intermédiaire" =
    "Origine sociale : Intermédiaire",
  
  "origine_socialeDominante employée/ouvrière" =
    "Origine sociale : Employée/ouvrière",
  
  "origine_socialeMonoactif employé/ouvrier" =
    "Origine sociale : Monoactif employé/ouvrier",
  
  "origine_socialeMénage inactif" =
    "Origine sociale : Ménage inactif"
)

# 2. Construire le tableau

tableau_regression <- comparaison_OR %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    variable = unname(etiquettes[variable])
  )

# 3. Visualiser le tableau
View(tableau_regression)


# Vérifier les dimensions
dim(tableau_regression)

# Vérifier que tous les intitulés sont reconnus
sum(is.na(tableau_regression$variable))

# Vérifier l'échantillon des quatre modèles
sapply(modeles, nobs)


# ============================================
# ÉTAPE 12 - ROBUSTESSE DE L'EFFET DE L'ÂGE
# ============================================

# 1. Créer deux variables d'âge
# Âge centré autour de 45 ans et âge au carré

plan_mfv_age <- update(
  plan_mfv_provisoire,
  age_c = age - 45,
  age_c2 = (age - 45)^2
)

# 2. Estimer le modèle avec effet quadratique

modele4_age2 <- svyglm(
  en_emploi ~ profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  design = plan_mfv_age,
  family = quasibinomial()
)

# 3. Afficher les résultats
summary(modele4_age2)

# 4. Vérifier l'échantillon
nobs(modele4_age2)


# ============================================
# COMPARAISON DU MODÈLE 4
# Avec et sans âge quadratique
# ============================================

profils_a_comparer <- c(
  "profilJamais quitté",
  "profilNatifs de retour"
)

comparaison_age <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_M4 = round(
    exp(coef(modele4)[profils_a_comparer]),
    3
  ),
  
  OR_M4_age2 = round(
    exp(coef(modele4_age2)[profils_a_comparer]),
    3
  ),
  
  row.names = NULL
)

print(comparaison_age)


# ============================================
# ÉTAPE 13 - INTERACTION PROFIL × TERRITOIRE
# ============================================

# 1. Estimer le modèle avec interaction

modele_interaction <- svyglm(
  en_emploi ~
    profil * territoire +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    origine_sociale,
  
  design = plan_mfv_age,
  family = quasibinomial()
)

# 2. Afficher les résultats
summary(modele_interaction)

# 3. Vérifier la population
nobs(modele_interaction)


# ============================================
# IDENTIFIER LES TERMES D'INTERACTION
# ============================================

# Afficher uniquement les noms et coefficients
# des interactions

coeff_interactions <- coef(modele_interaction)

coeff_interactions <- coeff_interactions[
  grepl(":", names(coeff_interactions))
]

print(round(coeff_interactions, 3))


# ============================================
# ÉTAPE 14 - TEST GLOBAL DES INTERACTIONS
# Profil migratoire × territoire
# ============================================

test_interaction <- survey::regTermTest(
  modele_interaction,
  ~ profil:territoire,
  method = "Wald"
)

# Afficher le résultat
print(test_interaction)


# ============================================
# ÉTAPE 15 - TEST DE SENSIBILITÉ
# Échantillon initial : 5 283 individus
# ============================================

# 1. Créer le plan provisoire
# sur les 5 283 observations

plan_mfv_5283 <- svydesign(
  ids = ~1,
  weights = ~PFLIN,
  data = mfv_regression
)

# 2. Ajouter l'âge quadratique

plan_mfv_5283 <- update(
  plan_mfv_5283,
  age_c = age - 45,
  age_c2 = (age - 45)^2
)

# 3. Estimer le modèle sans origine sociale

modele_5283 <- svyglm(
  en_emploi ~ profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire,
  design = plan_mfv_5283,
  family = quasibinomial()
)

# Vérifier le nombre d'observations
nobs(modele_5283)


# ============================================
# ÉCHANTILLON RESTREINT : 4 995 INDIVIDUS
# ============================================

modele_4995 <- svyglm(
  en_emploi ~ profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire,
  design = plan_mfv_age,
  family = quasibinomial()
)

# Vérifier les observations
nobs(modele_4995)


# ============================================
# COMPARAISON DES DEUX ÉCHANTILLONS
# ============================================

variables_profils <- c(
  "profilJamais quitté",
  "profilNatifs de retour"
)

comparaison_echantillons <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_5283 = round(
    exp(coef(modele_5283)[variables_profils]),
    3
  ),
  
  OR_4995 = round(
    exp(coef(modele_4995)[variables_profils]),
    3
  ),
  
  row.names = NULL
)

print(comparaison_echantillons)


# ============================================
# ÉTAPE 16 - ROBUSTESSE DU DIPLÔME
# Classification à 6 niveaux
# ============================================

# Vérifier que la variable initiale existe
stopifnot(
  "niveau_diplome" %in%
    names(mfv_regression_complete)
)

# Créer une variable à 6 niveaux
plan_mfv_age_6 <- update(
  plan_mfv_age,
  
  diplome_6 = relevel(
    factor(niveau_diplome),
    ref = "Aucun diplôme / brevet"
  )
)

# Estimer le modèle complet
modele4_diplome6 <- svyglm(
  en_emploi ~
    profil +
    diplome_6 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_mfv_age_6,
  family = quasibinomial()
)

# Vérifier le nombre d'observations
nobs(modele4_diplome6)


# ============================================
# COMPARAISON 4 ET 6 NIVEAUX DE DIPLÔME
# ============================================

variables_profils <- c(
  "profilJamais quitté",
  "profilNatifs de retour"
)

comparaison_diplomes <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_4_diplomes = round(
    exp(coef(modele4_age2)[variables_profils]),
    3
  ),
  
  OR_6_diplomes = round(
    exp(coef(modele4_diplome6)[variables_profils]),
    3
  ),
  
  row.names = NULL
)

print(comparaison_diplomes)


# ============================================
# ÉTAPE 17 - PROBABILITÉS D'EMPLOI AJUSTÉES
# Modèle 4 avec âge quadratique
# ============================================

# 1. Identifier les trois profils
profils_migratoires <- levels(
  mfv_regression_complete$profil
)

# 2. Calculer les probabilités prédites
# pour chaque profil migratoire

probabilites_ajustees <- sapply(
  profils_migratoires,
  function(p) {
    
    # Copier la population d'analyse
    donnees_prediction <- mfv_regression_complete
    
    # Attribuer le même profil à tous les individus
    donnees_prediction$profil <- factor(
      p,
      levels = profils_migratoires
    )
    
    # Prédire la probabilité d'emploi
    prediction <- predict(
      modele4_age2,
      newdata = donnees_prediction,
      type = "response"
    )
    
    # Calculer la moyenne pondérée
    100 * weighted.mean(
      as.numeric(prediction),
      w = donnees_prediction$PFLIN
    )
  }
)

# 3. Présenter les résultats
tableau_probabilites <- data.frame(
  profil = names(probabilites_ajustees),
  probabilite_emploi = round(
    as.numeric(probabilites_ajustees),
    1
  )
)

print(tableau_probabilites)


# ============================================
# CORRECTION - VARIABLES D'ÂGE
# ============================================

# Créer une base destinée aux prédictions

donnees_prediction_base <- mfv_regression_complete %>%
  mutate(
    age_c = age - 45,
    age_c2 = (age - 45)^2
  )

# Vérifier la présence des variables
c(
  "age_c",
  "age_c2"
) %in% names(donnees_prediction_base)


# ============================================
# ÉTAPE 17 - PROBABILITÉS AJUSTÉES
# VERSION CORRIGÉE
# ============================================

# 1. Identifier les profils migratoires

profils_migratoires <- levels(
  mfv_regression_complete$profil
)

# 2. Calculer les probabilités prédites

probabilites_ajustees <- sapply(
  profils_migratoires,
  function(p) {
    
    # Utiliser la base corrigée
    donnees_prediction <- donnees_prediction_base
    
    # Attribuer le même profil migratoire
    # à toutes les observations
    donnees_prediction$profil <- factor(
      p,
      levels = profils_migratoires
    )
    
    # Prédire la probabilité d'emploi
    prediction <- predict(
      modele4_age2,
      newdata = donnees_prediction,
      type = "response"
    )
    
    # Calculer la moyenne pondérée
    100 * weighted.mean(
      as.numeric(prediction),
      w = donnees_prediction$PFLIN
    )
  }
)

# 3. Construire le tableau final

tableau_probabilites <- data.frame(
  profil = names(probabilites_ajustees),
  probabilite_emploi = round(
    as.numeric(probabilites_ajustees),
    1
  )
)

# 4. Afficher les résultats
print(tableau_probabilites)


# ============================================
# ÉTAPE 18 - ÉCARTS DE PROBABILITÉS AJUSTÉES
# ============================================

# Comparer les trois profils migratoires

ecarts_probabilites <- data.frame(
  comparaison = c(
    "Natifs de retour - Séjours courts",
    "Jamais quitté - Séjours courts",
    "Natifs de retour - Jamais quitté"
  ),
  
  ecart_points = round(
    c(
      probabilites_ajustees["Natifs de retour"] -
        probabilites_ajustees["Séjours courts"],
      
      probabilites_ajustees["Jamais quitté"] -
        probabilites_ajustees["Séjours courts"],
      
      probabilites_ajustees["Natifs de retour"] -
        probabilites_ajustees["Jamais quitté"]
    ),
    2
  )
)

# Afficher les résultats
print(ecarts_probabilites)


# ============================================
# ÉTAPE 19 - TEST DE ROBUSTESSE
# Population âgée de 25 à 54 ans
# ============================================

# 1. Restreindre le plan d'analyse

plan_mfv_25_54 <- subset(
  plan_mfv_age,
  age >= 25 & age <= 54
)

# 2. Estimer le même modèle complet

modele_25_54 <- svyglm(
  en_emploi ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_mfv_25_54,
  family = quasibinomial()
)

# 3. Vérifier le nombre d'observations
nobs(modele_25_54)


# ============================================
# COMPARAISON DES TRANCHES D'ÂGE
# ============================================

variables_profils <- c(
  "profilJamais quitté",
  "profilNatifs de retour"
)

comparaison_tranches_age <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_25_64 = round(
    exp(coef(modele4_age2)[variables_profils]),
    3
  ),
  
  OR_25_54 = round(
    exp(coef(modele_25_54)[variables_profils]),
    3
  ),
  
  row.names = NULL
)

print(comparaison_tranches_age)


# ============================================
# ÉTAPE 20 - EMPLOI ET RETRAITE SELON L'ÂGE
# ============================================

library(dplyr)
library(haven)

# 1. Préparer les tranches d'âge et la retraite

mfv_age_situation <- mfv_regression_complete %>%
  mutate(
    tranche_age = case_when(
      age >= 25 & age <= 54 ~ "25-54 ans",
      age >= 55 & age <= 64 ~ "55-64 ans"
    ),
    
    # Code 4 de SITUA_rec : retraite / préretraite
    retraite = as.numeric(
      haven::zap_labels(SITUA_rec)
    ) == 4
  )

# 2. Calculer les pourcentages pondérés

tableau_age_situation <- mfv_age_situation %>%
  group_by(tranche_age, profil) %>%
  summarise(
    emploi_pct = round(
      100 * weighted.mean(en_emploi, PFLIN),
      1
    ),
    
    retraite_pct = round(
      100 * weighted.mean(retraite, PFLIN),
      1
    ),
    
    .groups = "drop"
  )

# 3. Afficher les résultats
print(tableau_age_situation)


# ============================================
# ÉTAPE 21 - TEST DE ROBUSTESSE
# Population des 25-64 ans hors retraite
# ============================================

# 1. Vérifier la présence de la variable
stopifnot(
  "situa_code" %in%
    names(mfv_regression_complete)
)

# 2. Exclure les personnes déclarant
# être à la retraite ou en préretraite
# Code 4 de SITUA_rec

plan_mfv_hors_retraite <- subset(
  plan_mfv_age,
  situa_code != 4
)

# 3. Estimer le modèle avec les mêmes contrôles

modele_hors_retraite <- svyglm(
  en_emploi ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_mfv_hors_retraite,
  family = quasibinomial()
)

# 4. Vérifier le nombre d'observations
nobs(modele_hors_retraite)


# ============================================
# COMPARAISON AVEC ET SANS RETRAITÉS
# ============================================

variables_profils <- c(
  "profilJamais quitté",
  "profilNatifs de retour"
)

comparaison_retraite <- data.frame(
  profil = c(
    "Jamais quitté",
    "Natifs de retour"
  ),
  
  OR_avec_retraites = round(
    exp(coef(modele4_age2)[variables_profils]),
    3
  ),
  
  OR_hors_retraite = round(
    exp(coef(modele_hors_retraite)[variables_profils]),
    3
  ),
  
  row.names = NULL
)

print(comparaison_retraite)


# ============================================
# ÉTAPE 22 - SYNTHÈSE DES TESTS DE ROBUSTESSE
# ============================================

# 1. Regrouper les modèles retenus

modeles_robustesse <- list(
  "M4 - Age linéaire" = modele4,
  "M4 - Age quadratique" = modele4_age2,
  "Diplôme à 6 niveaux" = modele4_diplome6,
  "Population 25-54 ans" = modele_25_54,
  "Hors retraite" = modele_hors_retraite,
  "Sans origine sociale - 5283" = modele_5283
)

# 2. Construire le tableau comparatif

tableau_robustesse <- data.frame(
  modele = names(modeles_robustesse),
  
  N = sapply(
    modeles_robustesse,
    nobs
  ),
  
  OR_jamais_quitte = sapply(
    modeles_robustesse,
    function(m) {
      exp(coef(m)["profilJamais quitté"])
    }
  ),
  
  OR_natifs_retour = sapply(
    modeles_robustesse,
    function(m) {
      exp(coef(m)["profilNatifs de retour"])
    }
  ),
  
  row.names = NULL
)

# 3. Arrondir les odds ratios

tableau_robustesse$OR_jamais_quitte <-
  round(tableau_robustesse$OR_jamais_quitte, 3)

tableau_robustesse$OR_natifs_retour <-
  round(tableau_robustesse$OR_natifs_retour, 3)

# 4. Visualiser le tableau

View(tableau_robustesse)

print(tableau_robustesse)


# ============================================
# ÉTAPE 23 - RECHERCHE DES VARIABLES
# DU PLAN DE SONDAGE MFV2
# ============================================

# Rechercher d'autres noms possibles
# pour les variables du plan de sondage

variables_plan_complement <- grep(
  "SAE|VAGUE|STRAT|SECT|ZONE|SOND|TIRAGE|PSU|UPA|IDMEN|IDLOG",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

# Afficher uniquement les noms
print(variables_plan_complement)


