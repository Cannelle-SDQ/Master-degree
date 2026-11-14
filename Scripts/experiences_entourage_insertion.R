
# ============================================
# ÉTAPE 57 - INVENTAIRE DES VARIABLES
# EXPÉRIENCES PROFESSIONNELLES ET ENTOURAGE
# ============================================

library(dplyr)

# 1. Variables identifiées dans le dictionnaire MFV2

variables_recherchees <- c(
  "TRAJ_NBEMPL",
  "TRAJ_AGPEM_rec",
  "TRAJ_TCONT",
  "TRAJ_INT",
  "TRAJ_INT1AN",
  "TRAJ_PARC",
  "ASAL_TROUV1",
  "ASAL_TROUV2",
  "AIDEREC_FAM",
  "AIDEREC_TOT",
  "SCO_AIDEAUTRE"
)

# 2. Vérifier leur présence dans la base EGO

inventaire_variables <- data.frame(
  variable = variables_recherchees,
  
  presente = variables_recherchees %in%
    names(ego)
)

# 3. Récupérer les intitulés des variables

inventaire_variables$intitule <- vapply(
  variables_recherchees,
  
  function(v) {
    
    if (!v %in% names(ego)) {
      return("Variable absente")
    }
    
    libelle <- attr(
      ego[[v]],
      "label"
    )
    
    if (is.null(libelle) ||
        length(libelle) == 0) {
      return("Intitulé non renseigné")
    }
    
    as.character(libelle[1])
  },
  
  character(1)
)

# 4. Afficher l'inventaire

print(
  tibble::as_tibble(inventaire_variables),
  width = Inf
)

# 5. Examiner les modalités documentées
# (sans afficher les observations individuelles)

variables_presentes <- variables_recherchees[
  variables_recherchees %in% names(ego)
]

for (v in variables_presentes) {
  
  cat("\n---------------------\n")
  cat("VARIABLE :", v, "\n")
  
  modalites <- attr(
    ego[[v]],
    "labels"
  )
  
  if (is.null(modalites)) {
    cat("Pas d'étiquettes de modalités disponibles\n")
  } else {
    print(modalites)
  }
}


# ============================================
# ÉTAPE 58 - DISPONIBILITÉ DES VARIABLES
# SELON LA SITUATION DE TRAVAIL
# ============================================

library(dplyr)
library(haven)

# 1. Vérifier les variables dans mfv_natifs

variables_a_verifier <- c(
  variables_recherchees,
  "TRAVAIL"
)

variables_absentes <- setdiff(
  variables_a_verifier,
  names(mfv_natifs)
)

if (length(variables_absentes) > 0) {
  stop(
    "Variables absentes de mfv_natifs : ",
    paste(variables_absentes, collapse = ", ")
  )
}

# 2. Créer une base de diagnostic

base_diagnostic <- mfv_natifs %>%
  mutate(
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    
    groupe_travail = case_when(
      travail_code == "1" ~ "Travaille",
      travail_code == "2" ~ "Ne travaille pas",
      TRUE ~ NA_character_
    )
  )

# 3. Calculer la disponibilité de chaque variable

diagnostic_disponibilite <- do.call(
  rbind,
  
  lapply(
    variables_recherchees,
    
    function(v) {
      
      do.call(
        rbind,
        
        lapply(
          c("Travaille", "Ne travaille pas"),
          
          function(g) {
            
            indices <- which(
              base_diagnostic$groupe_travail == g
            )
            
            valeurs <- as.character(
              haven::zap_labels(
                base_diagnostic[[v]][indices]
              )
            )
            
            renseigne <- !is.na(valeurs) &
              trimws(valeurs) != ""
            
            data.frame(
              variable = v,
              groupe = g,
              n_observations = length(indices),
              n_renseigne = sum(renseigne),
              taux_renseigne = round(
                100 * mean(renseigne),
                1
              )
            )
          }
        )
      )
    }
  )
)

# 4. Afficher uniquement les pourcentages
# Masquer ceux fondés sur moins de 10 réponses

tableau_disponibilite <- diagnostic_disponibilite %>%
  mutate(
    taux_renseigne = if_else(
      n_renseigne < 10,
      NA_real_,
      taux_renseigne
    )
  ) %>%
  select(
    variable,
    groupe,
    taux_renseigne
  )

print(
  tibble::as_tibble(tableau_disponibilite),
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 59 - CANAUX D'OBTENTION DE L'EMPLOI
# SELON LE PROFIL MIGRATOIRE
# ============================================

library(dplyr)
library(haven)

# 1. Vérifier la disponibilité des variables

stopifnot(
  all(
    c("profil", "PFLIN",
      "ASAL_TROUV1", "ASAL_TROUV2") %in%
      names(plan_position$variables)
  )
)

# 2. Préparer les deux méthodes déclarées

base_recrutement <- plan_position$variables %>%
  mutate(
    canal1 = as.character(
      haven::zap_labels(ASAL_TROUV1)
    ),
    
    canal2 = as.character(
      haven::zap_labels(ASAL_TROUV2)
    ),
    
    canal1 = if_else(
      canal1 %in% as.character(1:11),
      canal1, NA_character_
    ),
    
    canal2 = if_else(
      canal2 %in% as.character(1:11),
      canal2, NA_character_
    ),
    
    methode_renseignee =
      !is.na(canal1) | !is.na(canal2),
    
    relations_personnelles =
      canal1 %in% "4" | canal2 %in% "4",
    
    relations_professionnelles =
      canal1 %in% "3" | canal2 %in% "3",
    
    concours =
      canal1 %in% "9" | canal2 %in% "9"
  )

# 3. Fonction de pourcentage pondéré
# avec masquage des petites cellules

pourcentage_protege <- function(
    indicateur, base, poids) {
  
  indices <- which(base)
  
  if (length(indices) == 0) {
    return(NA_real_)
  }
  
  oui <- indicateur[indices]
  poids_valides <- poids[indices]
  
  if (sum(oui) < 10 ||
      sum(!oui) < 10) {
    return(NA_real_)
  }
  
  round(
    100 * sum(poids_valides[oui]) /
      sum(poids_valides),
    2
  )
}

# 4. Comparaison entre les trois profils

tableau_canaux_recrutement <- base_recrutement %>%
  group_by(profil) %>%
  summarise(
    
    taux_renseigne = pourcentage_protege(
      methode_renseignee,
      rep(TRUE, n()),
      PFLIN
    ),
    
    relations_personnelles_pct =
      pourcentage_protege(
        relations_personnelles,
        methode_renseignee,
        PFLIN
      ),
    
    relations_professionnelles_pct =
      pourcentage_protege(
        relations_professionnelles,
        methode_renseignee,
        PFLIN
      ),
    
    concours_pct = pourcentage_protege(
      concours,
      methode_renseignee,
      PFLIN
    ),
    
    .groups = "drop"
  )

# 5. Afficher les résultats agrégés

print(
  tibble::as_tibble(
    tableau_canaux_recrutement
  ),
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 60 - CANAUX DE RECRUTEMENT
# SELON LE TYPE D'EMPLOYEUR
# ============================================

library(dplyr)

# 1. Vérifier la présence du type d'employeur

stopifnot(
  "type_employeur" %in%
    names(base_recrutement)
)

# 2. Calculer les proportions pondérées
# en réutilisant la fonction
# pourcentage_protege() de l'étape 59

tableau_canaux_employeur <- base_recrutement %>%
  group_by(type_employeur) %>%
  summarise(
    
    taux_renseigne = pourcentage_protege(
      methode_renseignee,
      rep(TRUE, n()),
      PFLIN
    ),
    
    relations_personnelles_pct =
      pourcentage_protege(
        relations_personnelles,
        methode_renseignee,
        PFLIN
      ),
    
    relations_professionnelles_pct =
      pourcentage_protege(
        relations_professionnelles,
        methode_renseignee,
        PFLIN
      ),
    
    concours_pct = pourcentage_protege(
      concours,
      methode_renseignee,
      PFLIN
    ),
    
    .groups = "drop"
  )

# 3. Afficher les résultats

print(
  tibble::as_tibble(tableau_canaux_employeur),
  width = Inf
)


# ============================================
# ÉTAPE 61 - RECRUTEMENT PAR CONCOURS
# SELON LE PROFIL MIGRATOIRE
# ============================================

library(survey)
library(dplyr)

# 1. Ajouter les indicateurs au plan
# de sondage provisoire

stopifnot(
  nrow(base_recrutement) ==
    nrow(plan_position$variables)
)

plan_concours <- update(
  plan_position,
  
  methode_renseignee =
    base_recrutement$methode_renseignee,
  
  recrutement_concours =
    as.integer(base_recrutement$concours)
)

# 2. Conserver les personnes ayant
# déclaré au moins une méthode

plan_concours <- subset(
  plan_concours,
  methode_renseignee
)

# 3. Diagnostic interne des effectifs

controle_concours <- with(
  plan_concours$variables,
  table(profil, recrutement_concours)
)

diagnostic_concours <- data.frame(
  indicateur = c(
    "Nombre d'observations",
    "Profils avec moins de 10 concours",
    "Profils avec moins de 10 autres méthodes"
  ),
  
  resultat = c(
    nrow(plan_concours$variables),
    
    sum(
      controle_concours[, "1"] < 10
    ),
    
    sum(
      controle_concours[, "0"] < 10
    )
  )
)

print(diagnostic_concours)

# 4. Régression logistique pondérée

modele_concours <- svyglm(
  recrutement_concours ~
    profil +
    type_employeur +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_concours,
  family = quasibinomial()
)

# 5. Vérifier le modèle

nobs(modele_concours)

modele_concours$converged

# 6. Extraire les odds ratios

or_concours <- exp(
  coef(modele_concours)
)

variables_cibles <- grep(
  "^(profil|type_employeur|diplome_4)",
  names(or_concours),
  value = TRUE
)

tableau_or_concours <- data.frame(
  variable = variables_cibles,
  
  odds_ratio = round(
    as.numeric(
      or_concours[variables_cibles]
    ),
    3
  )
)

print(tableau_or_concours)


# ============================================
# ÉTAPE 62 - EXPÉRIENCES PROFESSIONNELLES
# ET SITUATION ACTUELLE D'EMPLOI
# ============================================

library(dplyr)
library(haven)

# 1. Préparer les variables

base_experiences <- mfv_natifs %>%
  mutate(
    
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    
    premier_contrat = as.character(
      haven::zap_labels(TRAJ_TCONT)
    ),
    
    interruption_code = as.character(
      haven::zap_labels(TRAJ_INT)
    ),
    
    groupe_travail = case_when(
      travail_code == "1" ~ "Travaille",
      travail_code == "2" ~ "Ne travaille pas",
      TRUE ~ NA_character_
    ),
    
    # Premier emploi en CDI
    # Codes 8 et 9 : CDI temps complet/partiel
    
    premier_emploi_cdi = case_when(
      premier_contrat %in% c("8", "9") ~ 1,
      premier_contrat %in% as.character(
        c(1:7, 10, 11)
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Interruption professionnelle antérieure
    
    interruption_anterieure = case_when(
      interruption_code == "1" ~ 1,
      interruption_code == "2" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(groupe_travail))

# 2. Comparaison descriptive pondérée

tableau_experiences_emploi <- base_experiences %>%
  group_by(groupe_travail) %>%
  summarise(
    
    couverture_premier_contrat = round(
      100 * mean(!is.na(premier_emploi_cdi)),
      1
    ),
    
    premier_emploi_cdi_pct = round(
      100 * weighted.mean(
        premier_emploi_cdi,
        w = PFLIN,
        na.rm = TRUE
      ),
      2
    ),
    
    couverture_interruptions = round(
      100 * mean(!is.na(interruption_anterieure)),
      1
    ),
    
    interruption_anterieure_pct = round(
      100 * weighted.mean(
        interruption_anterieure,
        w = PFLIN,
        na.rm = TRUE
      ),
      2
    ),
    
    .groups = "drop"
  )

# 3. Afficher les résultats

print(
  tibble::as_tibble(
    tableau_experiences_emploi
  ),
  width = Inf
)


# ============================================
# ÉTAPE 63 - SITUATION D'EMPLOI ACTUELLE
# SELON LES EXPÉRIENCES ANTÉRIEURES
# ============================================

library(dplyr)

# 1. Fonction de calcul des taux d'emploi
# selon une caractéristique antérieure

calculer_taux_emploi <- function(variable, libelles) {
  
  base_experiences %>%
    filter(!is.na(.data[[variable]])) %>%
    group_by(modalite = .data[[variable]]) %>%
    summarise(
      
      # Contrôle interne des effectifs
      n_emploi = sum(travail_code == "1"),
      n_hors_emploi = sum(travail_code == "2"),
      
      # Pourcentage pondéré actuellement en emploi
      taux_emploi = 100 * sum(
        PFLIN[travail_code == "1"]
      ) / sum(PFLIN),
      
      .groups = "drop"
    ) %>%
    mutate(
      
      # Libellés des catégories 0 et 1
      situation = if_else(
        modalite == 1,
        libelles[2],
        libelles[1]
      ),
      
      # Masquage de précaution
      taux_emploi = if_else(
        n_emploi >= 10 & n_hors_emploi >= 10,
        round(taux_emploi, 2),
        NA_real_
      )
    ) %>%
    select(
      situation,
      taux_emploi
    )
}

# 2. Taux d'emploi selon le premier contrat

tableau_cdi_emploi <- calculer_taux_emploi(
  "premier_emploi_cdi",
  c(
    "Premier emploi hors CDI",
    "Premier emploi en CDI"
  )
)

print(tableau_cdi_emploi)

# 3. Taux d'emploi selon les interruptions

tableau_interruptions_emploi <- calculer_taux_emploi(
  "interruption_anterieure",
  c(
    "Pas d'interruption déclarée",
    "Interruption déclarée"
  )
)

print(tableau_interruptions_emploi)


# ============================================
# ÉTAPE 64 - EXPÉRIENCES PROFESSIONNELLES
# ET SITUATION ACTUELLE D'EMPLOI
# ============================================

library(dplyr)
library(haven)
library(survey)

# 1. Récupérer la population des modèles
# d'emploi précédents

base_exp_modele <- plan_mfv_age$variables

# 2. Vérifier les variables nécessaires

variables_necessaires <- c(
  "TRAVAIL",
  "TRAJ_TCONT",
  "TRAJ_INT",
  "profil",
  "diplome_4",
  "age_c",
  "age_c2",
  "sexe",
  "territoire",
  "origine_sociale",
  "PFLIN"
)

stopifnot(
  all(variables_necessaires %in%
        names(base_exp_modele))
)

# 3. Recoder les variables

base_exp_modele <- base_exp_modele %>%
  mutate(
    
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    
    contrat_code = as.character(
      haven::zap_labels(TRAJ_TCONT)
    ),
    
    interruption_code = as.character(
      haven::zap_labels(TRAJ_INT)
    ),
    
    # Situation d'emploi actuelle
    emploi_actuel = case_when(
      travail_code == "1" ~ 1L,
      travail_code == "2" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Premier contrat en CDI
    premier_cdi = case_when(
      contrat_code %in% c("8", "9") ~ 1L,
      contrat_code %in%
        as.character(c(1:7, 10, 11)) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Interruptions professionnelles
    interruption = case_when(
      interruption_code == "1" ~ 1L,
      interruption_code == "2" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 4. Conserver un échantillon commun
# pour les deux modèles

base_exp_complete <- base_exp_modele %>%
  filter(
    !is.na(emploi_actuel),
    !is.na(premier_cdi),
    !is.na(interruption),
    complete.cases(
      pick(
        profil,
        diplome_4,
        age_c,
        age_c2,
        sexe,
        territoire,
        origine_sociale,
        PFLIN
      )
    )
  )

# 5. Vérifier la taille de l'échantillon

nrow(base_exp_complete)

# 6. Construire le plan provisoire

plan_exp <- svydesign(
  ids = ~1,
  weights = ~PFLIN,
  data = base_exp_complete
)

# 7. Modèle de référence

modele_exp_reference <- svyglm(
  emploi_actuel ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_exp,
  family = quasibinomial()
)

# 8. Modèle avec expériences professionnelles

modele_exp_complet <- svyglm(
  emploi_actuel ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    premier_cdi +
    interruption,
  
  design = plan_exp,
  family = quasibinomial()
)

# 9. Vérifier les effectifs identiques

nobs(modele_exp_reference)
nobs(modele_exp_complet)

# 10. Vérifier la convergence

modele_exp_reference$converged
modele_exp_complet$converged

# 11. Comparer les odds ratios

variables_cibles <- c(
  "profilJamais quitté",
  "profilNatifs de retour",
  "premier_cdi",
  "interruption"
)

extraire_or <- function(modele, variable) {
  
  coefficients <- coef(modele)
  
  if (!variable %in% names(coefficients)) {
    return(NA_real_)
  }
  
  exp(unname(coefficients[variable]))
}

tableau_or_experiences <- data.frame(
  variable = variables_cibles,
  
  OR_reference = sapply(
    variables_cibles,
    function(v) {
      extraire_or(modele_exp_reference, v)
    }
  ),
  
  OR_avec_experiences = sapply(
    variables_cibles,
    function(v) {
      extraire_or(modele_exp_complet, v)
    }
  ),
  
  row.names = NULL
)

# 12. Arrondir les résultats

tableau_or_experiences[-1] <- round(
  tableau_or_experiences[-1],
  3
)

# 13. Afficher

print(tableau_or_experiences)


# ============================================
# ÉTAPE 65 - PROBABILITÉS AJUSTÉES
# SELON LES EXPÉRIENCES PROFESSIONNELLES
# ============================================

library(dplyr)

# 1. Fonction de standardisation marginale

calculer_proba_experience <- function(
    variable, valeur) {
  
  nouvelles_donnees <- base_exp_complete
  
  # Attribuer la même modalité
  # à toute la population de référence
  
  nouvelles_donnees[[variable]] <- rep(
    valeur,
    nrow(nouvelles_donnees)
  )
  
  # Prédire la probabilité d'emploi actuel
  
  predictions <- as.numeric(
    predict(
      modele_exp_complet,
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

# 2. Construire le tableau

tableau_proba_experiences <- data.frame(
  experience = c(
    "Premier emploi hors CDI",
    "Premier emploi en CDI",
    "Sans interruption déclarée",
    "Avec interruption déclarée"
  ),
  
  probabilite_emploi = c(
    calculer_proba_experience(
      "premier_cdi", 0
    ),
    
    calculer_proba_experience(
      "premier_cdi", 1
    ),
    
    calculer_proba_experience(
      "interruption", 0
    ),
    
    calculer_proba_experience(
      "interruption", 1
    )
  )
)

# 3. Arrondir les résultats

tableau_proba_experiences$probabilite_emploi <-
  round(
    tableau_proba_experiences$probabilite_emploi,
    2
  )

# 4. Afficher

print(tableau_proba_experiences)

# 5. Calculer les écarts ajustés

ecart_cdi <- with(
  tableau_proba_experiences,
  probabilite_emploi[2] -
    probabilite_emploi[1]
)

ecart_interruption <- with(
  tableau_proba_experiences,
  probabilite_emploi[4] -
    probabilite_emploi[3]
)

cat(
  "\nÉcart ajusté premier CDI :",
  round(ecart_cdi, 2),
  "points\n"
)

cat(
  "Écart ajusté interruption :",
  round(ecart_interruption, 2),
  "points\n"
)


# ============================================
# ÉTAPE 66 - AIDES DE L'ENTOURAGE
# ET SITUATION ACTUELLE D'EMPLOI
# ============================================

library(dplyr)
library(haven)

# 1. Préparer les variables

base_entourage <- mfv_natifs %>%
  mutate(
    
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    
    aide_fam_code = as.character(
      haven::zap_labels(AIDEREC_FAM)
    ),
    
    aide_tot_code = as.character(
      haven::zap_labels(AIDEREC_TOT)
    ),
    
    # Situation de travail actuelle
    emploi_actuel = case_when(
      travail_code == "1" ~ 1L,
      travail_code == "2" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Aide régulière de la famille
    aide_familiale = case_when(
      aide_fam_code == "1" ~ 1L,
      aide_fam_code == "0" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Aide régulière toutes sources
    aide_totale = case_when(
      aide_tot_code == "1" ~ 1L,
      aide_tot_code == "0" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 2. Fonction de calcul des taux d'emploi

calculer_taux_entourage <- function(
    variable, nom_indicateur) {
  
  base_entourage %>%
    filter(
      !is.na(emploi_actuel),
      !is.na(.data[[variable]]),
      !is.na(PFLIN),
      PFLIN > 0
    ) %>%
    group_by(
      aide = .data[[variable]]
    ) %>%
    summarise(
      
      # Contrôles internes
      n_emploi = sum(emploi_actuel == 1),
      n_hors_emploi = sum(emploi_actuel == 0),
      
      # Taux pondéré
      taux_emploi = 100 * weighted.mean(
        emploi_actuel,
        w = PFLIN
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      
      indicateur = nom_indicateur,
      
      situation = if_else(
        aide == 1,
        "Reçoit une aide régulière",
        "Ne reçoit pas d'aide régulière"
      ),
      
      # Masquer les petites cellules
      taux_emploi = if_else(
        n_emploi >= 10 & n_hors_emploi >= 10,
        round(taux_emploi, 2),
        NA_real_
      )
    ) %>%
    select(
      indicateur,
      situation,
      taux_emploi
    )
}

# 3. Aide familiale

tableau_aide_famille <- calculer_taux_entourage(
  "aide_familiale",
  "Aide familiale"
)

print(tableau_aide_famille)

# 4. Aide régulière toutes sources

tableau_aide_totale <- calculer_taux_entourage(
  "aide_totale",
  "Aide toutes sources"
)

print(tableau_aide_totale)


# ============================================
# ÉTAPE 67 - AIDES RÉGULIÈRES
# ET SITUATION ACTUELLE D'EMPLOI
# RÉGRESSIONS LOGISTIQUES PONDÉRÉES
# ============================================

library(dplyr)
library(haven)
library(survey)

# 1. Reprendre la base des modèles d'emploi

base_aides_modele <- plan_mfv_age$variables %>%
  mutate(
    
    travail_code = as.character(
      haven::zap_labels(TRAVAIL)
    ),
    
    aide_fam_code = as.character(
      haven::zap_labels(AIDEREC_FAM)
    ),
    
    aide_tot_code = as.character(
      haven::zap_labels(AIDEREC_TOT)
    ),
    
    # Exercice actuel d'une activité
    emploi_actuel = case_when(
      travail_code == "1" ~ 1L,
      travail_code == "2" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Réception d'une aide familiale
    aide_familiale = case_when(
      aide_fam_code == "1" ~ 1L,
      aide_fam_code == "0" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Réception d'une aide toutes sources
    aide_totale = case_when(
      aide_tot_code == "1" ~ 1L,
      aide_tot_code == "0" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 2. Conserver le même échantillon
# pour les trois modèles

base_aides_complete <- base_aides_modele %>%
  filter(
    complete.cases(
      pick(
        emploi_actuel,
        aide_familiale,
        aide_totale,
        profil,
        diplome_4,
        age_c,
        age_c2,
        sexe,
        territoire,
        origine_sociale,
        PFLIN
      )
    ),
    PFLIN > 0
  )

# 3. Vérifier la population

nrow(base_aides_complete)

# 4. Construire le plan provisoire

plan_aides <- svydesign(
  ids = ~1,
  weights = ~PFLIN,
  data = base_aides_complete
)

# 5. Modèle de référence

modele_aides_reference <- svyglm(
  emploi_actuel ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  
  design = plan_aides,
  family = quasibinomial()
)

# 6. Modèle avec aide familiale

modele_aide_famille <- svyglm(
  emploi_actuel ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    aide_familiale,
  
  design = plan_aides,
  family = quasibinomial()
)

# 7. Modèle avec aide toutes sources

modele_aide_totale <- svyglm(
  emploi_actuel ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale +
    aide_totale,
  
  design = plan_aides,
  family = quasibinomial()
)

# 8. Vérifier les effectifs

c(
  reference = nobs(modele_aides_reference),
  famille = nobs(modele_aide_famille),
  toutes_sources = nobs(modele_aide_totale)
)

# 9. Vérifier les convergences

c(
  reference = modele_aides_reference$converged,
  famille = modele_aide_famille$converged,
  toutes_sources = modele_aide_totale$converged
)

# 10. Fonction d'extraction des OR

extraire_or_aides <- function(modele, variable) {
  
  b <- coef(modele)
  
  if (!variable %in% names(b)) {
    return(NA_real_)
  }
  
  exp(unname(b[variable]))
}

# 11. Comparer les coefficients

variables_aides <- c(
  "profilJamais quitté",
  "profilNatifs de retour",
  "aide_familiale",
  "aide_totale"
)

tableau_or_aides <- data.frame(
  variable = variables_aides,
  
  OR_reference = sapply(
    variables_aides,
    function(v) extraire_or_aides(
      modele_aides_reference, v
    )
  ),
  
  OR_famille = sapply(
    variables_aides,
    function(v) extraire_or_aides(
      modele_aide_famille, v
    )
  ),
  
  OR_toutes_sources = sapply(
    variables_aides,
    function(v) extraire_or_aides(
      modele_aide_totale, v
    )
  ),
  
  row.names = NULL
)

# 12. Arrondir les valeurs

tableau_or_aides[-1] <- round(
  tableau_or_aides[-1],
  3
)

# 13. Afficher les résultats

print(tableau_or_aides)


# ============================================
# ÉTAPE 68 - PROBABILITÉS AJUSTÉES
# AIDES RÉGULIÈRES ET EMPLOI ACTUEL
# ============================================

library(dplyr)

# 1. Fonction de standardisation marginale

calculer_proba_aide <- function(
    modele, variable, valeur) {
  
  nouvelles_donnees <- base_aides_complete
  
  # Fixer la situation d'aide pour toutes
  # les personnes de la population de référence
  
  nouvelles_donnees[[variable]] <- rep(
    valeur,
    nrow(nouvelles_donnees)
  )
  
  # Calculer les probabilités prédites
  
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
    w = nouvelles_donnees$PFLIN
  )
}

# 2. Construire le tableau des probabilités

tableau_proba_aides <- data.frame(
  
  indicateur = c(
    "Aide familiale régulière",
    "Aide régulière toutes sources"
  ),
  
  sans_aide = c(
    calculer_proba_aide(
      modele_aide_famille,
      "aide_familiale", 0
    ),
    
    calculer_proba_aide(
      modele_aide_totale,
      "aide_totale", 0
    )
  ),
  
  avec_aide = c(
    calculer_proba_aide(
      modele_aide_famille,
      "aide_familiale", 1
    ),
    
    calculer_proba_aide(
      modele_aide_totale,
      "aide_totale", 1
    )
  )
)

# 3. Calculer les écarts avant arrondi

tableau_proba_aides <- tableau_proba_aides %>%
  mutate(
    ecart_points = avec_aide - sans_aide
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 2)
    )
  )

# 4. Afficher les résultats

print(tableau_proba_aides)

# 5. Vérifier l'effectif du modèle

nobs(modele_aide_famille)
nobs(modele_aide_totale)


# ============================================
# ÉTAPE 69 - EXPÉRIENCES, RELATIONS
# ET POSITIONS PROFESSIONNELLES
# ============================================

library(dplyr)
library(haven)

# 1. Reprendre la population du modèle multinomial

base_positions_ressources <- base_multinom %>%
  mutate(
    
    # Variables professionnelles
    contrat_code = as.character(
      haven::zap_labels(TRAJ_TCONT)
    ),
    
    interruption_code = as.character(
      haven::zap_labels(TRAJ_INT)
    ),
    
    # Méthodes d'obtention de l'emploi
    canal1 = as.character(
      haven::zap_labels(ASAL_TROUV1)
    ),
    
    canal2 = as.character(
      haven::zap_labels(ASAL_TROUV2)
    ),
    
    # 2. Premier emploi en CDI
    premier_cdi = case_when(
      contrat_code %in% c("8", "9") ~ 1L,
      contrat_code %in%
        as.character(c(1:7, 10, 11)) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # 3. Interruptions professionnelles
    interruption = case_when(
      interruption_code == "1" ~ 1L,
      interruption_code == "2" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # 4. Identifier les méthodes valides
    canal1 = if_else(
      canal1 %in% as.character(1:11),
      canal1,
      NA_character_
    ),
    
    canal2 = if_else(
      canal2 %in% as.character(1:11),
      canal2,
      NA_character_
    ),
    
    methode_renseignee =
      !is.na(canal1) | !is.na(canal2),
    
    # 5. Relations utilisées pour le recrutement
    # 3 = relations professionnelles
    # 4 = relations familiales ou personnelles
    
    recrutement_relations = case_when(
      !methode_renseignee ~ NA_integer_,
      canal1 %in% c("3", "4") |
        canal2 %in% c("3", "4") ~ 1L,
      TRUE ~ 0L
    ),
    
    # 6. Concours ou examen
    recrutement_concours = case_when(
      !methode_renseignee ~ NA_integer_,
      canal1 == "9" | canal2 == "9" ~ 1L,
      TRUE ~ 0L
    )
  )

# 7. Vérifier la population

nrow(base_positions_ressources)

# 8. Vérifier les taux de disponibilité

variables_ressources <- c(
  "premier_cdi",
  "interruption",
  "recrutement_relations",
  "recrutement_concours"
)

diagnostic_ressources <- data.frame(
  variable = variables_ressources,
  
  taux_renseigne = sapply(
    variables_ressources,
    function(v) {
      round(
        100 * mean(
          !is.na(base_positions_ressources[[v]])
        ),
        1
      )
    }
  ),
  
  row.names = NULL
)

print(diagnostic_ressources)

# 9. Effectif commun disponible pour un modèle
# intégrant toutes les ressources

variables_modele <- c(
  "position_3",
  variables_ressources
)

n_complets_ressources <- sum(
  complete.cases(
    base_positions_ressources[
      , variables_modele,
      drop = FALSE
    ]
  )
)

cat(
  "\nObservations complètes :",
  n_complets_ressources,
  "\n"
)

# 10. Diagnostic local des petites cellules
# par position socioprofessionnelle

diagnostic_petites_cellules <- sapply(
  c(
    "recrutement_relations",
    "recrutement_concours"
  ),
  
  function(v) {
    
    tab <- table(
      base_positions_ressources$position_3,
      base_positions_ressources[[v]]
    )
    
    sum(tab < 10)
  }
)

print(diagnostic_petites_cellules)


# ============================================
# ÉTAPE 70 - MODÈLE MULTINOMIAL
# AVEC EXPÉRIENCES ET RECRUTEMENT
# ============================================

library(dplyr)
library(nnet)

# 1. Définir les nouvelles variables
# à intégrer dans le modèle

variables_ressources_principales <- c(
  "premier_cdi",
  "recrutement_relations",
  "recrutement_concours"
)

# 2. Préparer l'échantillon commun

base_ressources_principale <-
  base_positions_ressources %>%
  filter(
    if_all(
      all_of(variables_ressources_principales),
      ~ !is.na(.x)
    ),
    PFLIN > 0
  ) %>%
  mutate(
    poids_modele = PFLIN / mean(PFLIN)
  )

# 3. Vérifier le nombre d'observations

cat(
  "Observations disponibles :",
  nrow(base_ressources_principale),
  "\n"
)

# 4. Modèle de référence

modele_ressources_reference <- nnet::multinom(
  position_3 ~
    profil +
    diplome_4 +
    age_c20 +
    age_c2_400 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur,
  
  data = base_ressources_principale,
  weights = poids_modele,
  
  trace = FALSE,
  maxit = 500,
  MaxNWts = 10000
)

# 5. Modèle enrichi

modele_ressources_complet <- nnet::multinom(
  position_3 ~
    profil +
    diplome_4 +
    age_c20 +
    age_c2_400 +
    sexe +
    territoire +
    origine_sociale +
    type_employeur +
    premier_cdi +
    recrutement_relations +
    recrutement_concours,
  
  data = base_ressources_principale,
  weights = poids_modele,
  
  trace = FALSE,
  maxit = 500,
  MaxNWts = 10000
)

# 6. Vérifier les convergences

c(
  reference =
    modele_ressources_reference$convergence,
  
  complet =
    modele_ressources_complet$convergence
)

# 7. Vérifier les effectifs des modèles

c(
  reference =
    nrow(fitted(modele_ressources_reference)),
  
  complet =
    nrow(fitted(modele_ressources_complet))
)

# 8. Extraire les rapports de risques
# relatifs (RRR) du modèle enrichi

coefficients_ressources <- coef(
  modele_ressources_complet
)

variables_cibles <- grep(
  "^(profil|premier_cdi|recrutement_relations|recrutement_concours)",
  colnames(coefficients_ressources),
  value = TRUE
)

tableau_rrr_ressources <- as.data.frame(
  round(
    exp(
      coefficients_ressources[
        , variables_cibles,
        drop = FALSE
      ]
    ),
    3
  )
)

# 9. Afficher les résultats

print(tableau_rrr_ressources)


# ============================================
# ÉTAPE 71 - COMPARAISON DES MODÈLES
# MULTINOMIAUX AVANT ET APRÈS RESSOURCES
# ============================================

library(dplyr)

# 1. Population commune
base_comparaison <- base_ressources_principale

profils <- levels(base_comparaison$profil)
categories <- levels(base_comparaison$position_3)

# 2. Fonction de standardisation marginale

calculer_probas_multinom <- function(
    modele, nom_modele, profil_choisi) {
  
  nouvelles_donnees <- base_comparaison
  
  nouvelles_donnees$profil <- factor(
    rep(profil_choisi, nrow(nouvelles_donnees)),
    levels = profils
  )
  
  predictions <- as.matrix(
    predict(
      modele,
      newdata = nouvelles_donnees,
      type = "probs"
    )
  )
  
  probabilites <- sapply(
    categories,
    function(categorie) {
      100 * weighted.mean(
        predictions[, categorie],
        w = nouvelles_donnees$PFLIN
      )
    }
  )
  
  data.frame(
    modele = nom_modele,
    profil = profil_choisi,
    t(probabilites),
    check.names = FALSE,
    row.names = NULL
  )
}

# 3. Calculer les deux modèles

resultats_reference <- lapply(
  profils,
  function(p) {
    calculer_probas_multinom(
      modele_ressources_reference,
      "Sans ressources",
      p
    )
  }
)

resultats_complet <- lapply(
  profils,
  function(p) {
    calculer_probas_multinom(
      modele_ressources_complet,
      "Avec ressources",
      p
    )
  }
)

# 4. Construire le tableau comparatif

tableau_comparaison_ressources <- bind_rows(
  resultats_reference,
  resultats_complet
)

# 5. Vérifier que chaque ligne totalise 100 %

stopifnot(
  all(
    abs(
      rowSums(
        tableau_comparaison_ressources[
          , categories, drop = FALSE
        ]
      ) - 100
    ) < 1e-6
  )
)

# 6. Arrondir les résultats

tableau_comparaison_ressources[
  , categories
] <- round(
  tableau_comparaison_ressources[
    , categories, drop = FALSE
  ],
  2
)

# 7. Afficher

print(tableau_comparaison_ressources)

View(tableau_comparaison_ressources)


# ============================================
# ÉTAPE 72 - SÉLECTION LIÉE À LA
# DISPONIBILITÉ DES INTERRUPTIONS
# ============================================

library(dplyr)

# 1. Reprendre la population du modèle enrichi

base_diagnostic_interruptions <-
  base_ressources_principale %>%
  mutate(
    interruption_disponible =
      !is.na(interruption)
  )

# 2. Vérifier la couverture globale

diagnostic_global <- base_diagnostic_interruptions %>%
  summarise(
    taux_disponibilite = round(
      100 * mean(interruption_disponible),
      1
    )
  )

print(diagnostic_global)

# 3. Fonction de diagnostic par catégorie
# sans diffusion des petits effectifs

diagnostic_disponibilite_groupe <- function(variable) {
  
  base_diagnostic_interruptions %>%
    group_by(groupe = .data[[variable]]) %>%
    summarise(
      n_disponible =
        sum(interruption_disponible),
      
      n_absent =
        sum(!interruption_disponible),
      
      taux_disponibilite = round(
        100 * mean(interruption_disponible),
        1
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      taux_disponibilite = if_else(
        n_disponible >= 10 & n_absent >= 10,
        taux_disponibilite,
        NA_real_
      )
    ) %>%
    select(
      groupe,
      taux_disponibilite
    )
}

# 4. Disponibilité selon le parcours migratoire

diagnostic_profil <- diagnostic_disponibilite_groupe(
  "profil"
)

print(diagnostic_profil)

# 5. Disponibilité selon la position professionnelle

diagnostic_position <- diagnostic_disponibilite_groupe(
  "position_3"
)

print(diagnostic_position)

# 6. Contrôle local des petites cellules
# parmi les observations avec interruption renseignée

base_interruptions_complete <-
  base_diagnostic_interruptions %>%
  filter(interruption_disponible)

controle_interruptions <- with(
  base_interruptions_complete,
  table(
    profil,
    position_3,
    interruption
  )
)

diagnostic_cellules <- data.frame(
  indicateur = c(
    "Cellules sans observation",
    "Cellules avec moins de 10 observations"
  ),
  
  resultat = c(
    sum(controle_interruptions == 0),
    sum(controle_interruptions < 10)
  )
)

print(diagnostic_cellules)


# ============================================
# ÉTAPE 73 - VÉRIFIER LE CHAMP
# DE LA VARIABLE TRAJ_INT
# ============================================

library(dplyr)
library(haven)

# 1. Afficher l'intitulé complet
# de la question

attr(ego$TRAJ_INT, "label")

# 2. Vérifier les modalités de réponse

attr(ego$TRAJ_INT, "labels")

# 3. Examiner l'intitulé des questions
# voisines sur le parcours professionnel

variables_trajectoire <- c(
  "TRAJ_NBEMPL",
  "TRAJ_AGPEM_rec",
  "TRAJ_TCONT",
  "TRAJ_INT",
  "TRAJ_INT1AN",
  "TRAJ_PARC"
)

for (v in variables_trajectoire) {
  cat(
    "\n", v, " : ",
    attr(ego[[v]], "label"),
    "\n"
  )
}

# 4. Vérifier la couverture d'autres
# variables chez les personnes ayant
# ou non une réponse à TRAJ_INT

diagnostic_champ <- base_ressources_principale %>%
  mutate(
    interruption_disponible =
      !is.na(interruption),
    
    premier_emploi_renseigne =
      !is.na(premier_cdi),
    
    parcours_renseigne =
      !is.na(TRAJ_PARC),
    
    nombre_emplois_renseigne =
      !is.na(TRAJ_NBEMPL)
  ) %>%
  group_by(interruption_disponible) %>%
  summarise(
    premier_emploi_pct = round(
      100 * mean(premier_emploi_renseigne),
      1
    ),
    
    parcours_pct = round(
      100 * mean(parcours_renseigne),
      1
    ),
    
    nombre_emplois_pct = round(
      100 * mean(nombre_emplois_renseigne),
      1
    ),
    
    .groups = "drop"
  )

print(diagnostic_champ)


# ============================================
# ÉTAPE 74 - DISPONIBILITÉ DE TRAJ_INT
# SELON LE PARCOURS PROFESSIONNEL
# ============================================

library(dplyr)
library(haven)

# 1. Repartir des 1 029 observations
# sans filtrage sur le premier contrat

base_champ_74 <- base_multinom %>%
  mutate(
    
    int_code = as.character(
      haven::zap_labels(TRAJ_INT)
    ),
    
    interruption_disponible =
      int_code %in% c("1", "2"),
    
    nb_emplois = as.character(
      haven::zap_labels(TRAJ_NBEMPL)
    ),
    
    nb_emplois = if_else(
      nb_emplois %in% as.character(1:4),
      nb_emplois,
      NA_character_
    ),
    
    contrat_code = as.character(
      haven::zap_labels(TRAJ_TCONT)
    ),
    
    parcours_code = as.character(
      haven::zap_labels(TRAJ_PARC)
    ),
    
    premier_contrat_valide =
      contrat_code %in% as.character(1:11),
    
    parcours_valide =
      parcours_code %in% as.character(1:4)
  )

# 2. Vérifier les informations valides
# selon la disponibilité de TRAJ_INT

diagnostic_74_global <- base_champ_74 %>%
  group_by(interruption_disponible) %>%
  summarise(
    
    premier_contrat_pct = round(
      100 * mean(premier_contrat_valide),
      1
    ),
    
    parcours_pct = round(
      100 * mean(parcours_valide),
      1
    ),
    
    nb_emplois_pct = round(
      100 * mean(!is.na(nb_emplois)),
      1
    ),
    
    .groups = "drop"
  )

print(
  diagnostic_74_global,
  width = Inf
)

# 3. Examiner la disponibilité de TRAJ_INT
# selon le nombre d'emplois occupés

diagnostic_74_emplois <- base_champ_74 %>%
  filter(!is.na(nb_emplois)) %>%
  group_by(nb_emplois) %>%
  summarise(
    
    n_renseigne = sum(
      interruption_disponible
    ),
    
    n_non_renseigne = sum(
      !interruption_disponible
    ),
    
    taux_disponibilite = round(
      100 * mean(interruption_disponible),
      1
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    nombre_emplois = case_when(
      nb_emplois == "1" ~ "1 emploi",
      nb_emplois == "2" ~ "2 emplois",
      nb_emplois == "3" ~ "3 emplois",
      nb_emplois == "4" ~ "4 emplois ou plus"
    ),
    
    # Masquer les résultats si une
    # sous-catégorie est trop petite
    
    taux_disponibilite = if_else(
      n_renseigne >= 10 &
        n_non_renseigne >= 10,
      taux_disponibilite,
      NA_real_
    )
  ) %>%
  select(
    nombre_emplois,
    taux_disponibilite
  )

print(diagnostic_74_emplois)


# ============================================
# ÉTAPE 75 - COMPATIBILITÉ AVEC LE FILTRE
# HISTORIQUE DE MFV1
# ============================================

library(dplyr)

controle_filtre_historique <- base_champ_74 %>%
  filter(!is.na(nb_emplois)) %>%
  mutate(
    filtre_presume = nb_emplois %in% c(
      "2", "3", "4"
    ),
    
    concordance =
      filtre_presume == interruption_disponible
  ) %>%
  summarise(
    diagnostic = case_when(
      mean(concordance) >= 0.95 ~
        "Concordance forte (au moins 95 %)",
      
      mean(concordance) >= 0.80 ~
        "Concordance partielle (80 à moins de 95 %)",
      
      TRUE ~
        "Concordance faible (moins de 80 %)"
    )
  )

print(controle_filtre_historique)


# ÉTAPE 76 - FAISABILITÉ D'UNE ANALYSE
# DE RÉSEAUX AVEC MFV2

noms <- names(ego)

# 1. Variables permettant potentiellement
# d'identifier des relations dans le logement

liens_logement <- grep(
  "^(NOI|CONJOINT|MER2E|PER2E|LIENPERS)",
  noms,
  value = TRUE,
  ignore.case = TRUE
)

# 2. Variables détaillées de soutien
# provenant des personnes de l'entourage

liens_soutien <- grep(
  "^(GER_lienaid|ANFR_.*FamR|SFR_Pers)",
  noms,
  value = TRUE,
  ignore.case = TRUE
)

# 3. Variables sur les canaux de recrutement

liens_professionnels <- grep(
  "^ASAL_TROUV",
  noms,
  value = TRUE,
  ignore.case = TRUE
)

# 4. Afficher seulement les noms
# des variables, jamais les réponses

cat("\nRELATIONS DANS LE LOGEMENT\n")
print(liens_logement)

cat("\nPERSONNES APPORTANT UN SOUTIEN\n")
print(liens_soutien)

cat("\nCANAUX DE RECRUTEMENT\n")
print(liens_professionnels)

# 5. Rechercher si un tableau des habitants
# est déjà chargé séparément dans R

cat("\nAUTRES OBJETS POTENTIELS\n")

print(
  grep(
    "thl|habitants|menage",
    ls(envir = .GlobalEnv),
    value = TRUE,
    ignore.case = TRUE
  )
)


# ============================================
# ÉTAPE 77 - IDENTIFIER LES LIENS
# EXPLOITABLES POUR L'ANALYSE DE RÉSEAUX
# ============================================

library(haven)

# 1. Variables à examiner

variables_a_verifier <- c(
  "NOIKISH",
  "CONJOINT",
  "LIENPERS_01",
  "LIENPERS_02",
  "LIENPERS_13",
  "GER_LIENAID_1",
  "ANFR_ADMIFAMRP1",
  "SFR_PERS_1"
)

# 2. Afficher les intitulés et dictionnaires
# de modalités, sans afficher les réponses

for (v in variables_a_verifier) {
  
  if (v %in% names(ego)) {
    
    cat("\n============================\n")
    cat("VARIABLE :", v, "\n")
    
    libelle <- attr(ego[[v]], "label")
    
    cat(
      "Intitulé :",
      if (is.null(libelle)) {
        "Non renseigné"
      } else {
        as.character(libelle)
      },
      "\n"
    )
    
    modalites <- attr(ego[[v]], "labels")
    
    if (!is.null(modalites)) {
      cat("Dictionnaire des modalités :\n")
      print(modalites)
    }
  }
}

# 3. Rechercher les variables décrivant
# la nature et les références des liens

variables_liens <- grep(
  "^(LIENTYP|LIENPERS|CONJOINT|MER2E|PER2E|MERELOG|PERELOG)",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

cat("\nVARIABLES RELATIONNELLES :\n")
print(variables_liens)

# 4. Rechercher les noms d'identifiants
# potentiels (sans afficher leurs valeurs)

variables_identifiants <- grep(
  "(IDENT|^ID|NOI|NUMFA|KISH|IDMEN|IDLOG)",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

cat("\nIDENTIFIANTS POTENTIELS :\n")
print(variables_identifiants)


# ============================================
# ÉTAPE 78 - DIAGNOSTIC DES LIENS
# INDIVIDUELS DANS MFV2
# ============================================

library(dplyr)
library(haven)

# 1. Variables de relations entre habitants

cols_liens <- sprintf(
  "LIENPERS_%02d", 1:13
)

cols_types <- sprintf(
  "LIENTYP_%02d", 1:13
)

stopifnot(
  all(cols_liens %in% names(mfv_natifs)),
  all(cols_types %in% names(mfv_natifs))
)

# 2. Fonction de conversion des codes

convertir_code <- function(x) {
  suppressWarnings(
    as.integer(
      as.character(haven::zap_labels(x))
    )
  )
}

# 3. Matrices des personnes référencées
# et de la nature des relations

mat_liens <- do.call(
  cbind,
  lapply(
    cols_liens,
    function(v) convertir_code(mfv_natifs[[v]])
  )
)

mat_types <- do.call(
  cbind,
  lapply(
    cols_types,
    function(v) convertir_code(mfv_natifs[[v]])
  )
)

# 4. Numéro individuel auquel
# correspond chaque colonne

n <- nrow(mfv_natifs)

mat_sources <- matrix(
  rep(1:13, each = n),
  nrow = n
)

# 5. Identifier les références valides
# à une autre personne du logement

liens_valides <-
  !is.na(mat_liens) &
  mat_liens >= 1 &
  mat_liens <= 13 &
  mat_liens != mat_sources

# 6. Vérifier si la nature du lien
# est également renseignée

types_valides <-
  !is.na(mat_types) &
  mat_types >= 1 &
  mat_types <= 8

liens_qualifies <-
  liens_valides & types_valides

# 7. Nombre de références par observation

nb_liens <- rowSums(
  liens_valides,
  na.rm = TRUE
)

nb_liens_qualifies <- rowSums(
  liens_qualifies,
  na.rm = TRUE
)

# 8. Protection des résultats agrégés

pourcentage_protege <- function(effectif, total) {
  if (
    effectif < 10 ||
    (total - effectif) < 10
  ) {
    return(NA_real_)
  }
  
  round(100 * effectif / total, 1)
}

# 9. Diagnostic agrégé

diagnostic_reseaux <- data.frame(
  indicateur = c(
    "Au moins une relation individuelle",
    "Au moins deux relations individuelles",
    "Au moins une relation avec type connu"
  ),
  
  pourcentage = c(
    pourcentage_protege(
      sum(nb_liens >= 1), n
    ),
    
    pourcentage_protege(
      sum(nb_liens >= 2), n
    ),
    
    pourcentage_protege(
      sum(nb_liens_qualifies >= 1), n
    )
  )
)

print(diagnostic_reseaux)

# 10. Vérifier les variables utiles
# au repérage des habitants

print(
  intersect(
    c(
      "NHAB",
      "NUV",
      "NOIKISH",
      "CONJOINT",
      "NUMFA",
      "IDENT_EGO"
    ),
    names(mfv_natifs)
  )
)


# ============================================
# ÉTAPE 79 - IDENTIFIER LES VARIABLES
# DE FILTRAGE DES AIDES REÇUES
# ============================================

library(dplyr)

# 1. Rechercher les variables indiquant
# si une aide a été reçue

variables_aides <- grep(
  "^(GER_GARDENF|ANFR_ADMIFAM|ANFR_MENFAM|ANFR_QUOTIFAM|SFR_EXIST)$",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

# 2. Relever uniquement leurs intitulés

dictionnaire_aides <- data.frame(
  variable = variables_aides,
  
  intitule = sapply(
    variables_aides,
    function(v) {
      lib <- attr(ego[[v]], "label")
      if (is.null(lib)) {
        "Intitulé absent"
      } else {
        as.character(lib)
      }
    }
  ),
  
  row.names = NULL
)

print(dictionnaire_aides, row.names = FALSE)

# 3. Rechercher des variables
# supplémentaires sur l'aide reçue

variables_complementaires <- grep(
  "^(GER_|ANFR_|SFR_)",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

cat(
  "\nNombre de variables disponibles sur les aides :",
  length(variables_complementaires),
  "\n"
)


# ============================================
# ÉTAPE 80 - CONFIGURATIONS DE SOLIDARITÉ
# SELON LES PARCOURS MIGRATOIRES
# ============================================

library(dplyr)
library(haven)

# 1. Fonction de recodage Oui / Non
# en utilisant les libellés du dictionnaire

recoder_oui_non <- function(x) {
  
  labs <- attr(x, "labels")
  
  if (is.null(labs)) {
    stop("Dictionnaire de modalités absent.")
  }
  
  libelles <- tolower(
    trimws(
      gsub("^[0-9]+[. ]*", "", names(labs))
    )
  )
  
  index_oui <- which(
    grepl("^oui\\b", libelles, perl = TRUE)
  )
  
  index_non <- which(
    grepl("^non\\b", libelles, perl = TRUE)
  )
  
  if (
    length(index_oui) != 1 ||
    length(index_non) != 1
  ) {
    stop(
      "Modalités Oui/Non ambiguës : ",
      paste(names(labs), collapse = " ; ")
    )
  }
  
  code_oui <- as.character(
    unname(labs[index_oui])
  )
  
  code_non <- as.character(
    unname(labs[index_non])
  )
  
  valeurs <- as.character(
    haven::zap_labels(x)
  )
  
  case_when(
    valeurs == code_oui ~ 1L,
    valeurs == code_non ~ 0L,
    TRUE ~ NA_integer_
  )
}

# 2. Fonction tenant compte du filtre
# de la question sur l'aide de l'entourage

combiner_filtre_aide <- function(
    aide_generale,
    aide_entourage) {
  
  case_when(
    aide_generale == 0L ~ 0L,
    aide_generale == 1L &
      aide_entourage == 1L ~ 1L,
    aide_generale == 1L &
      aide_entourage == 0L ~ 0L,
    TRUE ~ NA_integer_
  )
}

# 3. Construire les indicateurs

base_solidarites <- mfv_natifs %>%
  mutate(
    
    aide_administrative =
      combiner_filtre_aide(
        recoder_oui_non(ANFR_ADMI),
        recoder_oui_non(ANFR_ADMIFAM)
      ),
    
    aide_menagere =
      combiner_filtre_aide(
        recoder_oui_non(ANFR_MEN),
        recoder_oui_non(ANFR_MENFAM)
      ),
    
    aide_soins =
      combiner_filtre_aide(
        recoder_oui_non(ANFR_QUOTI),
        recoder_oui_non(ANFR_QUOTIFAM)
      ),
    
    aide_financiere =
      recoder_oui_non(SFR_EXIST)
  )

# 4. Définir les indicateurs

indicateurs_solidarites <- c(
  "aide_administrative",
  "aide_menagere",
  "aide_soins",
  "aide_financiere"
)

# 5. Fonction de calcul des pourcentages
# pondérés avec protection des petits effectifs

calculer_solidarite <- function(variable) {
  
  base_solidarites %>%
    filter(
      !is.na(profil),
      !is.na(.data[[variable]]),
      !is.na(PFLIN),
      PFLIN > 0
    ) %>%
    group_by(profil) %>%
    summarise(
      
      n_oui = sum(.data[[variable]] == 1),
      n_non = sum(.data[[variable]] == 0),
      
      pourcentage = 100 * weighted.mean(
        .data[[variable]],
        w = PFLIN
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      indicateur = variable,
      
      pourcentage = if_else(
        n_oui >= 10 & n_non >= 10,
        round(pourcentage, 2),
        NA_real_
      )
    ) %>%
    select(
      indicateur,
      profil,
      pourcentage
    )
}

# 6. Calculer les résultats

tableau_solidarites <- bind_rows(
  lapply(
    indicateurs_solidarites,
    calculer_solidarite
  )
)

# 7. Afficher le tableau

print(
  tableau_solidarites,
  n = Inf,
  width = Inf
)



# ============================================
# ÉTAPE 81 - COMPOSITION RELATIONNELLE
# DE L'AIDE ADMINISTRATIVE
# ============================================

library(dplyr)
library(haven)

# 1. Variables détaillant les aidants
# pour les tâches administratives

cols_aidants <- c(
  "ANFR_ADMIFAMRP1",
  "ANFR_ADMIFAMRP2",
  "ANFR_ADMIFAMRP3"
)

stopifnot(
  all(cols_aidants %in% names(base_solidarites))
)

# 2. Transformer les codes en matrice
# sans afficher les observations individuelles

mat_aidants <- do.call(
  cbind,
  lapply(
    cols_aidants,
    function(v) {
      as.character(
        haven::zap_labels(
          base_solidarites[[v]]
        )
      )
    }
  )
)

# 3. Identifier les codes valides

codes_famille <- as.character(1:21)
code_relations <- "22"
code_autre <- "23"

codes_valides <- as.character(1:23)


# ============================================
# ÉTAPE 81 - CORRECTION DE LA PARTIE 4
# ============================================

# Vérifier que les données initiales
# sont bien organisées en matrice

print(dim(mat_aidants))

# Fonction conservant les dimensions
# de la matrice après utilisation de %in%

compter_codes <- function(matrice, codes) {
  
  correspondances <- matrix(
    matrice %in% codes,
    nrow = nrow(matrice),
    ncol = ncol(matrice)
  )
  
  rowSums(correspondances) > 0
}

# Au moins un aidant identifiable
# par une catégorie de relation

base_solidarites$aidant_detail_valide <-
  compter_codes(
    mat_aidants,
    codes_valides
  )

# Au moins un aidant familial

base_solidarites$aidant_familial <-
  compter_codes(
    mat_aidants,
    codes_famille
  )

# Au moins un ami, voisin,
# collègue ou autre relation

base_solidarites$aidant_relation <-
  compter_codes(
    mat_aidants,
    code_relations
  )

# Au moins un aidant classé
# dans la catégorie "Autre personne"

base_solidarites$aidant_autre <-
  compter_codes(
    mat_aidants,
    code_autre
  )

cat("Indicateurs relationnels créés correctement.\n")


# 5. Conserver les bénéficiaires
# d'une aide administrative dont
# au moins un aidant est décrit

base_aidants_admin <- base_solidarites %>%
  filter(
    aide_administrative == 1,
    aidant_detail_valide,
    !is.na(profil),
    !is.na(PFLIN),
    PFLIN > 0
  )

# 6. Fonction de calcul sécurisé
# des pourcentages pondérés

calculer_type_aidant <- function(variable) {
  
  base_aidants_admin %>%
    group_by(profil) %>%
    summarise(
      n_oui = sum(.data[[variable]]),
      n_non = sum(!.data[[variable]]),
      
      pourcentage = 100 * weighted.mean(
        as.integer(.data[[variable]]),
        w = PFLIN
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      type_aidant = variable,
      
      pourcentage = if_else(
        n_oui >= 10 & n_non >= 10,
        round(pourcentage, 2),
        NA_real_
      )
    ) %>%
    select(
      type_aidant,
      profil,
      pourcentage
    )
}

# 7. Produire le tableau

types_aidants <- c(
  "aidant_familial",
  "aidant_relation",
  "aidant_autre"
)

tableau_types_aidants <- bind_rows(
  lapply(
    types_aidants,
    calculer_type_aidant
  )
)

print(
  tableau_types_aidants,
  n = Inf,
  width = Inf
)

# 8. Vérifier la couverture des détails
# parmi les bénéficiaires déclarés

diagnostic_couverture_aidants <-
  base_solidarites %>%
  filter(
    aide_administrative == 1,
    !is.na(profil)
  ) %>%
  group_by(profil) %>%
  summarise(
    n_detail = sum(aidant_detail_valide),
    n_sans_detail = sum(!aidant_detail_valide),
    
    couverture = round(
      100 * mean(aidant_detail_valide),
      1
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    couverture = if_else(
      n_detail >= 10 & n_sans_detail >= 10,
      couverture,
      NA_real_
    )
  ) %>%
  select(profil, couverture)

print(diagnostic_couverture_aidants)


# ============================================
# ÉTAPE 82 - COMPOSITION FAMILIALE
# DE L'AIDE ADMINISTRATIVE
# ============================================

library(dplyr)
library(haven)

# 1. Reconstituer les codes uniquement
# pour les bénéficiaires ayant un aidant décrit

mat_admin_82 <- do.call(
  cbind,
  lapply(
    cols_aidants,
    function(v) {
      as.character(
        haven::zap_labels(
          base_aidants_admin[[v]]
        )
      )
    }
  )
)

# 2. Fonction détectant au moins
# un aidant de la catégorie demandée

presence_type <- function(codes) {
  
  correspondances <- matrix(
    mat_admin_82 %in% as.character(codes),
    nrow = nrow(mat_admin_82),
    ncol = ncol(mat_admin_82)
  )
  
  rowSums(correspondances) > 0
}

# 3. Regrouper les catégories de proches
# selon le dictionnaire MFV2

base_admin_82 <- base_aidants_admin %>%
  mutate(
    
    conjoint_descendants =
      presence_type(
        c(1, 8:11, 14:15)
      ),
    
    parents_beaux_parents =
      presence_type(2:7),
    
    fratrie_famille_elargie =
      presence_type(
        c(12:13, 16:21)
      ),
    
    relations_extrafamiliales =
      presence_type(22)
  )

# 4. Tableau pondéré avec protection
# des petits effectifs

categories_82 <- c(
  "conjoint_descendants",
  "parents_beaux_parents",
  "fratrie_famille_elargie",
  "relations_extrafamiliales"
)

calcul_82 <- function(variable) {
  
  base_admin_82 %>%
    group_by(profil) %>%
    summarise(
      
      n_oui = sum(.data[[variable]]),
      n_non = sum(!.data[[variable]]),
      
      pct = 100 * weighted.mean(
        as.integer(.data[[variable]]),
        w = PFLIN
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      categorie = variable,
      
      pct = if_else(
        n_oui >= 10 & n_non >= 10,
        round(pct, 2),
        NA_real_
      )
    ) %>%
    select(
      categorie,
      profil,
      pct
    )
}

tableau_famille_82 <- bind_rows(
  lapply(
    categories_82,
    calcul_82
  )
)

print(
  tableau_famille_82,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 83 - AIDE ADMINISTRATIVE
# ET PARCOURS MIGRATOIRE
# ANALYSE AJUSTÉE EXPLORATOIRE
# ============================================

library(dplyr)

# 1. Vérifier les variables nécessaires

variables_83 <- c(
  "ANFR_ADMI",
  "ANFR_ADMIFAM",
  "profil",
  "diplome_4",
  "age_c",
  "age_c2",
  "sexe",
  "territoire",
  "origine_sociale",
  "PFLIN"
)

manquantes_83 <- setdiff(
  variables_83,
  names(plan_mfv_age$variables)
)

if (length(manquantes_83) > 0) {
  stop(
    "Variables absentes : ",
    paste(manquantes_83, collapse = ", ")
  )
}

# 2. Reconstituer l'indicateur d'aide
# avec la fonction de l'étape 80

base_83 <- plan_mfv_age$variables %>%
  mutate(
    aide_administrative =
      combiner_filtre_aide(
        recoder_oui_non(ANFR_ADMI),
        recoder_oui_non(ANFR_ADMIFAM)
      )
  ) %>%
  filter(
    !is.na(aide_administrative),
    !is.na(profil),
    PFLIN > 0,
    if_all(
      all_of(c(
        "diplome_4",
        "age_c",
        "age_c2",
        "sexe",
        "territoire",
        "origine_sociale"
      )),
      ~ !is.na(.x)
    )
  ) %>%
  mutate(
    poids_modele = PFLIN / mean(PFLIN)
  )

# 3. Modèle logistique pondéré
# Variance non validée : exploration seule

modele_admin_83 <- glm(
  aide_administrative ~
    profil +
    diplome_4 +
    age_c +
    age_c2 +
    sexe +
    territoire +
    origine_sociale,
  data = base_83,
  weights = poids_modele,
  family = quasibinomial()
)

# 4. Vérifier la convergence
# sans afficher de données individuelles

cat(
  "Convergence :",
  modele_admin_83$converged,
  "\n"
)

# 5. Calculer les probabilités ajustées
# standardisées sur la même population

profils_83 <- levels(
  droplevels(base_83$profil)
)

resultats_83 <- lapply(
  profils_83,
  function(p) {
    
    base_prediction <- base_83
    base_prediction$profil <- factor(
      p,
      levels = levels(base_83$profil)
    )
    
    probabilites <- predict(
      modele_admin_83,
      newdata = base_prediction,
      type = "response"
    )
    
    data.frame(
      profil = p,
      probabilite_ajustee = round(
        100 * weighted.mean(
          probabilites,
          w = base_83$PFLIN
        ),
        2
      )
    )
  }
) %>%
  bind_rows()

# 6. Vérification des effectifs par profil
# et protection des résultats

controle_83 <- base_83 %>%
  group_by(profil) %>%
  summarise(
    publiable =
      sum(aide_administrative == 1) >= 10 &
      sum(aide_administrative == 0) >= 10,
    .groups = "drop"
  )

resultats_83 <- resultats_83 %>%
  left_join(
    controle_83,
    by = "profil"
  ) %>%
  mutate(
    probabilite_ajustee = if_else(
      publiable,
      probabilite_ajustee,
      NA_real_
    )
  ) %>%
  select(
    profil,
    probabilite_ajustee
  )

print(resultats_83)


# ============================================
# ÉTAPE 84 - COMPARAISON À ÉCHANTILLON
# CONSTANT : TAUX OBSERVÉS ET AJUSTÉS
# ============================================

library(dplyr)

# 1. Calculer les taux observés
# sur l'échantillon du modèle 83

taux_bruts_84 <- base_83 %>%
  group_by(profil) %>%
  summarise(
    
    n_oui = sum(
      aide_administrative == 1
    ),
    
    n_non = sum(
      aide_administrative == 0
    ),
    
    taux_observe = 100 * weighted.mean(
      aide_administrative,
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    publiable =
      n_oui >= 10 & n_non >= 10,
    
    taux_observe = if_else(
      publiable,
      round(taux_observe, 2),
      NA_real_
    )
  ) %>%
  select(
    profil,
    taux_observe,
    publiable
  )

# 2. Ajouter les probabilités ajustées

comparaison_84 <- taux_bruts_84 %>%
  left_join(
    resultats_83,
    by = "profil"
  ) %>%
  mutate(
    
    probabilite_ajustee = if_else(
      publiable,
      probabilite_ajustee,
      NA_real_
    ),
    
    difference_ajustement = round(
      probabilite_ajustee - taux_observe,
      2
    )
  ) %>%
  select(
    profil,
    taux_observe,
    probabilite_ajustee,
    difference_ajustement
  )

print(
  comparaison_84,
  width = Inf
)

# 3. Vérification qualitative du champ
# sans afficher les effectifs détaillés

cat(
  "\nModèle convergé :",
  modele_admin_83$converged,
  "\n"
)


# ============================================
# ÉTAPE 85 - VARIABLES RELATIONNELLES
# ET HISTOIRE MIGRATOIRE
# ============================================

library(dplyr)

# 1. Variables repérées dans le questionnaire

variables_85 <- c(
  "MRD_FREQUENT",
  "MRD_LVARRIV",
  "MRT_RECU",
  "MRT_RP1",
  "MRT_RP2",
  "MRT_RTYP",
  "MRT_DON",
  "MRTD_RP1",
  "MRTD_RP2",
  "MRR_RAIS1",
  "MRR_RAIS2",
  "MRR_DIF1",
  "MRR_DIF2",
  "SFR_EXIST",
  "SFR_PERS_1",
  "ANFR_ADMIFAMRP1"
)

# 2. Vérifier leur présence et intitulé

dictionnaire_85 <- data.frame(
  variable = variables_85,
  presente = variables_85 %in% names(ego)
)

dictionnaire_85$intitule <- vapply(
  variables_85,
  function(v) {
    
    if (!v %in% names(ego)) {
      return("Absente du fichier EGO")
    }
    
    lib <- attr(ego[[v]], "label")
    
    if (is.null(lib)) {
      return("Intitulé non renseigné")
    }
    
    as.character(lib)[1]
  },
  character(1)
)

print(
  dictionnaire_85,
  row.names = FALSE,
  right = FALSE
)


# ============================================
# ÉTAPE 86 - COUVERTURE DES VARIABLES
# RELATIONNELLES CHEZ LES NATIFS DE RETOUR
# ============================================

library(dplyr)
library(haven)

# 1. Conserver les natifs de retour

base_retour_86 <- mfv_natifs %>%
  filter(
    as.character(profil) == "Natifs de retour",
    !is.na(PFLIN),
    PFLIN > 0
  )

# 2. Variables et modalités valides
# d'après le questionnaire MFV2

codes_86 <- list(
  MRD_FREQUENT = 1:4,
  MRD_LVARRIV = 1:6,
  MRT_RECU = 1:2,
  MRT_RP1 = 1:23,
  MRT_DON = 1:2,
  MRR_RAIS1 = 1:14,
  MRR_DIF1 = 1:9,
  SFR_EXIST = 1:2,
  SFR_PERS_1 = 1:23,
  ANFR_ADMIFAMRP1 = 1:23
)

stopifnot(
  all(names(codes_86) %in%
        names(base_retour_86))
)

# 3. Fonction de couverture
# avec protection des petits effectifs

couverture_86 <- function(v) {
  
  valeurs <- as.character(
    haven::zap_labels(
      base_retour_86[[v]]
    )
  )
  
  valide <- valeurs %in%
    as.character(codes_86[[v]])
  
  n_valide <- sum(valide)
  n_absent <- sum(!valide)
  
  pct <- if (
    n_valide >= 10 &&
    n_absent >= 10
  ) {
    round(
      100 * weighted.mean(
        as.integer(valide),
        w = base_retour_86$PFLIN
      ),
      1
    )
  } else {
    NA_real_
  }
  
  data.frame(
    variable = v,
    couverture_pct = pct,
    couverture_affichable =
      n_valide >= 10 &&
      n_absent >= 10
  )
}

# 4. Résultats agrégés

diagnostic_86 <- bind_rows(
  lapply(
    names(codes_86),
    couverture_86
  )
)

print(
  diagnostic_86,
  row.names = FALSE
)

# 5. Afficher les dictionnaires
# de deux variables centrales

cat("\nMRD_FREQUENT :\n")
print(
  attr(
    ego$MRD_FREQUENT,
    "labels"
  )
)

cat("\nMRT_RECU :\n")
print(
  attr(
    ego$MRT_RECU,
    "labels"
  )
)


# ============================================
# ÉTAPE 87 - RELATIONS ET SOLIDARITÉS
# PENDANT LE DERNIER SÉJOUR MIGRATOIRE
# ============================================

library(dplyr)
library(haven)

# 1. Recoder les variables du séjour

code_numerique <- function(x) {
  suppressWarnings(
    as.integer(
      as.character(haven::zap_labels(x))
    )
  )
}

base_retour_87 <- base_retour_86 %>%
  mutate(
    frequentations = code_numerique(
      MRD_FREQUENT
    ),
    aide_recue_sejour = code_numerique(
      MRT_RECU
    ),
    aide_envoyee_sejour = code_numerique(
      MRT_DON
    )
  )

# 2. Composition des fréquentations
# durant le dernier séjour long

frequentations_87 <- base_retour_87 %>%
  filter(frequentations %in% 1:4) %>%
  mutate(
    categorie = factor(
      frequentations,
      levels = 1:4,
      labels = c(
        "Personnes du lieu de migration",
        "Personnes du DROM d'origine",
        "Autant les deux",
        "Ni les unes ni les autres"
      )
    )
  ) %>%
  group_by(categorie) %>%
  summarise(
    n_interne = n(),
    poids = sum(PFLIN),
    .groups = "drop"
  )

# Ne publier la répartition complète
# que si toutes les catégories sont
# suffisamment représentées

if (
  nrow(frequentations_87) == 4 &&
  all(frequentations_87$n_interne >= 10)
) {
  
  tableau_frequentations_87 <-
    frequentations_87 %>%
    mutate(
      pourcentage = round(
        100 * poids / sum(poids),
        1
      )
    ) %>%
    select(categorie, pourcentage)
  
  print(tableau_frequentations_87)
  
} else {
  cat(
    "Répartition détaillée masquée :",
    "au moins une catégorie est trop petite.\n"
  )
}

# 3. Fonction de proportion pondérée
# pour les questions Oui / Non

proportion_oui_87 <- function(data, variable) {
  
  base <- data %>%
    filter(.data[[variable]] %in% 1:2)
  
  n_oui <- sum(base[[variable]] == 1)
  n_non <- sum(base[[variable]] == 2)
  
  if (n_oui < 10 || n_non < 10) {
    return(NA_real_)
  }
  
  round(
    100 * weighted.mean(
      as.integer(base[[variable]] == 1),
      w = base$PFLIN
    ),
    1
  )
}

# 4. Transferts financiers pendant le séjour

tableau_transferts_87 <- data.frame(
  indicateur = c(
    "Aide financière familiale reçue",
    "Aide financière envoyée à la famille"
  ),
  
  pourcentage = c(
    proportion_oui_87(
      base_retour_87,
      "aide_recue_sejour"
    ),
    
    proportion_oui_87(
      base_retour_87,
      "aide_envoyee_sejour"
    )
  )
)

print(
  tableau_transferts_87,
  row.names = FALSE
)


# ============================================
# ÉTAPE 88 - FRÉQUENTATIONS SOCIALES
# ET AIDE FAMILIALE PENDANT LA MIGRATION
# ============================================

library(dplyr)

# 1. Construire un échantillon commun
# aux deux questions

base_88 <- base_retour_87 %>%
  filter(
    frequentations %in% 1:4,
    aide_recue_sejour %in% 1:2,
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    
    type_frequentation = factor(
      frequentations,
      levels = 1:4,
      labels = c(
        "Lieu de migration",
        "DROM d'origine",
        "Autant les deux",
        "Ni l'un ni l'autre"
      )
    ),
    
    aide_familiale = as.integer(
      aide_recue_sejour == 1
    )
  )

# 2. Calculer la proportion pondérée
# d'aide familiale reçue dans chaque groupe

resultats_88 <- base_88 %>%
  group_by(type_frequentation) %>%
  summarise(
    
    n_oui = sum(aide_familiale == 1),
    n_non = sum(aide_familiale == 0),
    
    pct_aide = 100 * weighted.mean(
      aide_familiale,
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    pourcentage = if_else(
      n_oui >= 10 & n_non >= 10,
      round(pct_aide, 1),
      NA_real_
    )
    
  ) %>%
  select(
    type_frequentation,
    pourcentage
  )

# 3. Afficher les résultats protégés

print(
  resultats_88,
  n = Inf,
  width = Inf
)

# 4. Calculer la proportion totale
# sur exactement le même échantillon

n_oui_total <- sum(
  base_88$aide_familiale == 1
)

n_non_total <- sum(
  base_88$aide_familiale == 0
)

pct_total_88 <- if (
  n_oui_total >= 10 &&
  n_non_total >= 10
) {
  
  round(
    100 * weighted.mean(
      base_88$aide_familiale,
      w = base_88$PFLIN
    ),
    1
  )
  
} else {
  NA_real_
}

cat(
  "\nAide familiale reçue dans",
  "l'échantillon commun :",
  pct_total_88,
  "%\n"
)


# ============================================
# ÉTAPE 89 - QUI APPORTE UNE AIDE
# FINANCIÈRE PENDANT LA MIGRATION ?
# ============================================

library(dplyr)
library(haven)

# 1. Conserver les bénéficiaires
# d'une aide familiale pendant le séjour

base_89 <- base_retour_87 %>%
  filter(
    aide_recue_sejour == 1,
    !is.na(PFLIN),
    PFLIN > 0
  )

# 2. Identifier les deux aidants principaux

variables_aidants_89 <- c(
  "MRT_RP1",
  "MRT_RP2"
)

mat_89 <- do.call(
  cbind,
  lapply(
    variables_aidants_89,
    function(v) {
      code_numerique(base_89[[v]])
    }
  )
)

# 3. Fonction : présence d'un aidant
# appartenant à une catégorie

presence_89 <- function(codes) {
  
  correspondances <- matrix(
    mat_89 %in% codes,
    nrow = nrow(mat_89),
    ncol = ncol(mat_89)
  )
  
  rowSums(correspondances) > 0
}

# 4. Construire les catégories d'aidants

base_89 <- base_89 %>%
  mutate(
    
    aidant_decrit = presence_89(1:23),
    
    conjoint = presence_89(1),
    
    parents_beaux_parents =
      presence_89(2:7),
    
    enfants_petits_enfants =
      presence_89(c(8:11, 14:15)),
    
    famille_elargie =
      presence_89(c(12:13, 16:21)),
    
    relations_autres =
      presence_89(22:23)
  )

# 5. Conserver les bénéficiaires
# avec au moins un aidant décrit

base_aidants_89 <- base_89 %>%
  filter(aidant_decrit)

# 6. Fonction de pourcentage pondéré
# avec seuil de confidentialité

calcul_89 <- function(variable) {
  
  oui <- sum(base_aidants_89[[variable]])
  non <- sum(!base_aidants_89[[variable]])
  
  pct <- if (
    oui >= 10 && non >= 10
  ) {
    round(
      100 * weighted.mean(
        as.integer(
          base_aidants_89[[variable]]
        ),
        w = base_aidants_89$PFLIN
      ),
      1
    )
  } else {
    NA_real_
  }
  
  data.frame(
    categorie = variable,
    pourcentage = pct
  )
}

# 7. Produire le tableau

categories_89 <- c(
  "conjoint",
  "parents_beaux_parents",
  "enfants_petits_enfants",
  "famille_elargie",
  "relations_autres"
)

tableau_aidants_89 <- bind_rows(
  lapply(categories_89, calcul_89)
)

print(
  tableau_aidants_89,
  row.names = FALSE
)

# 8. Vérifier la couverture des aidants
# parmi les bénéficiaires de l'aide

n_decrit <- sum(base_89$aidant_decrit)
n_sans_detail <- sum(!base_89$aidant_decrit)

couverture_89 <- if (
  n_decrit >= 10 &&
  n_sans_detail >= 10
) {
  round(
    100 * weighted.mean(
      as.integer(base_89$aidant_decrit),
      w = base_89$PFLIN
    ),
    1
  )
} else {
  NA_real_
}

cat(
  "\nCouverture des aidants décrits :",
  couverture_89,
  "% (NA = résultat masqué)\n"
)


# ============================================
# ÉTAPE 90 - MOTIF DE MIGRATION
# ET AIDE FINANCIÈRE FAMILIALE
# ============================================

library(dplyr)
library(haven)

# 1. Rechercher les variables liées
# au motif du séjour migratoire

variables_motif <- grep(
  "^MR_RAIS",
  names(base_retour_87),
  value = TRUE,
  ignore.case = TRUE
)

print(variables_motif)

# 2. Identifier la variable correspondant
# au dernier séjour migratoire

variable_motif_90 <- grep(
  "^MR_RAIS_1$",
  names(base_retour_87),
  value = TRUE,
  ignore.case = TRUE
)

if (length(variable_motif_90) != 1) {
  stop(
    "Variable du dernier séjour à vérifier ",
    "dans la liste affichée ci-dessus."
  )
}

# 3. Vérifier son intitulé et ses modalités

cat("\nIntitulé :\n")
print(
  attr(
    base_retour_87[[variable_motif_90]],
    "label"
  )
)

cat("\nModalités :\n")
print(
  attr(
    base_retour_87[[variable_motif_90]],
    "labels"
  )
)


# ============================================
# ÉTAPE 90 - SUITE
# COMPARAISON PONDÉRÉE
# ============================================

# 4. Construire l'échantillon commun

base_90 <- base_retour_87 %>%
  mutate(
    motif_sejour = code_numerique(
      .data[[variable_motif_90]]
    ),
    
    aide_familiale = code_numerique(
      MRT_RECU
    )
  ) %>%
  filter(
    motif_sejour %in% 1:3,
    aide_familiale %in% 1:2,
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    motif = factor(
      motif_sejour,
      levels = 1:3,
      labels = c(
        "Études",
        "Emploi / recherche d'emploi",
        "Formation professionnelle"
      )
    ),
    
    aide_recue = as.integer(
      aide_familiale == 1
    )
  )

# 5. Comparer la réception
# d'une aide financière familiale

tableau_90 <- base_90 %>%
  group_by(motif) %>%
  summarise(
    
    n_oui = sum(aide_recue == 1),
    n_non = sum(aide_recue == 0),
    
    taux = 100 * weighted.mean(
      aide_recue,
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    pourcentage = if_else(
      n_oui >= 10 & n_non >= 10,
      round(taux, 1),
      NA_real_
    )
  ) %>%
  select(
    motif,
    pourcentage
  )

# 6. Afficher les résultats

print(
  tableau_90,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 91 - MOTIF DU SÉJOUR ET
# SITUATION D'ACTIVITÉ PENDANT LA MIGRATION
# ============================================

library(dplyr)
library(haven)

# 1. Vérifier la présence de la variable

stopifnot(
  "MRA_ACTI" %in% names(base_90)
)

# 2. Examiner le dictionnaire

cat("\nIntitulé :\n")
print(attr(ego$MRA_ACTI, "label"))

cat("\nModalités :\n")
print(attr(ego$MRA_ACTI, "labels"))

# 3. Construire la variable d'activité

base_91 <- base_90 %>%
  mutate(
    activite_sejour = code_numerique(
      MRA_ACTI
    )
  )

# 4. Diagnostic agrégé par motif de départ
# Modalités d'activité regroupées :
# 1 = emploi
# 3 = études
# autres modalités valides = autres situations

diagnostic_91 <- base_91 %>%
  mutate(
    situation = case_when(
      activite_sejour == 1 ~ "Principalement en emploi",
      activite_sejour == 3 ~ "Principalement en études",
      activite_sejour %in% c(2, 4:7) ~
        "Autre situation",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(situation)) %>%
  group_by(motif, situation) %>%
  summarise(
    n_interne = n(),
    poids = sum(PFLIN),
    .groups = "drop"
  ) %>%
  group_by(motif) %>%
  mutate(
    publiable = all(n_interne >= 10),
    pourcentage = if_else(
      publiable,
      round(100 * poids / sum(poids), 1),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  select(
    motif,
    situation,
    pourcentage
  )

print(
  diagnostic_91,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 92 - AIDE FINANCIÈRE FAMILIALE
# SELON L'ACTIVITÉ PENDANT LE SÉJOUR
# ============================================

library(dplyr)

# 1. Conserver l'échantillon de l'étape 90
# et regrouper les situations d'activité

base_92 <- base_90 %>%
  mutate(
    activite_sejour = code_numerique(MRA_ACTI),
    
    situation_92 = case_when(
      activite_sejour == 1 ~
        "Principalement en emploi",
      
      activite_sejour %in% c(2, 3) ~
        "Études, formation ou apprentissage",
      
      activite_sejour %in% 4:7 ~
        "Autres situations",
      
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(situation_92))

# 2. Calculer l'aide reçue selon
# l'activité principale pendant le séjour

tableau_92 <- base_92 %>%
  group_by(situation_92) %>%
  summarise(
    n_oui = sum(aide_recue == 1),
    n_non = sum(aide_recue == 0),
    
    taux = 100 * weighted.mean(
      aide_recue,
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    pourcentage = if_else(
      n_oui >= 10 & n_non >= 10,
      round(taux, 1),
      NA_real_
    )
  ) %>%
  select(
    situation_92,
    pourcentage
  )

# 3. Afficher les résultats protégés

print(
  tableau_92,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 93 - PRÉPARER L'ANALYSE
# DE L'ÂGE AU DÉPART ET DE L'ORIGINE SOCIALE
# ============================================

library(dplyr)

# 1. Rechercher les variables de date
# du dernier séjour migratoire

variables_dates_93 <- grep(
  "^MR_.*(ADEP|MDEP)|^ANAIS$|^ANNAIS$",
  names(base_92),
  value = TRUE,
  ignore.case = TRUE
)

cat("\nVariables de dates disponibles :\n")
print(variables_dates_93)

# 2. Afficher les intitulés correspondants

dictionnaire_dates_93 <- data.frame(
  variable = variables_dates_93,
  
  intitule = vapply(
    variables_dates_93,
    function(v) {
      lib <- attr(base_92[[v]], "label")
      
      if (is.null(lib)) {
        return("Intitulé absent")
      }
      
      as.character(lib)[1]
    },
    character(1)
  )
)

print(
  dictionnaire_dates_93,
  row.names = FALSE
)

# 3. Vérifier la présence de l'origine sociale
# dans nos différentes bases

controle_origine_93 <- data.frame(
  base = c(
    "base_92",
    "mfv_natifs",
    "plan_mfv_age"
  ),
  
  origine_sociale_presente = c(
    "origine_sociale" %in% names(base_92),
    "origine_sociale" %in% names(mfv_natifs),
    "origine_sociale" %in%
      names(plan_mfv_age$variables)
  )
)

print(
  controle_origine_93,
  row.names = FALSE
)

# 4. Vérifier si une variable d'âge
# au départ est déjà disponible

variables_age_depart_93 <- grep(
  "AGE.*DEP|AGE.*MIG|AGE.*PART",
  names(base_92),
  value = TRUE,
  ignore.case = TRUE
)

cat("\nVariables potentielles d'âge au départ :\n")
print(variables_age_depart_93)


# ============================================
# ÉTAPE 94 - ÂGE AU DÉPART
# ET ORIGINE SOCIALE
# ============================================

library(dplyr)
library(haven)

# 1. Examiner la variable d'âge au départ

cat("\nÂGE AU DÉPART :\n")

print(
  attr(
    base_92$AGEDEP_1_rec,
    "label"
  )
)

cat("\nModalités éventuelles :\n")

print(
  attr(
    base_92$AGEDEP_1_rec,
    "labels"
  )
)

cat("\nType de variable :\n")

print(class(base_92$AGEDEP_1_rec))

# 2. Examiner l'année de départ

cat("\nANNÉE DU DÉPART :\n")

print(
  attr(
    base_92$MR_ADEP_1_rec,
    "label"
  )
)

print(
  attr(
    base_92$MR_ADEP_1_rec,
    "labels"
  )
)

# 3. Vérifier l'origine sociale
# dans la base des modèles précédents

cat("\nORIGINE SOCIALE :\n")

print(
  class(
    plan_mfv_age$variables$origine_sociale
  )
)

print(
  levels(
    factor(
      plan_mfv_age$variables$origine_sociale
    )
  )
)

# 4. Identifier les clés communes
# permettant de retrouver les mêmes individus

ids_94 <- intersect(
  c("IDENT_EGO", "NUMFA"),
  intersect(
    names(base_92),
    names(plan_mfv_age$variables)
  )
)

cat("\nIdentifiants communs disponibles :\n")
print(ids_94)

# 5. Contrôler l'unicité de ces identifiants
# sans afficher leurs valeurs confidentielles

controle_ids_94 <- bind_rows(
  lapply(
    ids_94,
    function(v) {
      
      x <- as.character(base_92[[v]])
      y <- as.character(
        plan_mfv_age$variables[[v]]
      )
      
      data.frame(
        identifiant = v,
        
        unique_base_92 =
          !anyNA(x) &&
          !anyDuplicated(x),
        
        unique_plan =
          !anyNA(y) &&
          !anyDuplicated(y),
        
        correspondance_complete =
          !anyNA(x) &&
          all(x %in% y)
      )
    }
  )
)

print(
  controle_ids_94,
  row.names = FALSE
)


# ============================================
# ÉTAPE 95 - AIDE FINANCIÈRE
# SELON L'ÂGE AU DÉPART
# ============================================

library(dplyr)
library(haven)

# 1. Reprendre l'échantillon de l'étape 90

base_95 <- base_90 %>%
  mutate(
    age_depart_code = code_numerique(
      AGEDEP_1_rec
    )
  ) %>%
  filter(age_depart_code %in% 1:4) %>%
  mutate(
    age_depart = factor(
      age_depart_code,
      levels = 1:4,
      labels = c(
        "Moins de 18 ans",
        "18 à 22 ans",
        "23 à 29 ans",
        "30 ans et plus"
      )
    )
  )

# 2. Calculer les proportions pondérées
# d'aide familiale reçue par âge au départ

tableau_age_95 <- base_95 %>%
  group_by(age_depart) %>%
  summarise(
    n_oui = sum(aide_recue == 1),
    n_non = sum(aide_recue == 0),
    
    taux = 100 * weighted.mean(
      aide_recue,
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    pourcentage = if_else(
      n_oui >= 10 & n_non >= 10,
      round(taux, 1),
      NA_real_
    )
  ) %>%
  select(
    age_depart,
    pourcentage
  )

print(
  tableau_age_95,
  n = Inf,
  width = Inf
)

# ============================================
# 3. VÉRIFIER L'APPARIEMENT DES BASES
# ============================================

# Identifiants sans afficher leurs valeurs

ids_base <- as.character(
  base_92$IDENT_EGO
)

ids_plan <- as.character(
  plan_mfv_age$variables$IDENT_EGO
)

# Correspondance des individus

apparies <- ids_base %in% ids_plan

nb_apparies <- sum(apparies)
nb_non_apparies <- sum(!apparies)

# Protection des résultats agrégés

taux_correspondance <- if (
  nb_apparies >= 10 &&
  nb_non_apparies >= 10
) {
  round(
    100 * mean(apparies),
    1
  )
} else {
  NA_real_
}

diagnostic_jointure_95 <- data.frame(
  identifiant = "IDENT_EGO",
  
  correspondance_complete =
    all(apparies),
  
  correspondance_pct =
    taux_correspondance
)

print(
  diagnostic_jointure_95,
  row.names = FALSE
)


# ============================================
# ÉTAPE 96 - AIDE FINANCIÈRE FAMILIALE
# SELON LE MOTIF, AJUSTÉE SUR L'ÂGE
# ============================================

library(dplyr)

# 1. Construire un échantillon commun

base_96 <- base_95 %>%
  filter(
    !is.na(motif),
    !is.na(age_depart),
    !is.na(aide_recue),
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    motif = droplevels(motif),
    age_depart = droplevels(age_depart),
    poids_modele = PFLIN / mean(PFLIN)
  )

# 2. Vérifier la présence
# des trois motifs et des quatre âges

stopifnot(
  nlevels(base_96$motif) == 3,
  nlevels(base_96$age_depart) == 4
)

# 3. Vérifier si les croisements
# motif x âge ont suffisamment d'observations
# Sans afficher les effectifs confidentiels

cellules_96 <- table(
  base_96$motif,
  base_96$age_depart
)

cat(
  "Toutes les cellules motif x âge",
  "ont au moins 10 observations :",
  all(cellules_96 >= 10),
  "\n"
)

# 4. Modèle logistique exploratoire
# Sans inférence sur les coefficients

modele_96 <- glm(
  aide_recue ~ motif + age_depart,
  data = base_96,
  weights = poids_modele,
  family = quasibinomial()
)

cat(
  "Convergence du modèle :",
  modele_96$converged,
  "\n"
)

# 5. Probabilités ajustées
# Standardisation sur le même échantillon

resultats_96 <- bind_rows(
  lapply(
    levels(base_96$motif),
    function(m) {
      
      groupe <- base_96 %>%
        filter(motif == m)
      
      # Taux observé dans le groupe
      
      taux_observe <- 100 * weighted.mean(
        groupe$aide_recue,
        w = groupe$PFLIN
      )
      
      # Appliquer le même motif
      # à tous les individus pour standardiser
      
      nouvelles_donnees <- base_96
      
      nouvelles_donnees$motif <- factor(
        m,
        levels = levels(base_96$motif)
      )
      
      prediction <- predict(
        modele_96,
        newdata = nouvelles_donnees,
        type = "response"
      )
      
      taux_ajuste <- 100 * weighted.mean(
        prediction,
        w = base_96$PFLIN
      )
      
      # Protection des résultats agrégés
      
      publiable <-
        sum(groupe$aide_recue == 1) >= 10 &&
        sum(groupe$aide_recue == 0) >= 10
      
      data.frame(
        motif = m,
        
        taux_observe = if (publiable) {
          round(taux_observe, 1)
        } else {
          NA_real_
        },
        
        probabilite_ajustee = if (publiable) {
          round(taux_ajuste, 1)
        } else {
          NA_real_
        }
      )
    }
  )
)

# 6. Afficher uniquement les résultats protégés

print(
  resultats_96,
  row.names = FALSE
)



# ============================================
# ÉTAPE 97 - COMPARAISON DES AIDES
# FAMILIALES À ÂGE AU DÉPART IDENTIQUE
# ============================================

library(dplyr)
library(tidyr)

# 1. Conserver les départs pour études
# et pour emploi uniquement

base_97 <- base_96 %>%
  filter(
    as.character(motif) %in% c(
      "Études",
      "Emploi / recherche d'emploi"
    )
  ) %>%
  mutate(
    motif = droplevels(motif)
  )

# 2. Calculer le pourcentage pondéré
# d'aide familiale selon l'âge et le motif

resultats_97 <- base_97 %>%
  group_by(age_depart, motif, .drop = TRUE) %>%
  summarise(
    
    n_oui = sum(aide_recue == 1),
    n_non = sum(aide_recue == 0),
    
    taux = 100 * weighted.mean(
      aide_recue,
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  complete(
    age_depart,
    motif,
    fill = list(
      n_oui = 0,
      n_non = 0,
      taux = NA_real_
    )
  )

# 3. Protéger les petits effectifs
# et conserver uniquement les classes d'âge
# où les deux motifs sont comparables

resultats_97 <- resultats_97 %>%
  group_by(age_depart) %>%
  mutate(
    
    comparaison_possible = all(
      n_oui >= 10 & n_non >= 10
    ),
    
    pourcentage = if_else(
      comparaison_possible,
      round(taux, 1),
      NA_real_
    )
  ) %>%
  ungroup()

# 4. Tableau comparatif

tableau_97 <- resultats_97 %>%
  select(
    age_depart,
    motif,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = motif,
    values_from = pourcentage
  ) %>%
  arrange(age_depart)

print(
  tableau_97,
  n = Inf,
  width = Inf
)

# 5. Vérifier les classes d'âge
# réellement comparables

diagnostic_97 <- resultats_97 %>%
  distinct(
    age_depart,
    comparaison_possible
  )

print(
  diagnostic_97,
  n = Inf
)


# ============================================
# ÉTAPE 98 - RATTACHER L'ORIGINE SOCIALE
# AUX MIGRATIONS POUR ÉTUDES ET EMPLOI
# ============================================

library(dplyr)

# 1. Préparer la table de référence
# contenant l'origine sociale

origine_98 <- plan_mfv_age$variables %>%
  transmute(
    identifiant_98 = as.character(IDENT_EGO),
    origine_sociale
  )

# 2. Vérifier l'unicité des identifiants

stopifnot(
  !anyNA(origine_98$identifiant_98),
  !anyDuplicated(origine_98$identifiant_98)
)

# 3. Conserver les deux classes d'âge
# pour lesquelles la comparaison est possible

base_98 <- base_97 %>%
  filter(
    age_depart_code %in% c(2, 3)
  ) %>%
  mutate(
    identifiant_98 = as.character(IDENT_EGO)
  )

n_avant <- nrow(base_98)

# 4. Rattacher l'origine sociale
# sans créer de doublons

base_98 <- base_98 %>%
  left_join(
    origine_98,
    by = "identifiant_98",
    relationship = "many-to-one"
  )

# 5. Contrôler la jointure

cat(
  "Nombre de lignes inchangé :",
  nrow(base_98) == n_avant,
  "\n"
)

# 6. Vérifier la disponibilité
# de l'origine sociale dans chaque groupe

diagnostic_98 <- base_98 %>%
  group_by(age_depart, motif) %>%
  summarise(
    n_disponible = sum(
      !is.na(origine_sociale)
    ),
    
    n_absent = sum(
      is.na(origine_sociale)
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    couverture_pct = if_else(
      n_disponible >= 10 & n_absent >= 10,
      
      round(
        100 * n_disponible /
          (n_disponible + n_absent),
        1
      ),
      
      NA_real_
    ),
    
    couverture_90pct =
      n_disponible /
      (n_disponible + n_absent) >= 0.90
  ) %>%
  select(
    age_depart,
    motif,
    couverture_pct,
    couverture_90pct
  )

print(
  diagnostic_98,
  n = Inf,
  width = Inf
)

# 7. Vérifier les catégories sociales

cat("\nCatégories d'origine sociale :\n")

print(
  levels(base_98$origine_sociale)
)


# ============================================
# ÉTAPE 99 - ORIGINE SOCIALE ET AIDE
# FINANCIÈRE PENDANT LES MIGRATIONS
# POUR ÉTUDES
# ============================================

library(dplyr)

# 1. Conserver les natifs de retour
# partis pour études entre 18 et 29 ans

base_etudes_99 <- base_98 %>%
  filter(
    as.character(motif) == "Études",
    age_depart_code %in% c(2, 3),
    !is.na(origine_sociale),
    !is.na(aide_recue),
    !is.na(PFLIN),
    PFLIN > 0
  )

# 2. Calculer la réception d'aide
# selon les cinq origines sociales

resultats_origine_99 <- base_etudes_99 %>%
  group_by(
    origine_sociale,
    .drop = FALSE
  ) %>%
  summarise(
    
    n_oui = sum(aide_recue == 1),
    
    n_non = sum(aide_recue == 0),
    
    taux = if (n() > 0) {
      100 * weighted.mean(
        aide_recue,
        w = PFLIN
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    
    publiable =
      n_oui >= 10 & n_non >= 10,
    
    pourcentage = if_else(
      publiable,
      round(taux, 1),
      NA_real_
    )
  )

# 3. Afficher le tableau protégé
# sans publier les effectifs détaillés

tableau_origine_99 <- resultats_origine_99 %>%
  select(
    origine_sociale,
    pourcentage
  )

print(
  tableau_origine_99,
  n = Inf,
  width = Inf
)

# 4. Identifier les catégories qui peuvent
# être présentées sans petits effectifs

diagnostic_99 <- resultats_origine_99 %>%
  select(
    origine_sociale,
    publiable
  )

print(
  diagnostic_99,
  n = Inf,
  width = Inf
)

# 5. Vérifier le taux global
# sur le même échantillon

n_oui_99 <- sum(
  base_etudes_99$aide_recue == 1
)

n_non_99 <- sum(
  base_etudes_99$aide_recue == 0
)

taux_global_99 <- if (
  n_oui_99 >= 10 &&
  n_non_99 >= 10
) {
  round(
    100 * weighted.mean(
      base_etudes_99$aide_recue,
      w = base_etudes_99$PFLIN
    ),
    1
  )
} else {
  NA_real_
}

cat(
  "\nAide familiale reçue parmi les",
  "migrants étudiants de 18 à 29 ans :",
  taux_global_99,
  "%\n"
)


# ============================================
# ÉTAPE 100 - RECHERCHER LES MONTANTS
# ET LES RESSOURCES ÉCONOMIQUES DANS MFV2
# ============================================

library(haven)

# 1. Rechercher les variables pertinentes

variables_montants_100 <- grep(
  "^(MRT_|MRTD_|SFR_.*MONT|TOTREVEN|TRANCHRE|PA_CONDA)",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

# 2. Afficher leurs intitulés

dictionnaire_100 <- data.frame(
  variable = variables_montants_100,
  
  intitule = vapply(
    variables_montants_100,
    function(v) {
      lib <- attr(ego[[v]], "label")
      
      if (is.null(lib)) {
        return("Intitulé absent")
      }
      
      as.character(lib)[1]
    },
    character(1)
  ),
  
  row.names = NULL
)

print(
  dictionnaire_100,
  row.names = FALSE,
  right = FALSE
)

# 3. Examiner les variables de montant
# des aides financières actuelles

variables_sfr_montant <- grep(
  "^SFR_.*MONT",
  names(ego),
  value = TRUE,
  ignore.case = TRUE
)

for (v in variables_sfr_montant) {
  
  cat("\nVariable :", v, "\n")
  
  print(
    attr(ego[[v]], "labels")
  )
}


# ============================================
# ÉTAPE 101 - RÉGULARITÉ DES AIDES
# FINANCIÈRES PENDANT LA MIGRATION
# ============================================

library(dplyr)
library(haven)

# 1. Vérifier les modalités

cat("MRT_RTYP :\n")

print(
  attr(ego$MRT_RTYP, "labels")
)

# 2. Conserver uniquement les personnes
# ayant reçu une aide familiale pendant
# leur dernier séjour long

base_101 <- base_90 %>%
  filter(
    aide_recue == 1,
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    type_aide = code_numerique(
      MRT_RTYP
    )
  ) %>%
  filter(
    type_aide %in% 1:2
  )

# 3. Comparer les aides régulières
# et occasionnelles selon le motif

resultats_101 <- base_101 %>%
  group_by(motif) %>%
  summarise(
    
    n_reguliere = sum(type_aide == 2),
    n_occasionnelle = sum(type_aide == 1),
    
    pct_reguliere = 100 * weighted.mean(
      as.integer(type_aide == 2),
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    
    pourcentage_regulier = if_else(
      n_reguliere >= 10 &
        n_occasionnelle >= 10,
      
      round(pct_reguliere, 1),
      
      NA_real_
    )
  ) %>%
  select(
    motif,
    pourcentage_regulier
  )

print(
  resultats_101,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 102 - RÉGULARITÉ DES AIDES
# SELON LE MOTIF ET L'ÂGE AU DÉPART
# ============================================

library(dplyr)
library(tidyr)

# 1. Personnes ayant reçu une aide familiale
# Départs à 18-22 ans ou 23-29 ans
# Motif : études ou emploi

base_102 <- base_90 %>%
  filter(
    aide_recue == 1,
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    age_code_102 = code_numerique(
      AGEDEP_1_rec
    ),
    type_aide_102 = code_numerique(
      MRT_RTYP
    )
  ) %>%
  filter(
    age_code_102 %in% c(2, 3),
    as.character(motif) %in% c(
      "Études",
      "Emploi / recherche d'emploi"
    )
  ) %>%
  mutate(
    age_depart_102 = factor(
      age_code_102,
      levels = c(2, 3),
      labels = c(
        "18 à 22 ans",
        "23 à 29 ans"
      )
    ),
    motif = droplevels(motif)
  )

# 2. Vérifier la couverture
# de la question sur le type d'aide

couverture_102 <- base_102 %>%
  group_by(age_depart_102, motif) %>%
  summarise(
    n_valides = sum(
      type_aide_102 %in% 1:2
    ),
    n_autres = sum(
      !(type_aide_102 %in% 1:2)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    couverture_90pct =
      n_valides / (n_valides + n_autres) >= 0.90,
    
    couverture_pct = if_else(
      n_valides >= 10 & n_autres >= 10,
      round(
        100 * n_valides /
          (n_valides + n_autres),
        1
      ),
      NA_real_
    )
  ) %>%
  select(
    age_depart_102,
    motif,
    couverture_90pct,
    couverture_pct
  )

print(
  couverture_102,
  n = Inf,
  width = Inf
)

# 3. Calculer les aides régulières
# parmi les réponses valides

resultats_102 <- base_102 %>%
  filter(type_aide_102 %in% 1:2) %>%
  group_by(
    age_depart_102,
    motif
  ) %>%
  summarise(
    n_reguliere = sum(type_aide_102 == 2),
    n_occasionnelle = sum(type_aide_102 == 1),
    
    taux = 100 * weighted.mean(
      as.integer(type_aide_102 == 2),
      w = PFLIN
    ),
    .groups = "drop"
  ) %>%
  group_by(age_depart_102) %>%
  mutate(
    # Les deux motifs doivent être présents
    # avec suffisamment de réponses de chaque type
    
    comparaison_possible =
      n_distinct(motif) == 2 &
      all(
        n_reguliere >= 10 &
          n_occasionnelle >= 10
      ),
    
    pourcentage = if_else(
      comparaison_possible,
      round(taux, 1),
      NA_real_
    )
  ) %>%
  ungroup()

# 4. Tableau comparatif protégé

tableau_102 <- resultats_102 %>%
  select(
    age_depart_102,
    motif,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = motif,
    values_from = pourcentage
  )

print(
  tableau_102,
  n = Inf,
  width = Inf
)

# 5. Diagnostic des comparaisons possibles

diagnostic_102 <- resultats_102 %>%
  distinct(
    age_depart_102,
    comparaison_possible
  )

print(
  diagnostic_102,
  n = Inf
)


# ============================================
# ÉTAPE 103 - RÉGULARITÉ DES AIDES
# MIGRATIONS POUR ÉTUDES ET EMPLOI
# DÉPARTS ENTRE 18 ET 29 ANS
# ============================================

library(dplyr)

# 1. Utiliser la base de l'étape 102
# avec les deux classes d'âge réunies

base_103 <- base_102 %>%
  filter(
    type_aide_102 %in% c(1, 2)
  )

# 2. Calculer la proportion d'aides
# régulières selon le motif

resultats_103 <- base_103 %>%
  group_by(motif) %>%
  summarise(
    n_reguliere = sum(type_aide_102 == 2),
    n_occasionnelle = sum(type_aide_102 == 1),
    
    taux = 100 * weighted.mean(
      as.integer(type_aide_102 == 2),
      w = PFLIN
    ),
    
    .groups = "drop"
  )

# 3. Vérifier la confidentialité
# dans les DEUX groupes

comparaison_publiable_103 <-
  nrow(resultats_103) == 2 &&
  all(
    resultats_103$n_reguliere >= 10 &
      resultats_103$n_occasionnelle >= 10
  )

cat(
  "Comparaison publiable :",
  comparaison_publiable_103,
  "\n"
)

# 4. Masquer les deux résultats si
# un des groupes est insuffisant

tableau_103 <- resultats_103 %>%
  mutate(
    pourcentage_regulier = if (
      comparaison_publiable_103
    ) {
      round(taux, 1)
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  select(
    motif,
    pourcentage_regulier
  )

print(
  tableau_103,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 104 - TROIS SITUATIONS
# DE SOUTIEN FINANCIER FAMILIAL
# ============================================

library(dplyr)
library(tidyr)

# 1. Repartir de la population éligible
# et non des seuls bénéficiaires d'aide

base_104 <- base_90 %>%
  mutate(
    age_code_104 = code_numerique(
      AGEDEP_1_rec
    ),
    
    type_aide_104 = code_numerique(
      MRT_RTYP
    )
  ) %>%
  filter(
    age_code_104 %in% c(2, 3),
    
    as.character(motif) %in% c(
      "Études",
      "Emploi / recherche d'emploi"
    ),
    
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    motif = droplevels(motif),
    
    situation_aide = case_when(
      aide_recue == 0 ~
        "Aucune aide",
      
      aide_recue == 1 &
        type_aide_104 == 1 ~
        "Aide occasionnelle",
      
      aide_recue == 1 &
        type_aide_104 == 2 ~
        "Aide régulière",
      
      TRUE ~ NA_character_
    ),
    
    situation_aide = factor(
      situation_aide,
      levels = c(
        "Aucune aide",
        "Aide occasionnelle",
        "Aide régulière"
      )
    )
  )

# 2. Vérifier la couverture des trois situations
# sans révéler les petits effectifs

couverture_104 <- base_104 %>%
  group_by(motif) %>%
  summarise(
    couverture_90pct =
      mean(!is.na(situation_aide)) >= 0.90,
    
    .groups = "drop"
  )

print(
  couverture_104,
  n = Inf
)

# 3. Ne conserver que les réponses
# dont la situation d'aide est connue

base_104_valide <- base_104 %>%
  filter(!is.na(situation_aide))

# 4. Répartition pondérée des trois situations

resultats_104 <- base_104_valide %>%
  group_by(
    motif,
    situation_aide,
    .drop = FALSE
  ) %>%
  summarise(
    n_brut = n(),
    poids = sum(PFLIN),
    .groups = "drop"
  ) %>%
  group_by(motif) %>%
  mutate(
    pourcentage = 100 * poids / sum(poids)
  ) %>%
  ungroup()

# 5. Vérifier la confidentialité :
# chaque situation doit comporter
# au moins 10 observations dans chaque motif

comparaison_publiable_104 <-
  nrow(resultats_104) == 6 &&
  all(resultats_104$n_brut >= 10)

cat(
  "Tableau publiable :",
  comparaison_publiable_104,
  "\n"
)

# 6. Afficher les taux seulement
# si l'ensemble du tableau est publiable

tableau_104 <- resultats_104 %>%
  mutate(
    pct = if (comparaison_publiable_104) {
      round(pourcentage, 1)
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  select(
    motif,
    situation_aide,
    pct
  ) %>%
  pivot_wider(
    names_from = situation_aide,
    values_from = pct
  )

print(
  tableau_104,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 105 - COUVERTURE PONDÉRÉE
# DES TROIS SITUATIONS D'AIDE
# ============================================

library(dplyr)

controle_105 <- base_104 %>%
  group_by(motif) %>%
  summarise(
    n_valides = sum(!is.na(situation_aide)),
    n_manquants = sum(is.na(situation_aide)),
    
    couverture_ponderee = 100 * weighted.mean(
      !is.na(situation_aide),
      w = PFLIN
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    couverture_ponderee_90pct =
      couverture_ponderee >= 90,
    
    # Protection de la confidentialité :
    # ne pas publier de pourcentage permettant
    # d'isoler un petit groupe de non-réponses
    
    pct_publiable =
      n_valides >= 10 &
      (n_manquants == 0 | n_manquants >= 10),
    
    couverture_affichable = if_else(
      pct_publiable,
      round(couverture_ponderee, 1),
      NA_real_
    )
  ) %>%
  select(
    motif,
    couverture_ponderee_90pct,
    couverture_affichable
  )

print(
  controle_105,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 106 - IDENTIFIER LES VARIABLES
# DE QUALIFICATION ET D'INSERTION
# ============================================

library(dplyr)
library(haven)

# 1. Rechercher les variables candidates
# dans le fichier EGO

motifs_106 <- c(
  "^MR_AIDE",
  "^MRT_",
  "^MRB_",
  "^DIP14$",
  "^DATDIP",
  "^AGDIP",
  "^TRAVAIL$",
  "^STATUT$",
  "PCS",
  "^MR_ARET_1",
  "^MRA_ACTIBIS$",
  "^MRA_CHO$"
)

variables_106 <- unique(
  unlist(
    lapply(
      motifs_106,
      function(p) {
        grep(
          p,
          names(ego),
          value = TRUE,
          ignore.case = TRUE
        )
      }
    )
  )
)

# 2. Construire le dictionnaire

dictionnaire_106 <- data.frame(
  variable = variables_106,
  
  disponible_base_90 =
    variables_106 %in% names(base_90),
  
  intitule = vapply(
    variables_106,
    function(v) {
      lib <- attr(ego[[v]], "label")
      
      if (is.null(lib)) {
        return("Intitulé absent")
      }
      
      as.character(lib)[1]
    },
    character(1)
  ),
  
  row.names = NULL
)

print(
  dictionnaire_106,
  row.names = FALSE,
  right = FALSE
)

# 3. Examiner les modalités des variables
# de bilan du séjour migratoire

variables_bilan_106 <- grep(
  "^MRB_",
  names(ego),
  value = TRUE
)

for (v in variables_bilan_106) {
  
  cat("\nVariable :", v, "\n")
  
  print(
    attr(ego[[v]], "labels")
  )
}


# ============================================
# ÉTAPE 107 - VÉRIFIER LES VARIABLES
# D'INSERTION ET DE QUALIFICATION
# ============================================

library(dplyr)
library(haven)

# 1. Vérifier les modalités des
# principales variables à expliquer

variables_107 <- c(
  "MR_AIDE1_1",
  "MR_AIDE2_1",
  "DIP14",
  "DATDIP_rec",
  "AGDIP_rec",
  "TRAVAIL",
  "STATUT",
  "MR_ADEP_1_rec",
  "MR_ARET_1_rec"
)

for (v in variables_107) {
  
  if (v %in% names(ego)) {
    
    cat("\n============================\n")
    cat("Variable :", v, "\n")
    cat("============================\n")
    
    print(
      attr(ego[[v]], "label")
    )
    
    print(
      attr(ego[[v]], "labels")
    )
  }
}

# 2. Rechercher plus largement
# la profession actuelle de l'enquêté
# dans les noms et intitulés de variables

libelles_107 <- vapply(
  names(ego),
  function(v) {
    
    lib <- attr(ego[[v]], "label")
    
    if (is.null(lib)) {
      return("")
    }
    
    as.character(lib)[1]
  },
  character(1)
)

# Termes susceptibles de désigner
# une catégorie socioprofessionnelle

candidats_107 <- grepl(
  "^(PCS|CS[0-9]|CSP|PROF|CATPRO)",
  names(ego),
  ignore.case = TRUE
) |
  grepl(
    "profession|socioprofessionnelle|catégorie sociale|cadre",
    libelles_107,
    ignore.case = TRUE
  )

# 3. Afficher les variables candidates
# sans aucune donnée individuelle

dictionnaire_prof_107 <- data.frame(
  variable = names(ego)[candidats_107],
  
  intitule = libelles_107[candidats_107],
  
  disponible_base_90 =
    names(ego)[candidats_107] %in%
    names(base_90),
  
  row.names = NULL
)

print(
  dictionnaire_prof_107,
  row.names = FALSE,
  right = FALSE
)


# ============================================
# ÉTAPE 108 - AIDES FAMILIALES,
# QUALIFICATIONS ET EMPLOI
# MIGRATIONS POUR ÉTUDES
# ============================================

library(dplyr)
library(tidyr)

# 1. Conserver les migrants pour études
# partis entre 18 et 29 ans

base_108 <- base_104 %>%
  filter(
    as.character(motif) == "Études",
    age_code_104 %in% c(2, 3),
    !is.na(situation_aide),
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    
    # Diplôme le plus élevé
    diplome_code = code_numerique(DIP14),
    
    # Situation professionnelle actuelle
    travail_code = code_numerique(TRAVAIL),
    
    # Bénéfices déclarés du séjour
    positif_1 = code_numerique(MRB_POSI1),
    positif_2 = code_numerique(MRB_POSI2)
  )

# 2. Construire les trois indicateurs

base_108 <- base_108 %>%
  mutate(
    
    # A cité "Formation/qualification"
    # parmi les deux bénéfices possibles
    benefice_formation = case_when(
      positif_1 == 1 | positif_2 == 1 ~ 1L,
      
      positif_1 %in% 2:8 |
        positif_2 %in% 2:8 ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    # Diplôme supérieur au moment
    # de l'enquête : au moins bac+2
    diplome_superieur = case_when(
      diplome_code %in%
        c(10, 12, 20, 30, 31, 33) ~ 1L,
      
      diplome_code %in%
        c(41, 42, 43, 44, 50, 60, 70, 71) ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    # En emploi au moment de l'enquête
    en_emploi = case_when(
      travail_code == 1 ~ 1L,
      travail_code == 2 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# 3. Mettre les indicateurs
# dans un format commun

base_108_long <- base_108 %>%
  select(
    situation_aide,
    PFLIN,
    benefice_formation,
    diplome_superieur,
    en_emploi
  ) %>%
  pivot_longer(
    cols = c(
      benefice_formation,
      diplome_superieur,
      en_emploi
    ),
    names_to = "indicateur",
    values_to = "valeur"
  )

# 4. Estimer les proportions pondérées

resultats_108 <- base_108_long %>%
  group_by(
    indicateur,
    situation_aide,
    .drop = FALSE
  ) %>%
  summarise(
    n_oui = sum(valeur == 1, na.rm = TRUE),
    n_non = sum(valeur == 0, na.rm = TRUE),
    
    couverture_90pct = weighted.mean(
      !is.na(valeur),
      w = PFLIN
    ) >= 0.90,
    
    taux = if (any(!is.na(valeur))) {
      100 * weighted.mean(
        valeur,
        w = PFLIN,
        na.rm = TRUE
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

# 5. Protéger chaque tableau
# Les trois groupes doivent être suffisants

resultats_108 <- resultats_108 %>%
  group_by(indicateur) %>%
  mutate(
    
    tableau_publiable =
      n() == 3 &&
      all(n_oui >= 10 & n_non >= 10) &&
      all(couverture_90pct),
    
    pourcentage = if (all(tableau_publiable)) {
      round(taux, 1)
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  ungroup()

# 6. Tableau final

tableau_108 <- resultats_108 %>%
  select(
    situation_aide,
    indicateur,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = indicateur,
    values_from = pourcentage
  )

print(
  tableau_108,
  n = Inf,
  width = Inf
)

# 7. Diagnostic de faisabilité

diagnostic_108 <- resultats_108 %>%
  distinct(
    indicateur,
    tableau_publiable
  )

print(
  diagnostic_108,
  n = Inf
)


# ============================================
# ÉTAPE 109 - PRÉPARER L'ANALYSE
# DES POSITIONS SOCIOPROFESSIONNELLES
# ============================================

library(dplyr)
library(haven)

# 1. Examiner les modalités exactes
# de la catégorie socioprofessionnelle

cat("\nPCS DE L'ENQUÊTÉ :\n")

print(
  attr(ego$CS8, "label")
)

print(
  attr(ego$CS8, "labels")
)

# 2. Examiner la situation professionnelle
# recodée, pour compléter l'interprétation

cat("\nSITUATION PROFESSIONNELLE :\n")

print(
  attr(ego$SITUA_rec, "label")
)

print(
  attr(ego$SITUA_rec, "labels")
)

# 3. Conserver les personnes
# actuellement en emploi parmi les
# migrants pour études de l'étape 108

base_109 <- base_108 %>%
  filter(
    en_emploi == 1
  )

# 4. Vérifier la disponibilité de CS8
# dans les trois groupes d'aide

diagnostic_109 <- base_109 %>%
  group_by(situation_aide) %>%
  summarise(
    
    groupe_10_ou_plus =
      n() >= 10,
    
    pcs_renseignee_90pct =
      weighted.mean(
        !is.na(CS8),
        w = PFLIN
      ) >= 0.90,
    
    .groups = "drop"
  )

print(
  diagnostic_109,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 110 - AIDES FAMILIALES
# ET POSITIONS SOCIOPROFESSIONNELLES
# ============================================

library(dplyr)
library(tidyr)

# 1. Repartir des personnes en emploi

base_110 <- base_109 %>%
  mutate(
    cs_code_110 = code_numerique(CS8),
    
    position_110 = case_when(
      cs_code_110 == 3 ~
        "Cadres et professions supérieures",
      
      cs_code_110 == 4 ~
        "Professions intermédiaires",
      
      cs_code_110 %in% c(1, 2, 5, 6) ~
        "Autres professions",
      
      TRUE ~ NA_character_
    ),
    
    position_110 = factor(
      position_110,
      levels = c(
        "Cadres et professions supérieures",
        "Professions intermédiaires",
        "Autres professions"
      )
    )
  )

# 2. Vérifier la couverture des PCS valides

couverture_110 <- base_110 %>%
  group_by(situation_aide) %>%
  summarise(
    couverture_90pct = weighted.mean(
      !is.na(position_110),
      w = PFLIN
    ) >= 0.90,
    
    .groups = "drop"
  )

print(
  couverture_110,
  n = Inf
)

# 3. Répartition pondérée des positions
# parmi les PCS valides

resultats_110 <- base_110 %>%
  filter(!is.na(position_110)) %>%
  group_by(
    situation_aide,
    position_110,
    .drop = FALSE
  ) %>%
  summarise(
    n_brut = n(),
    poids = sum(PFLIN),
    .groups = "drop"
  ) %>%
  group_by(situation_aide) %>%
  mutate(
    taux = if (sum(poids) > 0) {
      100 * poids / sum(poids)
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  ungroup()

# 4. Contrôle de confidentialité :
# les 9 cellules doivent être suffisantes

tableau_publiable_110 <-
  nrow(resultats_110) == 9 &&
  all(resultats_110$n_brut >= 10) &&
  all(couverture_110$couverture_90pct)

cat(
  "Comparaison des trois PCS publiable :",
  tableau_publiable_110,
  "\n"
)

# 5. Tableau protégé

tableau_110 <- resultats_110 %>%
  mutate(
    pourcentage = if (tableau_publiable_110) {
      round(taux, 1)
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  select(
    situation_aide,
    position_110,
    pourcentage
  ) %>%
  pivot_wider(
    names_from = position_110,
    values_from = pourcentage
  )

print(
  tableau_110,
  n = Inf,
  width = Inf
)

# 6. Vérifier si une analyse binaire
# serait possible en cas de petits effectifs

diagnostic_binaire_110 <- base_110 %>%
  filter(!is.na(position_110)) %>%
  group_by(situation_aide) %>%
  summarise(
    n_cadres = sum(cs_code_110 == 3),
    n_intermediaires = sum(cs_code_110 == 4),
    n_autres = sum(
      cs_code_110 %in% c(1, 2, 5, 6)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    cadres_vs_autres_possible =
      n_cadres >= 10 &
      (n_intermediaires + n_autres) >= 10,
    
    cadres_inter_vs_autres_possible =
      (n_cadres + n_intermediaires) >= 10 &
      n_autres >= 10
  ) %>%
  select(
    situation_aide,
    cadres_vs_autres_possible,
    cadres_inter_vs_autres_possible
  )

print(
  diagnostic_binaire_110,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 111 - AIDES FAMILIALES ET ACCÈS
# AUX PROFESSIONS DE CADRES
# OU INTERMÉDIAIRES
# ============================================

library(dplyr)

# 1. Conserver les personnes en emploi
# dont la PCS est valide

base_111 <- base_110 %>%
  filter(
    cs_code_110 %in% 1:6,
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  mutate(
    position_superieure_inter =
      as.integer(
        cs_code_110 %in% c(3, 4)
      )
  )

# 2. Calculer les pourcentages pondérés
# selon le type d'aide familiale

resultats_111 <- base_111 %>%
  group_by(
    situation_aide,
    .drop = FALSE
  ) %>%
  summarise(
    
    n_cadres_inter = sum(
      position_superieure_inter == 1
    ),
    
    n_autres = sum(
      position_superieure_inter == 0
    ),
    
    taux = if (n() > 0) {
      100 * weighted.mean(
        position_superieure_inter,
        w = PFLIN
      )
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

# 3. Vérification de confidentialité
# dans les trois groupes

tableau_publiable_111 <-
  nrow(resultats_111) == 3 &&
  all(
    resultats_111$n_cadres_inter >= 10 &
      resultats_111$n_autres >= 10
  )

cat(
  "Tableau publiable :",
  tableau_publiable_111,
  "\n"
)

# 4. Construire le tableau protégé

tableau_111 <- resultats_111 %>%
  mutate(
    pourcentage = if (
      tableau_publiable_111
    ) {
      round(taux, 1)
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  select(
    situation_aide,
    pourcentage
  )

print(
  tableau_111,
  n = Inf,
  width = Inf
)


# ============================================
# ÉTAPE 112 - PREMIÈRE ACM
# AIDES, DIPLÔMES ET POSITIONS
# PROFESSIONNELLES
# ============================================

library(dplyr)
library(haven)

# Si FactoMineR n'est pas installé :
# install.packages("FactoMineR")

library(FactoMineR)

# 1. Préparer les variables qualitatives

base_acm_112 <- base_108 %>%
  mutate(
    
    # Aides publiques du dernier séjour
    aide_pub_1 = code_numerique(MR_AIDE1_1),
    aide_pub_2 = code_numerique(MR_AIDE2_1),
    
    aide_publique = case_when(
      aide_pub_1 %in% 2:8 |
        aide_pub_2 %in% 2:8 ~
        "Aide publique déclarée",
      
      aide_pub_1 == 1 &
        (is.na(aide_pub_2) |
           aide_pub_2 == 1) ~
        "Aucune aide publique",
      
      TRUE ~ NA_character_
    ),
    
    # Diplôme supérieur
    diplome_acm = case_when(
      diplome_superieur == 1 ~
        "Diplôme supérieur",
      
      diplome_superieur == 0 ~
        "Sans diplôme supérieur",
      
      TRUE ~ NA_character_
    ),
    
    # Situation professionnelle actuelle
    cs_acm = code_numerique(CS8),
    
    position_acm = case_when(
      en_emploi == 0 ~
        "Sans emploi actuel",
      
      en_emploi == 1 &
        cs_acm %in% c(3, 4) ~
        "Cadre ou intermédiaire",
      
      en_emploi == 1 &
        cs_acm %in% c(1, 2, 5, 6) ~
        "Autre profession",
      
      TRUE ~ NA_character_
    ),
    
    # Age au départ : variable supplémentaire
    age_acm = case_when(
      age_code_104 == 2 ~ "18-22 ans",
      age_code_104 == 3 ~ "23-29 ans",
      TRUE ~ NA_character_
    )
  )

# 2. Conserver les observations complètes
# pour cette première ACM

base_acm_112 <- base_acm_112 %>%
  filter(
    !is.na(situation_aide),
    !is.na(aide_publique),
    !is.na(diplome_acm),
    !is.na(position_acm),
    !is.na(age_acm),
    !is.na(PFLIN),
    PFLIN > 0
  ) %>%
  transmute(
    aide_familiale = factor(situation_aide),
    aide_publique = factor(aide_publique),
    diplome = factor(diplome_acm),
    position_professionnelle = factor(position_acm),
    age_depart = factor(age_acm),
    PFLIN
  )

# 3. Contrôler les modalités en interne
# avant d'afficher le graphique

variables_actives_112 <- c(
  "aide_familiale",
  "aide_publique",
  "diplome",
  "position_professionnelle"
)

variables_acm_112 <- c(
  variables_actives_112,
  "age_depart"
)

effectifs_modalites_112 <- lapply(
  base_acm_112[variables_acm_112],
  table
)

modalites_suffisantes_112 <- all(
  vapply(
    effectifs_modalites_112,
    function(x) all(x >= 10),
    logical(1)
  )
)

cat(
  "Toutes les modalités ont au moins",
  "10 observations :",
  modalites_suffisantes_112,
  "\n"
)

stopifnot(
  nrow(base_acm_112) > 0,
  modalites_suffisantes_112
)

# 4. Préparer les poids
# et les colonnes de l'ACM

poids_acm_112 <- (
  base_acm_112$PFLIN /
    mean(base_acm_112$PFLIN)
)

donnees_acm_112 <- base_acm_112 %>%
  select(all_of(variables_acm_112))

# 5. Réaliser l'ACM pondérée
# L'âge est supplémentaire (colonne 5)

res_acm_112 <- MCA(
  donnees_acm_112,
  quali.sup = 5,
  row.w = poids_acm_112,
  ncp = 3,
  graph = FALSE
)

# 6. Examiner les valeurs propres

cat("\nINERTIE DES AXES :\n")

print(
  round(
    res_acm_112$eig[1:3, , drop = FALSE],
    2
  )
)

# 7. Représenter les modalités
# sans afficher les individus

plot(
  res_acm_112,
  choix = "var",
  axes = c(1, 2)
)

# 8. Examiner les contributions
# des modalités actives aux deux axes

cat("\nCONTRIBUTIONS DES MODALITÉS :\n")

print(
  round(
    res_acm_112$var$contrib[, 1:2,
                            drop = FALSE],
    2
  )
)


# ============================================
# ÉTAPE 113 - ACM DES RESSOURCES
# ET CONDITIONS SOCIALES DE MOBILITÉ
# ============================================

library(dplyr)
library(tidyr)
library(haven)
library(FactoMineR)

# ============================================
# 1. RÉCUPÉRER L'ORIGINE SOCIALE
# ============================================

origine_114 <- plan_mfv_age$variables %>%
  transmute(
    identifiant_114 = as.character(IDENT_EGO),
    origine_sociale
  )

# Vérifier l'unicité de la clé

stopifnot(
  !anyNA(origine_114$identifiant_114),
  !anyDuplicated(origine_114$identifiant_114)
)

base_114 <- base_108 %>%
  select(-any_of("origine_sociale")) %>%
  mutate(
    identifiant_114 = as.character(IDENT_EGO)
  ) %>%
  left_join(
    origine_114,
    by = "identifiant_114",
    relationship = "many-to-one"
  )

stopifnot(
  nrow(base_114) == nrow(base_108)
)

# ============================================
# 2. CONSTRUIRE LES VARIABLES ACTIVES
# ET SUPPLÉMENTAIRES
# ============================================

base_114 <- base_114 %>%
  mutate(
    
    # AIDE FAMILIALE
    aide_familiale = factor(
      as.character(situation_aide),
      levels = c(
        "Aucune aide",
        "Aide occasionnelle",
        "Aide régulière"
      )
    ),
    
    # AIDE PUBLIQUE
    code_public_1 = code_numerique(
      MR_AIDE1_1
    ),
    
    code_public_2 = code_numerique(
      MR_AIDE2_1
    ),
    
    aide_publique = case_when(
      code_public_1 %in% 2:8 |
        code_public_2 %in% 2:8 ~
        "Aide publique",
      
      code_public_1 == 1 &
        (is.na(code_public_2) |
           code_public_2 == 1) ~
        "Aucune aide publique",
      
      TRUE ~ NA_character_
    ),
    
    aide_publique = factor(
      aide_publique,
      levels = c(
        "Aucune aide publique",
        "Aide publique"
      )
    ),
    
    # ORIGINE SOCIALE PARENTALE
    origine_parentale = factor(
      as.character(origine_sociale)
    ),
    
    # ÂGE AU DÉPART
    age_depart = factor(
      case_when(
        age_code_104 == 2 ~ "18-22 ans",
        age_code_104 == 3 ~ "23-29 ans",
        TRUE ~ NA_character_
      ),
      levels = c(
        "18-22 ans",
        "23-29 ans"
      )
    ),
    
    # DIPLÔME : VARIABLE SUPPLÉMENTAIRE
    diplome = factor(
      case_when(
        diplome_superieur == 1 ~
          "Diplôme supérieur",
        
        diplome_superieur == 0 ~
          "Sans diplôme supérieur",
        
        TRUE ~ NA_character_
      )
    ),
    
    # EMPLOI : VARIABLE SUPPLÉMENTAIRE
    emploi = factor(
      case_when(
        en_emploi == 1 ~ "En emploi",
        en_emploi == 0 ~ "Sans emploi",
        TRUE ~ NA_character_
      )
    ),
    
    # POSITION PROFESSIONNELLE :
    # VARIABLE SUPPLÉMENTAIRE
    
    code_cs8 = code_numerique(CS8),
    
    position = factor(
      case_when(
        en_emploi == 0 ~
          "Sans emploi actuel",
        
        en_emploi == 1 &
          code_cs8 %in% c(3, 4) ~
          "Cadre ou intermédiaire",
        
        en_emploi == 1 &
          code_cs8 %in% c(1, 2, 5, 6) ~
          "Autre profession",
        
        TRUE ~ NA_character_
      )
    )
  )

# ============================================
# 3. DÉFINIR LES VARIABLES DE L'ACM
# ============================================

actives_114 <- c(
  "aide_familiale",
  "aide_publique",
  "origine_parentale",
  "age_depart"
)

supplementaires_114 <- c(
  "diplome",
  "emploi",
  "position"
)

variables_114 <- c(
  actives_114,
  supplementaires_114
)

# ============================================
# 4. PRÉPARER L'ÉCHANTILLON COMMUN
# ============================================

# Diagnostic de couverture
# sur l'échantillon initial

observations_completes_114 <- complete.cases(
  base_114[, variables_114]
)

couverture_ponderee_114 <- weighted.mean(
  observations_completes_114,
  w = base_114$PFLIN
)

cat(
  "Couverture pondérée >= 90 % :",
  couverture_ponderee_114 >= 0.90,
  "\n"
)

# Conserver les observations complètes

base_acm_114 <- base_114[
  observations_completes_114,
]

donnees_acm_114 <- base_acm_114 %>%
  select(all_of(variables_114)) %>%
  mutate(
    across(
      everything(),
      droplevels
    )
  )

# ============================================
# 5. CONTRÔLER LES MODALITÉS
# ============================================

controle_modalites_114 <- vapply(
  donnees_acm_114,
  function(x) {
    nlevels(x) >= 2 &&
      all(table(x) >= 10)
  },
  logical(1)
)

diagnostic_114 <- data.frame(
  variable = names(controle_modalites_114),
  modalites_suffisantes =
    as.logical(controle_modalites_114)
)

print(
  diagnostic_114,
  row.names = FALSE
)

# Arrêter si certaines modalités
# ne satisfont pas le seuil minimal

stopifnot(
  nrow(donnees_acm_114) > 0,
  all(controle_modalites_114)
)

# ============================================
# 6. EFFECTUER L'ACM PONDÉRÉE
# ============================================

poids_114 <- (
  base_acm_114$PFLIN /
    mean(base_acm_114$PFLIN)
)

res_acm_114 <- MCA(
  donnees_acm_114,
  
  # Colonnes 5, 6 et 7 :
  # variables supplémentaires
  quali.sup = 5:7,
  
  row.w = poids_114,
  ncp = 5,
  graph = FALSE
)

# ============================================
# 7. VALEURS PROPRES
# ============================================

cat("\nINERTIE DES AXES :\n")

print(
  round(
    res_acm_114$eig[1:5, , drop = FALSE],
    2
  )
)

# ============================================
# 8. CARTE FACTORIELLE DES MODALITÉS
# ============================================

# Afficher les modalités actives
# et supplémentaires, pas les individus

plot(
  res_acm_114,
  choix = "ind",
  invisible = "ind",
  label = c("var", "quali.sup"),
  axes = c(1, 2),
  autoLab = "yes",
  graph.type = "classic",
  title = "ACM - Ressources et mobilités étudiantes"
)

# ============================================
# 9. CONTRIBUTIONS DES MODALITÉS ACTIVES
# ============================================

cat("\nCONTRIBUTIONS DES MODALITÉS :\n")

print(
  round(
    res_acm_114$var$contrib[
      , 1:2, drop = FALSE
    ],
    2
  )
)

# ============================================
# 10. COORDONNÉES DES MODALITÉS
# SUPPLÉMENTAIRES
# ============================================

cat("\nMODALITÉS SUPPLÉMENTAIRES :\n")

print(
  round(
    res_acm_114$quali.sup$coord[
      , 1:2, drop = FALSE
    ],
    2
  )
)


# ============================================
# ÉTAPE 114 BIS - REGROUPEMENT
# DE L'ORIGINE SOCIALE PARENTALE
# ============================================

library(dplyr)
library(FactoMineR)

# 1. Identifier les modalités insuffisantes
# Sans afficher les effectifs confidentiels

origine_5 <- table(
  factor(
    as.character(base_acm_114$origine_parentale),
    levels = levels(base_114$origine_parentale)
  )
)

diagnostic_origine_5 <- data.frame(
  origine_sociale = names(origine_5),
  effectif_suffisant = as.integer(origine_5) >= 10
)

print(
  diagnostic_origine_5,
  row.names = FALSE
)

# 2. Tester un regroupement en trois catégories

base_acm_114 <- base_acm_114 %>%
  mutate(
    origine_3 = case_when(
      
      as.character(origine_parentale) %in% c(
        "Dominante cadre",
        "Dominante intermédiaire"
      ) ~ "Dominante cadre ou intermédiaire",
      
      as.character(origine_parentale) %in% c(
        "Dominante employée/ouvrière",
        "Monoactif employé/ouvrier"
      ) ~ "Ménage employé/ouvrier",
      
      as.character(origine_parentale) ==
        "Ménage inactif" ~ "Ménage inactif",
      
      TRUE ~ NA_character_
    ),
    
    origine_3 = factor(
      origine_3,
      levels = c(
        "Dominante cadre ou intermédiaire",
        "Ménage employé/ouvrier",
        "Ménage inactif"
      )
    )
  )

# 3. Vérifier les trois nouveaux effectifs

effectifs_3 <- table(base_acm_114$origine_3)

diagnostic_origine_3 <- data.frame(
  categorie = names(effectifs_3),
  effectif_suffisant = as.integer(effectifs_3) >= 10
)

print(
  diagnostic_origine_3,
  row.names = FALSE
)

# 4. Ne poursuivre que si les trois
# catégories respectent le seuil de 10

if (all(effectifs_3 >= 10) &&
    !anyNA(base_acm_114$origine_3)) {
  
  # Remplacer la variable active initiale
  
  donnees_acm_114$origine_parentale <-
    base_acm_114$origine_3
  
  # Recalculer l'ACM pondérée
  
  res_acm_114 <- MCA(
    donnees_acm_114,
    quali.sup = 5:7,
    row.w = poids_114,
    ncp = 5,
    graph = FALSE
  )
  
  cat("\nACM réalisée avec succès.\n")
  
  # Inertie des axes
  
  print(
    round(
      res_acm_114$eig[1:5, , drop = FALSE],
      2
    )
  )
  
  # Contributions des variables actives
  
  print(
    round(
      res_acm_114$var$contrib[, 1:2, drop = FALSE],
      2
    )
  )
  
  # Carte des modalités
  # Sans représenter les individus
  
  plot(
    res_acm_114,
    choix = "ind",
    invisible = "ind",
    label = c("var", "quali.sup"),
    axes = c(1, 2),
    autoLab = "yes",
    title = "ACM - Ressources et mobilités étudiantes"
  )
  
} else {
  
  cat(
    "\nLe regroupement en trois catégories",
    "reste insuffisant.",
    "\nAucune ACM n'a été recalculée.\n"
  )
}


# ============================================
# ÉTAPE 115 - ACM DES RESSOURCES
# AVEC QUATRE ORIGINES SOCIALES
# ============================================

library(dplyr)
library(FactoMineR)

# 1. Conserver les quatre catégories
# sociales suffisamment représentées

base_acm_115 <- base_acm_114 %>%
  filter(
    as.character(origine_parentale) !=
      "Ménage inactif"
  ) %>%
  mutate(
    origine_parentale =
      droplevels(origine_parentale)
  )

# 2. Préparer les variables
# Les quatre premières sont actives
# Les trois dernières supplémentaires

donnees_acm_115 <- base_acm_115 %>%
  select(
    aide_familiale,
    aide_publique,
    origine_parentale,
    age_depart,
    diplome,
    emploi,
    position
  ) %>%
  mutate(
    across(
      everything(),
      droplevels
    )
  )

# 3. Revérifier toutes les modalités
# après l'exclusion des ménages inactifs

controle_115 <- vapply(
  donnees_acm_115,
  function(x) {
    nlevels(x) >= 2 &&
      all(table(x) >= 10)
  },
  logical(1)
)

print(
  data.frame(
    variable = names(controle_115),
    effectifs_suffisants =
      as.logical(controle_115)
  ),
  row.names = FALSE
)

stopifnot(
  nrow(donnees_acm_115) > 0,
  all(controle_115)
)

# 4. Préparer les pondérations
# sur le nouvel échantillon

poids_115 <- base_acm_115$PFLIN /
  mean(base_acm_115$PFLIN)

# 5. Effectuer l'ACM

res_acm_115 <- MCA(
  donnees_acm_115,
  quali.sup = 5:7,
  row.w = poids_115,
  ncp = 5,
  graph = FALSE
)

# 6. Examiner l'inertie

cat("\nVALEURS PROPRES :\n")

print(
  round(
    res_acm_115$eig[1:5, , drop = FALSE],
    2
  )
)

# 7. Carte des modalités
# Actives et supplémentaires
# Sans afficher les individus

plot(
  res_acm_115,
  choix = "ind",
  invisible = "ind",
  label = c("var", "quali.sup"),
  axes = c(1, 2),
  autoLab = "yes",
  graph.type = "classic",
  title = "ACM - Ressources et mobilités étudiantes"
)

# 8. Contributions des modalités actives

cat("\nCONTRIBUTIONS AUX AXES 1 ET 2 :\n")

print(
  round(
    res_acm_115$var$contrib[, 1:2,
                            drop = FALSE],
    2
  )
)

# 9. Coordonnées des modalités
# supplémentaires

cat("\nMODALITÉS SUPPLÉMENTAIRES :\n")

print(
  round(
    res_acm_115$quali.sup$coord[
      , 1:2, drop = FALSE
    ],
    2
  )
)

# 10. Qualité de représentation
# des modalités actives

cat("\nCOS² DES MODALITÉS ACTIVES :\n")

print(
  round(
    res_acm_115$var$cos2[
      , 1:2, drop = FALSE
    ],
    2
  )
)


# ============================================
# ÉTAPE 116 - APPROFONDIR L'INTERPRÉTATION
# DE L'ACM DES RESSOURCES
# ============================================

library(FactoMineR)

# 1. Coordonnées et qualité de représentation
# des modalités actives

tableau_actives_116 <- data.frame(
  modalite = rownames(res_acm_115$var$coord),
  
  axe_1 = round(
    res_acm_115$var$coord[, 1], 2
  ),
  
  axe_2 = round(
    res_acm_115$var$coord[, 2], 2
  ),
  
  axe_3 = round(
    res_acm_115$var$coord[, 3], 2
  ),
  
  contribution_axe_3 = round(
    res_acm_115$var$contrib[, 3], 2
  ),
  
  cos2_plan_12 = round(
    rowSums(
      res_acm_115$var$cos2[, 1:2,
                           drop = FALSE]
    ),
    2
  )
)

cat("\nMODALITÉS ACTIVES :\n")

print(
  tableau_actives_116,
  row.names = FALSE
)

# 2. Coordonnées des modalités supplémentaires
# sur les trois premiers axes

cat("\nMODALITÉS SUPPLÉMENTAIRES :\n")

print(
  round(
    res_acm_115$quali.sup$coord[
      , 1:3, drop = FALSE
    ],
    2
  )
)

# 3. Rapport de corrélation des variables
# supplémentaires avec les axes,
# si disponible dans l'objet FactoMineR

eta2_116 <- res_acm_115$quali.sup$eta2

if (!is.null(eta2_116)) {
  
  cat("\nRAPPORTS DE CORRÉLATION ETA² :\n")
  
  print(
    round(
      eta2_116[, 1:3, drop = FALSE],
      3
    )
  )
  
}

# 4. Carte des dimensions 1 et 3

plot(
  res_acm_115,
  choix = "ind",
  invisible = "ind",
  label = c("var", "quali.sup"),
  axes = c(1, 3),
  autoLab = "yes",
  title = "ACM - Dimensions 1 et 3"
)

# 5. Carte des dimensions 2 et 3

plot(
  res_acm_115,
  choix = "ind",
  invisible = "ind",
  label = c("var", "quali.sup"),
  axes = c(2, 3),
  autoLab = "yes",
  title = "ACM - Dimensions 2 et 3"
)


# ============================================
# ÉTAPE 117 - CLASSIFICATION HCPC
# À PARTIR DE L'ACM
# ============================================

library(FactoMineR)

# 1. Classification sur les composantes
# de l'ACM des ressources

res_hcpc_117 <- HCPC(
  res_acm_115,
  nb.clust = -1,
  consol = FALSE,
  graph = FALSE
)

# 2. Récupérer les classes

classes_117 <- res_hcpc_117$data.clust$clust

stopifnot(
  length(classes_117) ==
    nrow(base_acm_115)
)

# 3. Vérification de confidentialité
# sans afficher les effectifs

effectifs_classes_117 <- table(classes_117)

diagnostic_117 <- data.frame(
  classe = names(effectifs_classes_117),
  effectif_suffisant =
    as.integer(effectifs_classes_117) >= 10
)

cat(
  "\nNombre de classes :",
  length(effectifs_classes_117),
  "\n"
)

print(
  diagnostic_117,
  row.names = FALSE
)

# 4. Vérifier si toutes les classes
# peuvent être étudiées

cat(
  "\nToutes les classes comportent",
  "au moins 10 observations :",
  all(effectifs_classes_117 >= 10),
  "\n"
)


# ============================================
# ÉTAPE 118 - CARACTÉRISER LES 4 CLASSES
# DE L'ACM / HCPC
# ============================================

library(dplyr)

# 1. Récupérer les classes de la HCPC

donnees_hcpc_118 <- res_hcpc_117$data.clust

# Vérifier l'appariement par les lignes
# avant d'associer classes et pondérations

indices_118 <- match(
  rownames(donnees_acm_115),
  rownames(donnees_hcpc_118)
)

stopifnot(
  !anyNA(indices_118),
  !anyDuplicated(indices_118)
)

# Vérifier également les variables actives

variables_actives_118 <- c(
  "aide_familiale",
  "aide_publique",
  "origine_parentale",
  "age_depart"
)

stopifnot(
  all(vapply(
    variables_actives_118,
    function(v) {
      identical(
        as.character(donnees_acm_115[[v]]),
        as.character(
          donnees_hcpc_118[[v]][indices_118]
        )
      )
    },
    logical(1)
  ))
)

classes_118 <- factor(
  donnees_hcpc_118$clust[indices_118]
)

# 2. Part pondérée de chaque classe

poids_classes_118 <- data.frame(
  classe = classes_118,
  PFLIN = base_acm_115$PFLIN
) %>%
  group_by(classe) %>%
  summarise(
    n_interne = n(),
    poids_total = sum(PFLIN),
    .groups = "drop"
  ) %>%
  mutate(
    part_ponderee = if (
      all(n_interne >= 10)
    ) {
      round(
        100 * poids_total / sum(poids_total),
        1
      )
    } else {
      rep(NA_real_, n())
    }
  ) %>%
  select(classe, part_ponderee)

cat("\nPOIDS DES QUATRE CLASSES :\n")

print(
  poids_classes_118,
  row.names = FALSE
)

# 3. Variables décrivant les classes
# Actives et supplémentaires

variables_profil_118 <- c(
  variables_actives_118,
  "diplome",
  "emploi",
  "position"
)

# 4. Identifier la modalité dominante
# de chaque variable dans chaque classe
# sans afficher les petits effectifs

profils_118 <- bind_rows(
  lapply(
    variables_profil_118,
    function(v) {
      
      bind_rows(
        lapply(
          levels(classes_118),
          function(cl) {
            
            i <- which(classes_118 == cl)
            
            modalites <- as.character(
              donnees_acm_115[[v]][i]
            )
            
            freq <- sort(
              table(modalites),
              decreasing = TRUE
            )
            
            dominante <- names(freq)[1]
            n_dominant <- as.integer(freq[1])
            
            # Protéger aussi le complément
            publiable <-
              n_dominant >= 10 &&
              (length(i) - n_dominant) >= 10
            
            pct <- if (publiable) {
              round(
                100 * weighted.mean(
                  modalites == dominante,
                  w = base_acm_115$PFLIN[i]
                ),
                1
              )
            } else {
              NA_real_
            }
            
            data.frame(
              classe = cl,
              variable = v,
              modalite_dominante =
                if (publiable) {
                  dominante
                } else {
                  "Masquée"
                },
              pourcentage = pct
            )
          }
        )
      )
    }
  )
)

# 5. Afficher les profils protégés

cat("\nPROFILS DES CLASSES :\n")

print(
  profils_118,
  row.names = FALSE
)


# ============================================
# ÉTAPE 119 - MODALITÉS SURREPRÉSENTÉES
# DANS LES QUATRE CLASSES HCPC
# ============================================

library(dplyr)

# Vérifier que les appartenances aux classes
# correspondent aux données de l'ACM

stopifnot(
  length(classes_118) == nrow(donnees_acm_115),
  length(poids_115) == nrow(donnees_acm_115)
)

variables_119 <- c(
  "aide_familiale",
  "aide_publique",
  "origine_parentale",
  "age_depart"
)

# Fonction de comparaison protégée

comparer_119 <- function(v, cl, modalite) {
  
  x <- as.character(donnees_acm_115[[v]])
  dans_classe <- as.character(classes_118) == cl
  a_modalite <- x == modalite
  
  # Quatre cellules :
  # modalité oui/non × dans/hors classe
  
  effectifs <- c(
    sum(dans_classe & a_modalite),
    sum(dans_classe & !a_modalite),
    sum(!dans_classe & a_modalite),
    sum(!dans_classe & !a_modalite)
  )
  
  if (any(effectifs < 10)) {
    return(NULL)
  }
  
  pct_classe <- 100 * weighted.mean(
    a_modalite[dans_classe],
    poids_115[dans_classe]
  )
  
  pct_autres <- 100 * weighted.mean(
    a_modalite[!dans_classe],
    poids_115[!dans_classe]
  )
  
  data.frame(
    classe = cl,
    variable = v,
    modalite = modalite,
    pct_classe = round(pct_classe, 1),
    pct_autres = round(pct_autres, 1),
    ecart_points = round(
      pct_classe - pct_autres, 1
    )
  )
}

# Calculer les comparaisons en interne

comparaisons_119 <- bind_rows(
  lapply(
    variables_119,
    function(v) {
      
      bind_rows(
        lapply(
          levels(classes_118),
          function(cl) {
            
            bind_rows(
              lapply(
                levels(donnees_acm_115[[v]]),
                function(m) {
                  comparer_119(v, cl, m)
                }
              )
            )
          }
        )
      )
    }
  )
)

# Retenir uniquement la modalité
# présentant le plus fort écart positif
# dans chaque classe et pour chaque variable

profils_distinctifs_119 <- comparaisons_119 %>%
  filter(ecart_points > 0) %>%
  group_by(classe, variable) %>%
  slice_max(
    order_by = ecart_points,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  arrange(classe, variable)

print(
  profils_distinctifs_119,
  row.names = FALSE
)


# ============================================
# ÉTAPE 120 - DIFFÉRENCES SCOLAIRES
# ET PROFESSIONNELLES ENTRE CLASSES
# ============================================

library(dplyr)

# 1. Vérifier l'alignement

stopifnot(
  length(classes_118) == nrow(donnees_acm_115),
  length(poids_115) == nrow(donnees_acm_115)
)

# 2. Construire les indicateurs
# supplémentaires sur le même échantillon

issues_120 <- data.frame(
  classe = classes_118,
  poids = poids_115,
  
  diplome = as.integer(
    donnees_acm_115$diplome ==
      "Diplôme supérieur"
  ),
  
  emploi = as.integer(
    donnees_acm_115$emploi ==
      "En emploi"
  ),
  
  position = as.integer(
    donnees_acm_115$position ==
      "Cadre ou intermédiaire"
  )
)

# 3. Comparer chaque classe à l'ensemble
# uniquement si les 4 groupes sont publiables

resultats_120 <- bind_rows(
  lapply(
    c("diplome", "emploi", "position"),
    function(v) {
      
      tab <- issues_120 %>%
        group_by(classe) %>%
        summarise(
          n_oui = sum(.data[[v]] == 1),
          n_non = sum(.data[[v]] == 0),
          
          taux = 100 * weighted.mean(
            .data[[v]],
            w = poids
          ),
          
          .groups = "drop"
        )
      
      publiable <- nrow(tab) == 4 &&
        all(tab$n_oui >= 10 &
              tab$n_non >= 10)
      
      if (!publiable) {
        return(
          data.frame(
            indicateur = v,
            classe = as.character(tab$classe),
            pourcentage = NA_real_,
            ecart_ensemble = NA_real_
          )
        )
      }
      
      taux_ensemble <- 100 * weighted.mean(
        issues_120[[v]],
        w = issues_120$poids
      )
      
      data.frame(
        indicateur = v,
        classe = as.character(tab$classe),
        
        pourcentage = round(
          tab$taux, 1
        ),
        
        ecart_ensemble = round(
          tab$taux - taux_ensemble, 1
        )
      )
    }
  )
)

print(
  resultats_120,
  row.names = FALSE
)
