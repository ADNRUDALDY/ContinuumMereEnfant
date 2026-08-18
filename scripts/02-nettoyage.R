# =============================================================
# 02-nettoyage.R
# Nettoyage documenté du jeu brut
# Entrée  : data/raw/continuum_brut.csv
# Sortie  : data/processed/continuum_propre.rds
# Principe : chaque modification est justifiée en commentaire
# =============================================================

library(tidyverse)

brut <- read_csv("data/raw/continuum_brut.csv", show_col_types = FALSE)
cat("Lignes en entrée :", nrow(brut), "\n")


# 1. Suppression du doublon exact -----------------------------
# id 401 est une copie intégrale de id 89. distinct() sur toutes
# les colonnes sauf id : on garde la première occurrence.

propre <- brut |>
  distinct(across(-id), .keep_all = TRUE)

cat("Après dédoublonnage :", nrow(propre), "\n")


# 2. Harmonisation des modalités ------------------------------
# str_to_title() : "RURAL" et "rural" -> "Rural"
# str_to_upper() : "m" -> "M"
# Correction certaine, aucune information créée.

propre <- propre |>
  mutate(
    milieu      = str_to_title(str_trim(milieu)),
    sexe_enfant = str_to_upper(str_trim(sexe_enfant)),
    district    = str_to_title(str_trim(district))
  )

# Vérification immédiate — réflexe à conserver après chaque correction
table(propre$milieu)
table(propre$sexe_enfant)


# 3. Neutralisation des valeurs impossibles -------------------
# Règles définies A PRIORI sur des bornes cliniques, pas ligne
# par ligne : le script reste valable si le jeu change.

propre <- propre |>
  mutate(
    age_mere        = if_else(age_mere < 12 | age_mere > 55,
                              NA_real_, age_mere),
    imc_pre         = if_else(imc_pre < 12 | imc_pre > 60,
                              NA_real_, imc_pre),
    poids_naissance = if_else(poids_naissance < 500 | poids_naissance > 6500,
                              NA_real_, poids_naissance),
    # 99 = code sentinelle "inconnu". ATTENTION : 0 reste une
    # valeur légitime et informative, on ne la touche pas.
    cpn_total       = if_else(cpn_total > 20, NA_real_, cpn_total),
    terme_sa        = if_else(terme_sa < 22 | terme_sa > 45,
                              NA_real_, terme_sa)
  )


# 4. Typage : facteurs et entiers -----------------------------
# Le NIVEAU DE RÉFÉRENCE est le premier de la liste. Tous les
# coefficients de régression s'interpréteront PAR RAPPORT à lui.
# Choix : la modalité la plus défavorisée, pour que les
# coefficients se lisent comme des gains.

propre <- propre |>
  mutate(
    milieu      = factor(milieu, levels = c("Rural", "Urbain")),
    sexe_enfant = factor(sexe_enfant, levels = c("F", "M")),
    district    = factor(district),
    district    = fct_relevel(district, "Centre"),   # district de référence
    
    # Facteur NON ordonné malgré la hiérarchie naturelle — voir note ci-dessous
    instruction = factor(instruction,
                         levels = c("Aucune", "Primaire",
                                    "Secondaire", "Superieur")),
    
    # Comptages en entiers plutôt qu'en doubles
    parite    = as.integer(parite),
    cpn_total = as.integer(cpn_total),
    doses_12m = as.integer(doses_12m)
  )


# 5. Variables dérivées ---------------------------------------

propre <- propre |>
  mutate(
    # Catégories d'IMC selon les seuils OMS
    imc_cat = case_when(
      is.na(imc_pre)  ~ NA_character_,
      imc_pre < 18.5  ~ "Insuffisance pondérale",
      imc_pre < 25    ~ "Normal",
      imc_pre < 30    ~ "Surpoids",
      TRUE            ~ "Obésité"
    ),
    imc_cat = factor(imc_cat, levels = c("Normal", "Insuffisance pondérale",
                                         "Surpoids", "Obésité")),
    
    # Prématurité : seuil OMS de 37 SA
    premature = if_else(terme_sa < 37, TRUE, FALSE),
    
    # Faible poids de naissance : seuil OMS de 2500 g
    fpn = if_else(poids_naissance < 2500, TRUE, FALSE),
    
    # Suivi prénatal adéquat : recommandation OMS de 8 contacts.
    # Ici le seuil est fixé à 4, plus réaliste en contexte de
    # ressources limitées — à documenter dans le rapport.
    cpn_adequat = if_else(cpn_total >= 4, TRUE, FALSE),
    
    # Calendrier vaccinal complet à 12 mois
    complet_12m = if_else(doses_12m >= 11, TRUE, FALSE),
    
    # Log du retard : la distribution est très asymétrique.
    # log1p(x) = log(x + 1), gère proprement les valeurs nulles.
    log_retard = log1p(retard_cumule)
  )


# 6. Contrôles de sortie --------------------------------------
# Un nettoyage non vérifié n'est pas un nettoyage.

stopifnot(
  nrow(propre) == 400,
  nlevels(propre$milieu) == 2,
  nlevels(propre$sexe_enfant) == 2,
  all(propre$age_mere >= 12, na.rm = TRUE),
  all(propre$cpn_total <= 20, na.rm = TRUE)
)

glimpse(propre)

# Manquants après nettoyage
propre |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") |>
  filter(n_na > 0) |>
  arrange(desc(n_na))


# 7. Export ---------------------------------------------------
# .rds conserve les types R (facteurs, niveaux, ordre).
# Le .csv est fourni en plus, pour lecture hors de R.

saveRDS(propre, "data/processed/continuum_propre.rds")
write_csv(propre, "data/processed/continuum_propre.csv")

cat("\nNettoyage terminé :", nrow(propre), "lignes,", ncol(propre), "colonnes\n")