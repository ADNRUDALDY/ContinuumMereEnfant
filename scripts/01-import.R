# =============================================================
# 01-import.R
# Import du jeu brut et contrôle qualité — AUCUNE modification
# Objectif : inventorier les anomalies, pas les corriger
# =============================================================

library(tidyverse)


# 1. Import ---------------------------------------------------

# read_csv() (readr) plutôt que read.csv() : plus rapide, meilleure
# détection des types, et il affiche ce qu'il a deviné.
brut <- read_csv("data/raw/continuum_brut.csv")

# Le message de spécification des colonnes qui s'affiche EST une
# information : lis-le. Il te dit comment R a interprété chaque colonne.


# 2. Première inspection --------------------------------------

dim(brut)        # dimensions : lignes x colonnes
names(brut)      # noms des colonnes
glimpse(brut)    # version tidyverse de str() — plus lisible
summary(brut)    # résumé statistique par colonne

# Regarde attentivement les Min. et Max. de summary() :
# c'est là que les valeurs impossibles se trahissent.


# 3. Doublons -------------------------------------------------

# Doublons sur la ligne entière (hors identifiant)
brut |>
  select(-id) |>
  duplicated() |>
  sum()

# Identifier précisément les lignes concernées
brut |>
  group_by(across(-id)) |>
  filter(n() > 1) |>
  ungroup() |>
  arrange(district, age_mere)

# Identifiants dupliqués ?
sum(duplicated(brut$id))


# 4. Valeurs manquantes ---------------------------------------

# Nombre de NA par colonne
colSums(is.na(brut))

# Version tidyverse, triée
brut |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") |>
  mutate(pct = round(100 * n_na / nrow(brut), 1)) |>
  filter(n_na > 0) |>
  arrange(desc(n_na))

# Lignes ayant au moins un NA
sum(!complete.cases(brut))

# QUESTION CLÉ : les NA sont-ils répartis au hasard ?
# On compare le nombre moyen de CPN selon que l'IMC est manquant ou non.
brut |>
  group_by(imc_manquant = is.na(imc_pre)) |>
  summarise(n = n(), cpn_moyen = mean(cpn_total, na.rm = TRUE))


# 5. Modalités des variables qualitatives ---------------------

# table() compte les effectifs ; useNA = "ifany" affiche aussi les NA
table(brut$district,    useNA = "ifany")
table(brut$milieu,      useNA = "ifany")
table(brut$instruction, useNA = "ifany")
table(brut$sexe_enfant, useNA = "ifany")
table(brut$cpn_precoce, useNA = "ifany")

# Combien de modalités distinctes par variable texte ?
brut |>
  summarise(across(where(is.character), n_distinct))


# 6. Plausibilité des variables quantitatives -----------------

# Bornes attendues, définies A PRIORI d'après la littérature clinique
brut |>
  summarise(
    age_min    = min(age_mere,        na.rm = TRUE),
    age_max    = max(age_mere,        na.rm = TRUE),
    imc_min    = min(imc_pre,         na.rm = TRUE),
    imc_max    = max(imc_pre,         na.rm = TRUE),
    cpn_max    = max(cpn_total,       na.rm = TRUE),
    poids_min  = min(poids_naissance, na.rm = TRUE),
    poids_max  = max(poids_naissance, na.rm = TRUE),
    terme_min  = min(terme_sa,        na.rm = TRUE),
    doses_max  = max(doses_12m,       na.rm = TRUE)
  )

# Repérage ciblé des valeurs hors bornes plausibles
brut |>
  filter(age_mere < 12 | age_mere > 50) |>
  select(id, age_mere, parite, instruction)

brut |>
  filter(imc_pre < 12 | imc_pre > 50) |>
  select(id, imc_pre, age_mere)

brut |>
  filter(poids_naissance < 500 | poids_naissance > 6000) |>
  select(id, poids_naissance, terme_sa, sexe_enfant)

brut |>
  filter(cpn_total > 15) |>
  select(id, cpn_total, terme_sa, milieu)

# Incohérence logique : la parité ne peut excéder l'âge fertile
brut |>
  filter(parite > (age_mere - 12) / 1.5) |>
  select(id, age_mere, parite)


# 7. Inspection visuelle --------------------------------------

# Un boxplot révèle instantanément les valeurs extrêmes
par(mfrow = c(2, 2))   # 4 graphiques en grille (graphique de base R)
boxplot(brut$age_mere,        main = "Âge maternel")
boxplot(brut$imc_pre,         main = "IMC pré-gestationnel")
boxplot(brut$poids_naissance, main = "Poids de naissance")
boxplot(brut$cpn_total,       main = "Nombre de CPN")
par(mfrow = c(1, 1))   # on rétablit l'affichage simple


# 8. Synthèse -------------------------------------------------

cat("\n===== SYNTHÈSE DU CONTRÔLE QUALITÉ =====\n")
cat("Lignes            :", nrow(brut), "\n")
cat("Colonnes          :", ncol(brut), "\n")
cat("Lignes complètes  :", sum(complete.cases(brut)), "\n")
cat("Lignes avec NA    :", sum(!complete.cases(brut)), "\n")
cat("Doublons (hors id):", sum(duplicated(select(brut, -id))), "\n")