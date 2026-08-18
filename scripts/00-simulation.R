# =============================================================
# 00-simulation.R
# Génération du jeu de données "Continuum de soins mère-enfant"
# Données SIMULÉES à visée pédagogique — aucune valeur épidémiologique
# =============================================================

library(tidyverse)

# La graine fixe le générateur aléatoire : tout le monde obtient
# exactement le même jeu de données. C'est la base de la reproductibilité.
set.seed(2026)

n <- 400  # nombre de couples mère-enfant


# 1. Caractéristiques socio-démographiques --------------------

district <- sample(
  c("Nord", "Sud", "Est", "Ouest", "Centre"),
  size = n, replace = TRUE,
  prob = c(0.18, 0.22, 0.20, 0.15, 0.25)   # effectifs déséquilibrés, comme en vrai
)

milieu <- ifelse(district == "Centre",
                 sample(c("Urbain", "Rural"), n, TRUE, prob = c(0.85, 0.15)),
                 sample(c("Urbain", "Rural"), n, TRUE, prob = c(0.35, 0.65)))

# rnorm() tire dans une loi normale ; round() arrondit ; pmax/pmin bornent
age_mere <- round(rnorm(n, mean = 27, sd = 6))
age_mere <- pmax(15, pmin(45, age_mere))

# L'instruction dépend du milieu : c'est une source de confusion volontaire
instruction <- ifelse(
  milieu == "Urbain",
  sample(c("Aucune", "Primaire", "Secondaire", "Superieur"), n, TRUE,
         prob = c(0.08, 0.22, 0.45, 0.25)),
  sample(c("Aucune", "Primaire", "Secondaire", "Superieur"), n, TRUE,
         prob = c(0.30, 0.38, 0.27, 0.05))
)

# La parité croît avec l'âge — relation réaliste
parite <- rpois(n, lambda = pmax(0, (age_mere - 18) / 5))
parite <- pmin(parite, 8)

imc_pre <- round(rnorm(n, mean = 23.5, sd = 3.8), 1)
imc_pre <- pmax(15, pmin(40, imc_pre))

# Distance au centre de santé : loi asymétrique (beaucoup de proches,
# une minorité très éloignée)
distance_cs <- round(rgamma(n, shape = 2, rate = 0.35) *
                       ifelse(milieu == "Rural", 1.8, 0.6), 1)
distance_cs <- pmin(distance_cs, 45)


# 2. Suivi prénatal -------------------------------------------

# Le nombre de CPN dépend de la distance, de l'instruction et du milieu.
# C'est ICI qu'on encode la "vérité terrain" que tes modèles devront retrouver.
score_acces <- 4.2 -
  0.06 * distance_cs +
  0.55 * (instruction == "Secondaire") +
  1.10 * (instruction == "Superieur") -
  0.45 * (instruction == "Aucune") +
  0.40 * (milieu == "Urbain") -
  0.12 * parite

cpn_total <- round(score_acces + rnorm(n, 0, 1.2))
cpn_total <- pmax(0, pmin(12, cpn_total))

# Précocité du suivi : liée à l'intensité, mais pas identique
cpn_precoce <- rbinom(n, 1, prob = plogis(-1.4 + 0.42 * cpn_total)) == 1


# 3. Naissance ------------------------------------------------

terme_sa <- round(rnorm(n, mean = 39, sd = 1.8), 1)
terme_sa <- pmax(28, pmin(42, terme_sa))

sexe_enfant <- sample(c("M", "F"), n, TRUE, prob = c(0.51, 0.49))

# VÉRITÉ TERRAIN Q1 — poids de naissance en grammes.
# Note les coefficients : ce sont eux que ta régression devra estimer.
poids_naissance <- 700 +
  62   * terme_sa +          # effet dominant : le terme
  18   * imc_pre +           # effet modéré
  22   * cpn_total +         # effet du suivi prénatal
  95   * cpn_precoce +       # bonus de précocité
  -4.5 * age_mere +
  38   * parite +
  105  * (sexe_enfant == "M") +
  rnorm(n, 0, 320)           # variabilité biologique résiduelle

poids_naissance <- round(pmax(900, pmin(5200, poids_naissance)))


# 4. Vaccination à 12 mois ------------------------------------

# VÉRITÉ TERRAIN Q3 : le suivi prénatal influence l'observance vaccinale,
# MAIS distance et instruction agissent sur les deux -> confusion volontaire.
score_obs <- 1.1 +
  0.16  * cpn_total +
  0.55  * cpn_precoce -
  0.055 * distance_cs +
  0.42  * (instruction %in% c("Secondaire", "Superieur")) +
  0.30  * (milieu == "Urbain")

doses_12m <- round(11 * plogis(score_obs - 1.2) + rnorm(n, 0, 1.1))
doses_12m <- pmax(0, pmin(11, doses_12m))

# VÉRITÉ TERRAIN Q2 — retard cumulé en jours (distribution asymétrique)
retard_cumule <- round(
  exp(3.9 - 0.11 * cpn_total - 0.38 * cpn_precoce +
        0.035 * distance_cs - 0.25 * (milieu == "Urbain") +
        rnorm(n, 0, 0.55))
)
retard_cumule <- pmin(retard_cumule, 400)


# 5. Assemblage -----------------------------------------------

donnees <- data.frame(
  id              = 1:n,
  district        = district,
  milieu          = milieu,
  age_mere        = age_mere,
  instruction     = instruction,
  parite          = parite,
  imc_pre         = imc_pre,
  distance_cs     = distance_cs,
  cpn_total       = cpn_total,
  cpn_precoce     = cpn_precoce,
  terme_sa        = terme_sa,
  sexe_enfant     = sexe_enfant,
  poids_naissance = poids_naissance,
  doses_12m       = doses_12m,
  retard_cumule   = retard_cumule
)


# 6. Dégradation volontaire -----------------------------------
# On abîme les données pour que le nettoyage soit un vrai exercice.

# a) Valeurs manquantes NON aléatoires : l'IMC manque plus souvent
#    quand la femme a eu peu de CPN (pas de pesée = pas de mesure)
idx_imc <- sample(which(donnees$cpn_total <= 3), 26)
donnees$imc_pre[idx_imc] <- NA

donnees$distance_cs[sample(n, 14)] <- NA
donnees$terme_sa[sample(n, 9)]     <- NA

# b) Modalités mal saisies (casse et orthographe incohérentes)
donnees$milieu[c(12, 87, 203)]      <- "urbain"
donnees$milieu[c(45, 156)]          <- "RURAL"
donnees$sexe_enfant[c(33, 199, 288)] <- "m"

# c) Valeurs aberrantes délibérées
donnees$age_mere[7]         <- 3      # âge impossible
donnees$poids_naissance[52] <- 47     # unité erronée (kg au lieu de g ?)
donnees$imc_pre[141]        <- 87.4   # IMC impossible
donnees$cpn_total[220]      <- 99     # code "inconnu" mal traduit

# d) Doublon
donnees <- rbind(donnees, donnees[89, ])
donnees$id[nrow(donnees)] <- 401


# 7. Export ---------------------------------------------------

write.csv(donnees, "data/raw/continuum_brut.csv", row.names = FALSE)

cat("Jeu généré :", nrow(donnees), "lignes,", ncol(donnees), "colonnes\n")
cat("Fichier écrit dans data/raw/continuum_brut.csv\n")