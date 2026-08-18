# =============================================================
# 03-descriptif.R
# Statistique descriptive univariée et bivariée
# Entrée : data/processed/continuum_propre.rds
# =============================================================

library(tidyverse)
library(gtsummary)
library(patchwork)

propre <- readRDS("data/processed/continuum_propre.rds")

# Thème graphique appliqué à TOUS les graphiques du script.
# Défini une fois, cohérence garantie.
theme_set(theme_minimal(base_size = 12))


# 1. Description univariée — quantitatives --------------------
# Moyenne ET médiane systématiquement : leur écart mesure
# l'asymétrie de la distribution.

propre |>
  select(where(is.numeric), -id) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valeur") |>
  group_by(variable) |>
  summarise(
    n       = sum(!is.na(valeur)),
    n_na    = sum(is.na(valeur)),
    moyenne = round(mean(valeur, na.rm = TRUE), 2),
    ecart_t = round(sd(valeur, na.rm = TRUE), 2),
    q1      = round(quantile(valeur, 0.25, na.rm = TRUE), 2),
    mediane = round(median(valeur, na.rm = TRUE), 2),
    q3      = round(quantile(valeur, 0.75, na.rm = TRUE), 2),
    min     = round(min(valeur, na.rm = TRUE), 2),
    max     = round(max(valeur, na.rm = TRUE), 2)
  ) |>
  print(n = Inf)


# 2. Description univariée — qualitatives ---------------------

propre |>
  select(where(is.factor)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "modalite") |>
  count(variable, modalite) |>
  group_by(variable) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  print(n = Inf)


# 3. Distributions — histogrammes -----------------------------
# La GRAMMAIRE de ggplot2 : données + aes() + geom_*() + habillage

h1 <- ggplot(propre, aes(x = poids_naissance)) +
  geom_histogram(bins = 30, fill = "#4C72B0", colour = "white") +
  geom_vline(xintercept = 2500, linetype = "dashed", colour = "red") +
  labs(title = "Poids de naissance",
       subtitle = "Trait rouge : seuil OMS de faible poids (2500 g)",
       x = "grammes", y = "effectif")

h2 <- ggplot(propre, aes(x = cpn_total)) +
  geom_bar(fill = "#55A868") +
  labs(title = "Consultations prénatales", x = "nombre", y = "effectif")

h3 <- ggplot(propre, aes(x = retard_cumule)) +
  geom_histogram(bins = 30, fill = "#C44E52", colour = "white") +
  labs(title = "Retard vaccinal cumulé",
       subtitle = "Distribution fortement asymétrique",
       x = "jours", y = "effectif")

h4 <- ggplot(propre, aes(x = log_retard)) +
  geom_histogram(bins = 30, fill = "#C44E52", colour = "white") +
  labs(title = "Retard vaccinal — échelle log",
       subtitle = "log(retard + 1) : la distribution se symétrise",
       x = "log(jours + 1)", y = "effectif")

(h1 | h2) / (h3 | h4)

ggsave("output/figures/distributions.png", width = 11, height = 8, dpi = 300)


# 4. Boxplots après nettoyage ---------------------------------
# À comparer avec ceux de l'étape 1 : l'échelle est enfin lisible.

b1 <- ggplot(propre, aes(y = age_mere)) + geom_boxplot() +
  labs(title = "Âge maternel", y = "années") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

b2 <- ggplot(propre, aes(y = imc_pre)) + geom_boxplot() +
  labs(title = "IMC pré-gestationnel", y = "kg/m²") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

b3 <- ggplot(propre, aes(y = poids_naissance)) + geom_boxplot() +
  labs(title = "Poids de naissance", y = "grammes") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

b4 <- ggplot(propre, aes(y = distance_cs)) + geom_boxplot() +
  labs(title = "Distance au centre de santé", y = "km") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

(b1 | b2) / (b3 | b4)


# 5. Table 1 — le tableau de publication ----------------------
# Convention universelle en recherche clinique : décrire
# l'échantillon global, puis par groupe d'exposition.

table1 <- propre |>
  select(age_mere, instruction, parite, imc_pre, distance_cs,
         cpn_total, cpn_precoce, terme_sa, sexe_enfant,
         poids_naissance, doses_12m, retard_cumule, milieu) |>
  tbl_summary(
    by = milieu,
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits  = all_continuous() ~ 1,
    missing = "ifany",
    missing_text = "Données manquantes",
    label = list(
      age_mere        ~ "Âge maternel (ans)",
      instruction     ~ "Niveau d'instruction",
      parite          ~ "Parité",
      imc_pre         ~ "IMC pré-gestationnel (kg/m²)",
      distance_cs     ~ "Distance au centre de santé (km)",
      cpn_total       ~ "Consultations prénatales",
      cpn_precoce     ~ "CPN précoce (1er trimestre)",
      terme_sa        ~ "Terme (SA)",
      sexe_enfant     ~ "Sexe de l'enfant",
      poids_naissance ~ "Poids de naissance (g)",
      doses_12m       ~ "Doses reçues à 12 mois",
      retard_cumule   ~ "Retard vaccinal cumulé (jours)"
    )
  ) |>
  add_overall() |>
  add_n() |>
  modify_header(label ~ "**Caractéristique**") |>
  modify_caption("**Tableau 1.** Description de l'échantillon (n = 400)")

table1


# 6. Exploration bivariée — quantitatif x quantitatif ---------

s1 <- ggplot(propre, aes(x = terme_sa, y = poids_naissance)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "#4C72B0") +
  labs(title = "Poids de naissance selon le terme",
       x = "terme (SA)", y = "poids (g)")

s2 <- ggplot(propre, aes(x = cpn_total, y = poids_naissance)) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#55A868") +
  labs(title = "Poids de naissance selon le suivi prénatal",
       x = "consultations prénatales", y = "poids (g)")

s3 <- ggplot(propre, aes(x = distance_cs, y = retard_cumule)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, colour = "#C44E52") +
  labs(title = "Retard vaccinal selon la distance",
       x = "distance (km)", y = "retard (jours)")

# Q3 — le pont entre les deux volets du continuum
s4 <- ggplot(propre, aes(x = cpn_total, y = doses_12m)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#8172B2") +
  labs(title = "Continuité du recours aux soins",
       subtitle = "Vaccination à 12 mois selon le suivi prénatal",
       x = "consultations prénatales", y = "doses reçues")

(s1 | s2) / (s3 | s4)

ggsave("output/figures/associations.png", width = 11, height = 8, dpi = 300)


# 7. Exploration bivariée — quantitatif x qualitatif ----------

g1 <- ggplot(propre, aes(x = milieu, y = cpn_total, fill = milieu)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Suivi prénatal selon le milieu", x = NULL, y = "CPN")

g2 <- ggplot(propre, aes(x = instruction, y = cpn_total, fill = instruction)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Suivi prénatal selon l'instruction", x = NULL, y = "CPN") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

g1 | g2


# 8. Matrice de corrélations ----------------------------------
# use = "pairwise.complete.obs" : chaque paire utilise les
# observations disponibles pour ELLE, sans supprimer les lignes
# incomplètes sur d'autres variables.

quanti <- propre |>
  select(age_mere, parite, imc_pre, distance_cs, cpn_total,
         terme_sa, poids_naissance, doses_12m, retard_cumule)

mat_cor <- cor(quanti, use = "pairwise.complete.obs")
round(mat_cor, 2)

mat_cor |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  ggplot(aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "#C44E52", mid = "white", high = "#4C72B0",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Corrélations entre variables quantitatives",
       x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 9. Tableaux croisés -----------------------------------------

table(propre$milieu, propre$cpn_adequat, dnn = c("Milieu", "CPN >= 4"))
round(100 * prop.table(table(propre$milieu, propre$cpn_adequat), margin = 1), 1)

table(propre$instruction, propre$complet_12m,
      dnn = c("Instruction", "Calendrier complet"))


# 10. Indicateurs clés ----------------------------------------

propre |>
  summarise(
    n                = n(),
    pct_fpn          = round(100 * mean(fpn, na.rm = TRUE), 1),
    pct_premature    = round(100 * mean(premature, na.rm = TRUE), 1),
    pct_cpn_adequat  = round(100 * mean(cpn_adequat, na.rm = TRUE), 1),
    pct_cpn_precoce  = round(100 * mean(cpn_precoce), 1),
    pct_complet_12m  = round(100 * mean(complet_12m), 1),
    poids_moyen      = round(mean(poids_naissance, na.rm = TRUE)),
    retard_median    = median(retard_cumule)
  )

# Indicateurs par district — format directement réutilisable
# pour un tableau de bord
propre |>
  group_by(district) |>
  summarise(
    n               = n(),
    cpn_moyen       = round(mean(cpn_total, na.rm = TRUE), 1),
    pct_complet_12m = round(100 * mean(complet_12m), 1),
    retard_median   = median(retard_cumule),
    .groups = "drop"
  ) |>
  arrange(desc(pct_complet_12m))