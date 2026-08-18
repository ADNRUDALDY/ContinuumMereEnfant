# =============================================================
# 06-continuite-q3.R
# Q3 : le suivi prénatal prédit-il l'observance vaccinale ?
# Illustration : association, confusion, et limites causales
# =============================================================

library(tidyverse)
library(broom)
library(car)
library(patchwork)

propre <- readRDS("data/processed/continuum_propre.rds")
theme_set(theme_minimal(base_size = 12))


# 1. L'association brute --------------------------------------

cor.test(propre$cpn_total, propre$doses_12m, use = "complete.obs")

ggplot(propre, aes(x = cpn_total, y = doses_12m)) +
  geom_jitter(width = 0.25, height = 0.25, alpha = 0.4) +
  geom_smooth(method = "lm", colour = "#8172B2") +
  labs(title = "Continuité du recours aux soins",
       subtitle = "Doses reçues à 12 mois selon le suivi prénatal",
       x = "consultations prénatales", y = "doses reçues")


# 2. Modèle 1 — non ajusté ------------------------------------
# Ce que voit un observateur naïf.

q3_brut <- lm(doses_12m ~ cpn_total, data = propre)
summary(q3_brut)


# 3. Modèle 2 — ajusté sur les facteurs d'accès ---------------
# distance, instruction et milieu influencent LES DEUX variables.
# C'est la définition du facteur de confusion.

q3_ajuste <- lm(
  doses_12m ~ cpn_total + cpn_precoce + distance_cs +
    instruction + milieu + age_mere + parite,
  data = propre
)

summary(q3_ajuste)


# 4. De combien le coefficient a-t-il changé ? ----------------
# C'est LA mesure de la confusion.

comparaison <- bind_rows(
  tidy(q3_brut,   conf.int = TRUE) |> mutate(modele = "1. Non ajusté"),
  tidy(q3_ajuste, conf.int = TRUE) |> mutate(modele = "2. Ajusté")
) |>
  filter(term == "cpn_total") |>
  select(modele, estimate, std.error, conf.low, conf.high, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

comparaison

# Pourcentage de l'association brute attribuable à la confusion
b_brut   <- coef(q3_brut)["cpn_total"]
b_ajuste <- coef(q3_ajuste)["cpn_total"]

cat("Coefficient brut   :", round(b_brut, 3), "\n")
cat("Coefficient ajusté :", round(b_ajuste, 3), "\n")
cat("Réduction          :", round(100 * (b_brut - b_ajuste) / b_brut, 1), "%\n")


# 5. Ajustement variable par variable -------------------------
# On isole la contribution de chaque facteur de confusion.

formules <- list(
  "cpn_total seul"        = doses_12m ~ cpn_total,
  "+ distance"            = doses_12m ~ cpn_total + distance_cs,
  "+ instruction"         = doses_12m ~ cpn_total + distance_cs + instruction,
  "+ milieu"              = doses_12m ~ cpn_total + distance_cs + instruction + milieu,
  "+ précocité CPN"       = doses_12m ~ cpn_total + distance_cs + instruction + milieu + cpn_precoce
)

map_dfr(names(formules), function(nom) {
  mod <- lm(formules[[nom]], data = propre)
  tidy(mod, conf.int = TRUE) |>
    filter(term == "cpn_total") |>
    mutate(modele = nom, r2 = summary(mod)$r.squared)
}) |>
  select(modele, estimate, conf.low, conf.high, p.value, r2) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))


# 6. Le même test sur le retard vaccinal ----------------------
# Cohérence : si la continuité existe, elle doit se voir aussi
# sur le retard, avec un signe OPPOSÉ.

q3_retard <- lm(
  log_retard ~ cpn_total + cpn_precoce + distance_cs +
    instruction + milieu + age_mere,
  data = propre
)

tidy(q3_retard, conf.int = TRUE) |>
  filter(term %in% c("cpn_total", "cpn_precoceTRUE")) |>
  mutate(
    variation_pct = round(100 * (exp(estimate) - 1), 1),
    across(where(is.numeric), ~ round(.x, 4))
  ) |>
  select(term, estimate, variation_pct, conf.low, conf.high, p.value)


# 7. Diagnostics ----------------------------------------------

diag_q3 <- augment(q3_ajuste)

q1p <- ggplot(diag_q3, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_smooth(se = FALSE, colour = "#4C72B0") +
  labs(title = "Résidus vs ajustées", x = "ajustées", y = "résidus")

q2p <- ggplot(diag_q3, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5) + stat_qq_line(colour = "red") +
  labs(title = "Q-Q plot", x = "quantiles théoriques", y = "observés")

q1p | q2p

vif(q3_ajuste)


# 8. Le point aveugle : la variable non mesurée ---------------
# Simulation d'un facteur de confusion NON OBSERVÉ.
# On fabrique une "motivation maternelle" qui influencerait
# les deux variables, pour voir ce qu'elle ferait au résultat.

set.seed(99)

test_confusion <- propre |>
  filter(!is.na(cpn_total)) |>
  mutate(
    # Variable latente fictive, corrélée aux deux comportements
    motivation = 0.5 * scale(cpn_total)[, 1] +
      0.5 * scale(doses_12m)[, 1] +
      rnorm(n(), 0, 0.5)
  )

m_sans_motivation <- lm(doses_12m ~ cpn_total + distance_cs + instruction,
                        data = test_confusion)
m_avec_motivation <- lm(doses_12m ~ cpn_total + distance_cs + instruction + motivation,
                        data = test_confusion)

bind_rows(
  tidy(m_sans_motivation) |> filter(term == "cpn_total") |> mutate(modele = "Sans motivation"),
  tidy(m_avec_motivation) |> filter(term == "cpn_total") |> mutate(modele = "Avec motivation")
) |>
  select(modele, estimate, std.error, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Le coefficient s'effondre. Or cette variable n'est PAS dans
# tes données réelles. C'est la limite structurelle de Q3.


# 9. Synthèse des trois questions -----------------------------

m_q1 <- readRDS("output/modele_q1.rds")
m_q2 <- readRDS("output/modele_q2.rds")

bind_rows(
  tidy(m_q1, conf.int = TRUE) |> filter(term == "cpn_total") |>
    mutate(question = "Q1 — poids de naissance (g)"),
  tidy(m_q2, conf.int = TRUE) |> filter(term == "cpn_total") |>
    mutate(question = "Q2 — log(retard vaccinal)"),
  tidy(q3_ajuste, conf.int = TRUE) |> filter(term == "cpn_total") |>
    mutate(question = "Q3 — doses à 12 mois")
) |>
  select(question, estimate, conf.low, conf.high, p.value) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))


# 10. Graphique de synthèse -----------------------------------

propre |>
  mutate(cpn_groupe = cut(cpn_total,
                          breaks = c(-1, 1, 3, 5, 20),
                          labels = c("0-1", "2-3", "4-5", "6+"))) |>
  filter(!is.na(cpn_groupe)) |>
  group_by(cpn_groupe) |>
  summarise(
    n              = n(),
    doses_moyennes = mean(doses_12m),
    erreur         = sd(doses_12m) / sqrt(n()),
    .groups = "drop"
  ) |>
  ggplot(aes(x = cpn_groupe, y = doses_moyennes)) +
  geom_col(fill = "#8172B2", width = 0.6) +
  geom_errorbar(aes(ymin = doses_moyennes - 1.96 * erreur,
                    ymax = doses_moyennes + 1.96 * erreur),
                width = 0.15) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.5, y = 0.3, size = 3) +
  labs(title = "Observance vaccinale selon l'intensité du suivi prénatal",
       subtitle = "Moyenne des doses reçues à 12 mois et IC 95 %",
       x = "consultations prénatales", y = "doses reçues à 12 mois")

ggsave("output/figures/continuite_q3.png", width = 9, height = 6, dpi = 300)


# 11. Sauvegarde ----------------------------------------------

saveRDS(q3_ajuste, "output/modele_q3.rds")
write_csv(tidy(q3_ajuste, conf.int = TRUE), "output/tables/coefficients_q3.csv")