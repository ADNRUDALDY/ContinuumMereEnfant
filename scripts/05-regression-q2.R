# =============================================================
# 05-regression-q2.R
# Q2 : déterminants du retard vaccinal cumulé à 12 mois
# Illustration : pourquoi et comment transformer la réponse
# =============================================================

library(tidyverse)
library(broom)
library(car)
library(patchwork)

propre <- readRDS("data/processed/continuum_propre.rds")
theme_set(theme_minimal(base_size = 12))


# 1. La réponse est-elle adaptée au modèle linéaire ? ---------

propre |>
  summarise(
    n        = n(),
    moyenne  = round(mean(retard_cumule), 1),
    mediane  = median(retard_cumule),
    ecart_t  = round(sd(retard_cumule), 1),
    min      = min(retard_cumule),
    max      = max(retard_cumule),
    # Coefficient d'asymétrie : 0 = symétrique, > 1 = forte queue droite
    skewness = round(mean((retard_cumule - mean(retard_cumule))^3) /
                       sd(retard_cumule)^3, 2)
  )

# Moyenne nettement > médiane : signature d'une queue à droite.


# 2. Modèle NAÏF sur l'échelle brute --------------------------
# On le construit délibérément pour observer son échec.

m_brut <- lm(
  retard_cumule ~ cpn_total + cpn_precoce + distance_cs + milieu +
    instruction + age_mere,
  data = propre
)

summary(m_brut)

# Le R² semble correct. Les diagnostics vont dire autre chose.


# 3. Diagnostics du modèle brut -------------------------------

diag_brut <- augment(m_brut)

b1 <- ggplot(diag_brut, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_smooth(se = FALSE, colour = "#4C72B0") +
  labs(title = "Brut — résidus vs ajustées",
       subtitle = "Forme en entonnoir : hétéroscédasticité",
       x = "ajustées", y = "résidus")

b2 <- ggplot(diag_brut, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5) + stat_qq_line(colour = "red") +
  labs(title = "Brut — Q-Q plot",
       subtitle = "Décrochage en haut à droite",
       x = "quantiles théoriques", y = "observés")

b3 <- ggplot(diag_brut, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, colour = "#C44E52") +
  labs(title = "Brut — homoscédasticité",
       subtitle = "Ligne croissante : variance non constante",
       x = "ajustées", y = "√|résidus std|")

b4 <- ggplot(diag_brut, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "#C44E52", colour = "white") +
  labs(title = "Brut — résidus", subtitle = "Asymétrie visible",
       x = "résidus", y = "effectif")

(b1 | b2) / (b3 | b4)

# Prédictions négatives : impossible pour un retard en jours
sum(diag_brut$.fitted < 0)
min(diag_brut$.fitted)

# Tests formels
car::ncvTest(m_brut)              # H0 : variance constante
shapiro.test(residuals(m_brut))   # H0 : résidus normaux


# 4. Modèle sur l'échelle logarithmique -----------------------
# log_retard = log(retard + 1), créé à l'étape 2.

m_log <- lm(
  log_retard ~ cpn_total + cpn_precoce + distance_cs + milieu +
    instruction + age_mere,
  data = propre
)

summary(m_log)


# 5. Diagnostics du modèle log --------------------------------

diag_log <- augment(m_log)

l1 <- ggplot(diag_log, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_smooth(se = FALSE, colour = "#4C72B0") +
  labs(title = "Log — résidus vs ajustées",
       subtitle = "Nuage sans structure", x = "ajustées", y = "résidus")

l2 <- ggplot(diag_log, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5) + stat_qq_line(colour = "red") +
  labs(title = "Log — Q-Q plot", subtitle = "Alignement satisfaisant",
       x = "quantiles théoriques", y = "observés")

l3 <- ggplot(diag_log, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, colour = "#55A868") +
  labs(title = "Log — homoscédasticité", subtitle = "Ligne horizontale",
       x = "ajustées", y = "√|résidus std|")

l4 <- ggplot(diag_log, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "#55A868", colour = "white") +
  labs(title = "Log — résidus", subtitle = "Distribution symétrique",
       x = "résidus", y = "effectif")

(l1 | l2) / (l3 | l4)

ggsave("output/figures/diagnostics_q2_log.png", width = 11, height = 8, dpi = 300)

car::ncvTest(m_log)
shapiro.test(residuals(m_log))


# 6. Confrontation à la vérité terrain ------------------------
# Les coefficients de la simulation vivent sur l'échelle log :
# ils sont donc DIRECTEMENT comparables à ceux de m_log.

verite_q2 <- tribble(
  ~term,             ~valeur_vraie,
  "(Intercept)",      3.9,
  "cpn_total",       -0.11,
  "cpn_precoceTRUE", -0.38,
  "distance_cs",      0.035,
  "milieuUrbain",    -0.25
)

tidy(m_log, conf.int = TRUE) |>
  inner_join(verite_q2, by = "term") |>
  mutate(
    across(c(estimate, conf.low, conf.high), ~ round(.x, 3)),
    ecart     = round(estimate - valeur_vraie, 3),
    dans_ic95 = valeur_vraie >= conf.low & valeur_vraie <= conf.high
  ) |>
  select(term, estimate, conf.low, conf.high, valeur_vraie, ecart, dans_ic95)

# Écart-type résiduel : la simulation utilisait 0.55
sigma(m_log)


# 7. Interprétation multiplicative ----------------------------
# Sur l'échelle log, exp(coefficient) est un RATIO, pas une
# différence. (exp(b) - 1) * 100 donne la variation en %.

tidy(m_log, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    ratio       = round(exp(estimate), 3),
    variation_p = round(100 * (exp(estimate) - 1), 1),
    ic_bas_p    = round(100 * (exp(conf.low) - 1), 1),
    ic_haut_p   = round(100 * (exp(conf.high) - 1), 1),
    p.value     = round(p.value, 4)
  ) |>
  select(term, ratio, variation_p, ic_bas_p, ic_haut_p, p.value)


# 8. Comparaison des deux modèles -----------------------------
# ATTENTION : les R² ne sont PAS comparables entre échelles.
# On compare les DIAGNOSTICS, pas les indicateurs d'ajustement.

tibble(
  modele        = c("Brut", "Log"),
  r2            = c(summary(m_brut)$r.squared, summary(m_log)$r.squared),
  p_ncv         = c(ncvTest(m_brut)$p, ncvTest(m_log)$p),
  p_shapiro     = c(shapiro.test(residuals(m_brut))$p.value,
                    shapiro.test(residuals(m_log))$p.value),
  pred_negatives = c(sum(augment(m_brut)$.fitted < 0),
                     sum(exp(augment(m_log)$.fitted) - 1 < 0))
) |>
  mutate(across(where(is.numeric), ~ signif(.x, 3)))


# 9. Prédictions sur l'échelle d'origine ----------------------
# Profils contrastés, pour illustrer l'ampleur des effets.

profils <- tibble(
  profil      = c("Favorable", "Intermédiaire", "Défavorable"),
  cpn_total   = c(8L, 4L, 1L),
  cpn_precoce = c(TRUE, TRUE, FALSE),
  distance_cs = c(1, 6, 25),
  milieu      = factor(c("Urbain", "Urbain", "Rural"),
                       levels = c("Rural", "Urbain")),
  instruction = factor(c("Superieur", "Secondaire", "Aucune"),
                       levels = c("Aucune", "Primaire", "Secondaire", "Superieur")),
  age_mere    = c(28, 28, 28)
)

pred_log <- predict(m_log, newdata = profils, interval = "confidence")

profils |>
  bind_cols(as_tibble(pred_log)) |>
  mutate(
    retard_estime = round(exp(fit) - 1),
    ic_bas        = round(exp(lwr) - 1),
    ic_haut       = round(exp(upr) - 1)
  ) |>
  select(profil, cpn_total, distance_cs, milieu,
         retard_estime, ic_bas, ic_haut)


# 10. Graphique des effets ------------------------------------

tidy(m_log, conf.int = TRUE) |>
  filter(term != "(Intercept)") |>
  mutate(
    across(c(estimate, conf.low, conf.high), exp),
    term = fct_reorder(term, estimate)
  ) |>
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  scale_x_log10() +
  labs(title = "Déterminants du retard vaccinal",
       subtitle = "Ratios multiplicatifs et IC 95 % — 1 = aucun effet",
       x = "ratio de retard (échelle log)", y = NULL)

ggsave("output/figures/coefficients_q2.png", width = 9, height = 6, dpi = 300)


# 11. Sauvegarde ----------------------------------------------

saveRDS(m_log, "output/modele_q2.rds")
write_csv(tidy(m_log, conf.int = TRUE), "output/tables/coefficients_q2.csv")