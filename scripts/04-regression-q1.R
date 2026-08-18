# =============================================================
# 04-regression-q1.R
# Q1 : déterminants du poids de naissance
# Régression linéaire multiple
# =============================================================

library(tidyverse)
library(broom)
library(car)
library(performance)
library(patchwork)

propre <- readRDS("data/processed/continuum_propre.rds")
theme_set(theme_minimal(base_size = 12))


# 1. Modèle naïf : le suivi prénatal seul --------------------
# Question : quel effet apparent des CPN si on n'ajuste sur rien ?

m_naif <- lm(poids_naissance ~ cpn_total, data = propre)
summary(m_naif)

# Retiens ce coefficient. Il va changer.


# 2. Ajout progressif des variables --------------------------
# On construit par couches pour VOIR l'effet de chaque ajustement.

m2 <- lm(poids_naissance ~ cpn_total + cpn_precoce, data = propre)
summary(m2)
# Le coefficient de cpn_total a-t-il bougé ? Pourquoi ?

m3 <- lm(poids_naissance ~ cpn_total + cpn_precoce + terme_sa, data = propre)
summary(m3)


# 3. Modèle complet ------------------------------------------

m_complet <- lm(
  poids_naissance ~ cpn_total + cpn_precoce + terme_sa + imc_pre +
    age_mere + parite + sexe_enfant + milieu + instruction,
  data = propre
)

summary(m_complet)


# 4. Sortie propre avec broom --------------------------------
# tidy() convertit la sortie du modèle en data.frame exploitable.

resultats <- tidy(m_complet, conf.int = TRUE) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

print(resultats, n = Inf)

# Indicateurs globaux du modèle
glance(m_complet) |>
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df, nobs)


# 5. Confrontation à la vérité terrain -----------------------
# Exercice impossible sur des données réelles : on connaît la réponse.

verite <- tribble(
  ~term,            ~valeur_vraie,
  "cpn_total",       22,
  "cpn_precoceTRUE", 95,
  "terme_sa",        62,
  "imc_pre",         18,
  "age_mere",        -4.5,
  "parite",          38,
  "sexe_enfantM",    105
)

resultats |>
  select(term, estimate, conf.low, conf.high) |>
  inner_join(verite, by = "term") |>
  mutate(
    ecart      = round(estimate - valeur_vraie, 1),
    dans_ic95  = valeur_vraie >= conf.low & valeur_vraie <= conf.high
  )

# La colonne dans_ic95 est le vrai test : l'intervalle de
# confiance à 95 % contient-il la valeur réelle ?


# 6. Colinéarité ---------------------------------------------
# VIF > 5 : à surveiller. VIF > 10 : problème sérieux.

vif(m_complet)


# 7. Diagnostics des résidus ---------------------------------
# Les quatre hypothèses du modèle linéaire se vérifient ICI.

par(mfrow = c(2, 2))
plot(m_complet)
par(mfrow = c(1, 1))

# Version ggplot2, plus lisible
diag <- augment(m_complet)

d1 <- ggplot(diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  geom_smooth(se = FALSE, colour = "#4C72B0") +
  labs(title = "Résidus vs valeurs ajustées",
       subtitle = "Attendu : nuage sans structure autour de 0",
       x = "valeurs ajustées", y = "résidus")

d2 <- ggplot(diag, aes(sample = .std.resid)) +
  stat_qq(alpha = 0.5) + stat_qq_line(colour = "red") +
  labs(title = "Droite de Henry (Q-Q plot)",
       subtitle = "Attendu : points alignés sur la droite",
       x = "quantiles théoriques", y = "quantiles observés")

d3 <- ggplot(diag, aes(x = .fitted, y = sqrt(abs(.std.resid)))) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, colour = "#C44E52") +
  labs(title = "Homoscédasticité",
       subtitle = "Attendu : ligne horizontale",
       x = "valeurs ajustées", y = "√|résidus standardisés|")

d4 <- ggplot(diag, aes(x = .resid)) +
  geom_histogram(bins = 30, fill = "#4C72B0", colour = "white") +
  labs(title = "Distribution des résidus",
       subtitle = "Attendu : approximativement normale",
       x = "résidus", y = "effectif")

(d1 | d2) / (d3 | d4)

ggsave("output/figures/diagnostics_q1.png", width = 11, height = 8, dpi = 300)

# Vérification automatisée des hypothèses
check_model(m_complet)


# 8. Observations influentes ---------------------------------
# Distance de Cook : seuil d'alerte usuel 4/n

seuil_cook <- 4 / nrow(diag)

diag |>
  mutate(ligne = row_number()) |>
  filter(.cooksd > seuil_cook) |>
  select(ligne, poids_naissance, terme_sa, cpn_total, .fitted, .resid, .cooksd) |>
  arrange(desc(.cooksd)) |>
  head(10)


# 9. Le problème des valeurs manquantes ----------------------
# lm() supprime silencieusement les lignes incomplètes.
# Combien d'observations le modèle utilise-t-il réellement ?

nrow(propre)
nobs(m_complet)
nrow(propre) - nobs(m_complet)   # lignes perdues

# Les lignes perdues sont-elles particulières ?
propre |>
  mutate(exclue = !complete.cases(
    select(propre, poids_naissance, cpn_total, cpn_precoce, terme_sa,
           imc_pre, age_mere, parite, sexe_enfant, milieu, instruction)
  )) |>
  group_by(exclue) |>
  summarise(
    n         = n(),
    cpn_moyen = round(mean(cpn_total, na.rm = TRUE), 2),
    dist_moy  = round(mean(distance_cs, na.rm = TRUE), 1)
  )

# Modèle SANS imc_pre : on récupère les 26 lignes perdues.
# Le coefficient des CPN change-t-il ?

m_sans_imc <- lm(
  poids_naissance ~ cpn_total + cpn_precoce + terme_sa +
    age_mere + parite + sexe_enfant + milieu + instruction,
  data = propre
)

bind_rows(
  tidy(m_complet)   |> filter(term == "cpn_total") |> mutate(modele = "Avec IMC"),
  tidy(m_sans_imc)  |> filter(term == "cpn_total") |> mutate(modele = "Sans IMC")
) |>
  select(modele, estimate, std.error, p.value, ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))


# 10. Comparaison des modèles --------------------------------

bind_rows(
  glance(m_naif)    |> mutate(modele = "1. CPN seul"),
  glance(m2)        |> mutate(modele = "2. + précocité"),
  glance(m3)        |> mutate(modele = "3. + terme"),
  glance(m_complet) |> mutate(modele = "4. Complet")
) |>
  select(modele, r.squared, adj.r.squared, sigma, AIC, nobs) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))


# 11. Graphique des coefficients -----------------------------

resultats |>
  filter(term != "(Intercept)") |>
  mutate(term = fct_reorder(term, estimate)) |>
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high)) +
  labs(title = "Déterminants du poids de naissance",
       subtitle = "Coefficients ajustés et intervalles de confiance à 95 %",
       x = "effet sur le poids (grammes)", y = NULL)

ggsave("output/figures/coefficients_q1.png", width = 9, height = 6, dpi = 300)


# 12. Sauvegarde ---------------------------------------------

saveRDS(m_complet, "output/modele_q1.rds")
write_csv(resultats, "output/tables/coefficients_q1.csv")