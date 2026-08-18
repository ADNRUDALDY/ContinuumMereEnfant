# =============================================================
# app.R — Continuum de soins mère-enfant
# Application de présentation des résultats de l'étude
# =============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(broom)
library(DT)
library(here)

# --- Chargement unique, au démarrage du serveur --------------
# Ce code s'exécute UNE FOIS, pas à chaque interaction.
# here() localise la racine du projet (fichier .Rproj) quel que
# soit le répertoire de travail utilisé pour lancer l'app.

propre <- readRDS(here("data", "processed", "continuum_propre.rds"))
theme_set(theme_minimal(base_size = 13))

# Modèles de référence, ajustés sur l'échantillon complet
m_q1 <- lm(poids_naissance ~ cpn_total + cpn_precoce + terme_sa + imc_pre +
             age_mere + parite + sexe_enfant + milieu + instruction,
           data = propre)

m_q2 <- lm(log_retard ~ cpn_total + cpn_precoce + distance_cs + milieu +
             instruction + age_mere,
           data = propre)

m_q3 <- lm(doses_12m ~ cpn_total + cpn_precoce + distance_cs +
             instruction + milieu + age_mere + parite,
           data = propre)

# Étiquettes lisibles pour les graphiques de coefficients
jolis_noms <- c(
  cpn_total = "Consultations prénatales",
  cpn_precoceTRUE = "CPN précoce (1er trim.)",
  terme_sa = "Terme (SA)",
  imc_pre = "IMC pré-gestationnel",
  age_mere = "Âge maternel",
  parite = "Parité",
  sexe_enfantM = "Sexe : masculin",
  milieuUrbain = "Milieu : urbain",
  distance_cs = "Distance au centre",
  instructionPrimaire = "Instruction : primaire",
  instructionSecondaire = "Instruction : secondaire",
  instructionSuperieur = "Instruction : supérieur"
)

# Fonction réutilisable : graphique en forêt des coefficients
graphe_coefs <- function(modele, titre, sous_titre, unite) {
  tidy(modele, conf.int = TRUE) |>
    filter(term != "(Intercept)") |>
    mutate(label = coalesce(jolis_noms[term], term),
           label = fct_reorder(label, estimate),
           signif = if_else(p.value < 0.05, "p < 0,05", "p ≥ 0,05")) |>
    ggplot(aes(x = estimate, y = label, colour = signif)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6) +
    scale_colour_manual(values = c("p < 0,05" = "#2C6E9B", "p ≥ 0,05" = "grey60")) +
    labs(title = titre, subtitle = sous_titre, x = unite, y = NULL,
         colour = NULL) +
    theme(legend.position = "bottom")
}


# =============================================================
# INTERFACE
# =============================================================

ui <- page_navbar(
  title = "Continuum de soins mère-enfant",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  # fillable = FALSE : la page défile normalement au lieu de forcer tout
  # le contenu de chaque onglet dans la hauteur exacte de la fenêtre.
  fillable = FALSE,
  
  # --- Onglet 1 : contexte -----------------------------------
  nav_panel(
    "Accueil",
    icon = icon("house"),
    div(
      class = "p-4 p-md-5 mb-4 rounded-3",
      style = "background: linear-gradient(135deg, #2C6E9B 0%, #1a3f5c 100%); color: white;",
      h1(class = "fw-bold", "Continuum de soins mère-enfant"),
      p(class = "lead mb-2",
        "Le suivi d'une grossesse et la vaccination d'un enfant relèvent-ils
        du même parcours de soins ? Ce site présente, en langage accessible,
        les résultats d'une étude statistique explorant cette question."),
      p(class = "mb-0 opacity-75",
        "Aucune connaissance préalable en statistique ou en épidémiologie
        n'est requise pour parcourir ce site — chaque onglet propose un
        « Guide de lecture » qui explique comment interpréter les graphiques.")
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header(icon("book-open"), " De quoi parle cette étude ?"),
        markdown("
Le **continuum de soins mère-enfant** est l'idée qu'une femme bien suivie
pendant sa grossesse, et un enfant correctement vacciné pendant sa première
année de vie, relèvent souvent d'un même parcours de recours aux soins :
la même mère, la même famille, le même accès aux services de santé.

Cette étude explore trois questions concrètes à partir de données portant
sur **400 couples mère-enfant** :

1. **Poids de naissance** — Quels facteurs (suivi prénatal, terme,
   caractéristiques maternelles...) sont associés au poids du bébé ?
2. **Retard vaccinal** — Quels facteurs expliquent qu'un enfant reçoive ses
   vaccins en retard par rapport au calendrier recommandé ?
3. **Continuité des soins** — Une mère bien suivie pendant sa grossesse
   a-t-elle un enfant mieux vacciné, même après avoir tenu compte d'autres
   facteurs comme la distance au centre de santé ?

Chaque question est traitée dans son propre onglet, avec un graphique
principal, un tableau de résultats et une explication en langage courant.
        ")
      ),
      card(
        card_header(icon("triangle-exclamation"), " Avertissement important"),
        div(
          class = "alert alert-warning mb-0",
          strong("Données simulées. "),
          "Ce jeu de données a été généré artificiellement, à des fins
          pédagogiques. Les résultats reflètent uniquement les paramètres
          encodés dans la simulation et n'ont aucune valeur épidémiologique
          réelle. Ils ne doivent en aucun cas être utilisés pour orienter une
          décision clinique ou de santé publique."
        )
      )
    ),
    layout_columns(
      value_box("Couples mère-enfant suivis", nrow(propre), showcase = icon("users")),
      value_box("Poids de naissance moyen",
                paste0(round(mean(propre$poids_naissance, na.rm = TRUE)), " g"),
                showcase = icon("baby")),
      value_box("Consultations prénatales, en moyenne",
                round(mean(propre$cpn_total, na.rm = TRUE), 1),
                showcase = icon("stethoscope")),
      value_box("Calendrier vaccinal complet",
                paste0(round(100 * mean(propre$complet_12m), 1), " %"),
                showcase = icon("syringe"))
    ),
    card(
      card_header(icon("compass"), " Comment naviguer sur ce site"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        div(
          h5(icon("magnifying-glass"), " Exploration"),
          p(class = "text-muted small",
            "Regardez les données brutes vous-même : filtrez par région,
            choisissez les variables à comparer, observez les tendances.")
        ),
        div(
          h5(icon("weight-scale"), " Q1 — Poids"),
          p(class = "text-muted small",
            "Quels facteurs sont liés au poids de naissance, et de combien
            de grammes chacun compte-t-il ?")
        ),
        div(
          h5(icon("syringe"), " Q2 — Retard vaccinal"),
          p(class = "text-muted small",
            "Quels facteurs accélèrent ou retardent la vaccination, en
            pourcentage ?")
        ),
        div(
          h5(icon("link"), " Q3 — Continuité"),
          p(class = "text-muted small",
            "Le suivi prénatal est-il lié à une meilleure vaccination,
            même après ajustement ?")
        )
      )
    ),
    accordion(
      open = FALSE,
      accordion_panel(
        "Glossaire — les termes utilisés sur ce site",
        icon = icon("spell-check"),
        layout_columns(
          col_widths = c(6, 6),
          markdown("
**CPN (consultation prénatale)** — Visite médicale de suivi pendant la
grossesse. Plus il y en a, plus le suivi est considéré comme complet.

**CPN précoce** — Première consultation prénatale effectuée au premier
trimestre de la grossesse, comme recommandé.

**Terme (SA)** — Âge de la grossesse à la naissance, exprimé en semaines
d'aménorrhée. Un terme normal se situe autour de 37 à 41 SA.

**IMC pré-gestationnel** — Indice de masse corporelle de la mère avant la
grossesse (poids / taille²), un indicateur de son état nutritionnel.

**Faible poids de naissance** — Seuil défini par l'OMS à 2 500 g ; en deçà,
le risque de complications néonatales augmente.
          "),
          markdown("
**Retard vaccinal cumulé** — Nombre total de jours de retard accumulés sur
l'ensemble des vaccins prévus au cours de la première année.

**Calendrier vaccinal complet** — L'enfant a reçu, à 12 mois, toutes les
doses recommandées pour son âge.

**Coefficient ajusté** — Effet estimé d'un facteur sur le résultat étudié,
une fois que l'on a « neutralisé » l'influence des autres facteurs inclus
dans le modèle.

**Intervalle de confiance (IC) à 95 %** — Fourchette de valeurs plausibles
pour l'effet réel. Plus elle est étroite, plus l'estimation est précise.

**Valeur p** — Probabilité d'observer un effet au moins aussi marqué si,
en réalité, il n'existait aucun effet. Par convention, on parle d'un
résultat statistiquement significatif lorsque p < 0,05.
          ")
        )
      ),
      accordion_panel(
        "Comment lire les graphiques de coefficients (Q1, Q2, Q3) ?",
        icon = icon("chart-line"),
        markdown("
La plupart des graphiques de ce site suivent le même principe, appelé
**graphique en forêt** :

- Chaque **ligne** correspond à un facteur (âge maternel, distance au
  centre de santé, etc.).
- Le **point** est l'estimation de l'effet de ce facteur.
- Le **trait horizontal** autour du point est l'intervalle de confiance à
  95 % : plus il est court, plus l'estimation est précise.
- La **ligne pointillée verticale** marque l'absence d'effet (0 pour un
  effet additif, 1 pour un effet multiplicatif comme au Q2).
- La **couleur** indique si l'effet est statistiquement significatif
  (bleu ou rouge) ou non distinguable de l'absence d'effet (gris) : un
  intervalle de confiance qui chevauche la ligne pointillée signale un
  effet non significatif.
        ")
      )
    )
  ),
  
  # --- Onglet 2 : exploration --------------------------------
  nav_panel(
    "Exploration",
    icon = icon("magnifying-glass"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Guide de lecture — Onglet Exploration",
        icon = icon("circle-info"),
        markdown("
Cet onglet permet d'**explorer librement les données**, sans passer par un
modèle statistique.

- Les filtres à gauche (district, milieu, âge maternel) réduisent
  l'échantillon affiché ; le nombre d'observations restantes est indiqué
  au-dessus du tableau.
- Le **nuage de points** montre la relation entre deux variables au choix,
  avec une droite de tendance (régression linéaire simple) et sa zone
  d'incertitude en gris.
- L'**histogramme** montre comment la variable en ordonnée se distribue
  dans l'échantillon filtré : est-elle concentrée, étalée, asymétrique ?
- Le **tableau** liste les observations individuelles filtrées — utile
  pour vérifier une valeur précise ou repérer une donnée atypique.

Une tendance visible ici est une simple **association brute**, sans prise
en compte d'autres facteurs ; les onglets Q1 à Q3 affinent ces relations
avec des modèles ajustés.
        ")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectInput("districts", "Districts",
                    choices = levels(propre$district),
                    selected = levels(propre$district),
                    multiple = TRUE),
        selectInput("milieux", "Milieu",
                    choices = levels(propre$milieu),
                    selected = levels(propre$milieu),
                    multiple = TRUE),
        sliderInput("age", "Âge maternel",
                    min = 15, max = 45, value = c(15, 45)),
        hr(),
        selectInput("var_x", "Variable en abscisse",
                    choices = c("Consultations prénatales" = "cpn_total",
                                "Distance au centre" = "distance_cs",
                                "Terme (SA)" = "terme_sa",
                                "IMC pré-gestationnel" = "imc_pre",
                                "Âge maternel" = "age_mere")),
        selectInput("var_y", "Variable en ordonnée",
                    choices = c("Poids de naissance" = "poids_naissance",
                                "Retard vaccinal" = "retard_cumule",
                                "Doses à 12 mois" = "doses_12m"),
                    selected = "poids_naissance"),
        checkboxInput("couleur", "Colorer par milieu", value = TRUE)
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Nuage de points"),
             plotOutput("nuage", height = "420px")),
        card(card_header("Distribution"),
             plotOutput("distrib", height = "420px"))
      ),
      card(card_header(textOutput("titre_table")),
           DTOutput("table"))
    )
  ),
  
  # --- Onglet 3 : Q1 -----------------------------------------
  nav_panel(
    "Q1 — Poids de naissance",
    icon = icon("weight-scale"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Guide de lecture — Onglet Q1",
        icon = icon("circle-info"),
        markdown("
**Question posée** : quels facteurs, mesurés pendant la grossesse ou à
l'accouchement, sont associés au poids de naissance du bébé ?

**Méthode** : une régression linéaire multiple estime l'effet propre de
chaque facteur (en grammes), toutes choses égales par ailleurs.

**Comment lire le graphique « Coefficients ajustés »** : chaque ligne est
un facteur ; le point est l'effet estimé en grammes ; le trait est
l'intervalle de confiance à 95 %. Un facteur à droite de la ligne
pointillée (0) est associé à un poids plus élevé, à gauche à un poids plus
faible. Le bleu signale un effet statistiquement significatif (p < 0,05).

**Le simulateur** en bas de page applique le modèle à un profil que vous
définissez vous-même, pour visualiser une prédiction concrète.
        ")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Coefficients ajustés"),
           plotOutput("coefs_q1", height = "520px")),
      card(
        card_header("Lecture"),
        markdown(sprintf("
Le modèle explique **%.1f %%** de la variance du poids de naissance
(R² ajusté = %.3f). L'écart-type résiduel est de **%.0f g**.

Cette proportion, modeste en apparence, reflète la variabilité biologique
intrinsèque du poids de naissance : une part majoritaire de la variation
individuelle échappe aux facteurs mesurés.

Le terme est le déterminant dominant. L'effet du suivi prénatal persiste
après ajustement, mais son intervalle de confiance reste large.

Les coefficients du milieu et de l'instruction ne se distinguent pas de
zéro : leur influence transite entièrement par le nombre de consultations,
déjà présent dans le modèle.
        ",
                         100 * summary(m_q1)$r.squared,
                         summary(m_q1)$adj.r.squared,
                         sigma(m_q1)))
      )
    ),
    card(card_header("Simulateur de prédiction"),
         layout_sidebar(
           sidebar = sidebar(
             position = "right", width = 280,
             sliderInput("p_terme", "Terme (SA)", 32, 42, 39, step = 0.5),
             sliderInput("p_cpn", "Consultations prénatales", 0, 12, 4),
             checkboxInput("p_precoce", "CPN au 1er trimestre", TRUE),
             sliderInput("p_imc", "IMC pré-gestationnel", 15, 40, 23.5, step = 0.5),
             sliderInput("p_age", "Âge maternel", 15, 45, 27),
             sliderInput("p_parite", "Parité", 0, 8, 1),
             radioButtons("p_sexe", "Sexe", c("F", "M"), inline = TRUE)
           ),
           uiOutput("prediction_q1")
         ))
  ),
  
  # --- Onglet 4 : Q2 -----------------------------------------
  nav_panel(
    "Q2 — Retard vaccinal",
    icon = icon("syringe"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Guide de lecture — Onglet Q2",
        icon = icon("circle-info"),
        markdown("
**Question posée** : quels facteurs expliquent qu'un enfant accumule du
retard sur son calendrier vaccinal ?

**Pourquoi une échelle logarithmique ?** Le retard cumulé (en jours) est
très asymétrique : beaucoup d'enfants sont à jour ou presque, quelques-uns
ont un retard important. Le modèle est donc ajusté sur le logarithme du
retard, ce qui a pour effet de rendre les coefficients **multiplicatifs**
plutôt qu'additifs.

**Comment lire le graphique « Effets multiplicatifs »** : l'axe horizontal
est un **ratio**. Un ratio de 1 (ligne pointillée) signifie aucun effet ;
un ratio de 0,90 signifie une réduction de 10 % du retard attendu ; un
ratio de 1,20 signifie une augmentation de 20 %. Le rouge signale un effet
statistiquement significatif.

Le tableau à droite convertit directement ces ratios en **pourcentages**
pour en faciliter la lecture.
        ")
      )
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Effets multiplicatifs"),
           plotOutput("coefs_q2", height = "440px")),
      card(
        card_header("Lecture"),
        markdown("
Le modèle est ajusté sur **log(retard + 1)**. Les coefficients s'y
interprètent de façon **multiplicative** : un coefficient de −0,11
correspond à `exp(−0,11) = 0,90`, soit une réduction d'environ **10 %**
par consultation supplémentaire.

L'effet est donc **proportionnel** et non absolu. Pour une famille dont
le retard attendu est de 100 jours, une consultation en retranche environ
10 ; pour une famille à 20 jours, environ 2.

La transformation logarithmique n'est pas cosmétique : sur l'échelle
brute, le modèle produit des prédictions négatives — impossibles pour un
retard exprimé en jours — et ses résidus présentent une forme en entonnoir
caractéristique d'une variance non constante.
        ")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Effets convertis en pourcentage"), DTOutput("table_q2")),
      card(card_header("Distribution : brute et logarithmique"),
           plotOutput("distrib_q2", height = "360px"))
    )
  ),
  
  # --- Onglet 5 : Q3 -----------------------------------------
  nav_panel(
    "Q3 — Continuité",
    icon = icon("link"),
    accordion(
      open = FALSE,
      accordion_panel(
        "Guide de lecture — Onglet Q3",
        icon = icon("circle-info"),
        markdown("
**Question posée** : une mère bien suivie pendant sa grossesse a-t-elle un
enfant mieux vacciné — même après avoir tenu compte d'autres facteurs qui
influencent l'accès aux soins ?

**Graphique de gauche** : chaque barre est le nombre moyen de doses reçues
à 12 mois pour un niveau de suivi prénatal donné, avec son intervalle de
confiance à 95 % (trait noir) et l'effectif du groupe (« n = »). Une
tendance croissante suggère une association entre les deux comportements.

**Graphique de droite** : montre comment le coefficient associé aux
consultations prénatales évolue à mesure qu'on ajoute des facteurs de
confusion potentiels (distance, instruction, milieu) au modèle. Si le
coefficient **diminue** fortement en ajustant, cela indique qu'une partie
de l'association brute s'explique par ces autres facteurs plutôt que par
un lien direct entre suivi prénatal et vaccination.

**Point essentiel** : une association, même après ajustement, ne prouve
pas une relation de cause à effet (voir « Interprétation et limites »
ci-dessous).
        ")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Observance selon l'intensité du suivi prénatal"),
           plotOutput("barres_q3", height = "400px")),
      card(card_header("Effet de l'ajustement progressif"),
           plotOutput("ajustement_q3", height = "400px"))
    ),
    card(
      card_header("Interprétation et limites"),
      markdown("
Le nombre de consultations prénatales est **associé** à une meilleure
observance vaccinale à 12 mois, y compris après ajustement sur la distance
au centre de santé, le niveau d'instruction maternel et le milieu de
résidence.

Le coefficient diminue nettement à mesure que ces facteurs sont introduits :
une part importante de l'association brute était attribuable à des
déterminants d'accès agissant simultanément sur les deux comportements.

**Ce que cette analyse ne permet pas de conclure**

Le devis est observationnel. Des caractéristiques maternelles non mesurées —
motivation, confiance dans le système de santé, autonomie décisionnelle au
sein du foyer — peuvent influencer à la fois le suivi prénatal et la
vaccination. Cette confusion résiduelle ne peut être ni quantifiée ni exclue.

Seul un devis expérimental, assignant au hasard un suivi prénatal renforcé,
permettrait d'établir un lien causal.
      ")
    )
  ),
  
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Données simulées"))
)


# =============================================================
# SERVEUR
# =============================================================

server <- function(input, output, session) {
  
  # --- Donnée réactive : recalculée quand les filtres changent
  filtre <- reactive({
    propre |>
      filter(district %in% input$districts,
             milieu %in% input$milieux,
             is.na(age_mere) | (age_mere >= input$age[1] & age_mere <= input$age[2]))
  })
  
  output$titre_table <- renderText({
    paste0("Données filtrées — ", nrow(filtre()), " observations")
  })
  
  output$nuage <- renderPlot({
    d <- filtre()
    validate(need(nrow(d) > 5, "Trop peu d'observations pour ce filtre."))
    
    p <- ggplot(d, aes(x = .data[[input$var_x]], y = .data[[input$var_y]]))
    
    if (isTRUE(input$couleur)) {
      p <- p + geom_jitter(aes(colour = milieu), width = 0.2, alpha = 0.6, size = 2) +
        scale_colour_manual(values = c(Rural = "#C0703A", Urbain = "#2C6E9B"))
    } else {
      p <- p + geom_jitter(width = 0.2, alpha = 0.6, size = 2)
    }
    
    p + geom_smooth(method = "lm", se = TRUE, colour = "grey25") +
      labs(x = names(which(c(
        "Consultations prénatales" = "cpn_total",
        "Distance au centre" = "distance_cs",
        "Terme (SA)" = "terme_sa",
        "IMC pré-gestationnel" = "imc_pre",
        "Âge maternel" = "age_mere") == input$var_x)),
        y = NULL, colour = NULL) +
      theme(legend.position = "bottom")
  })
  
  output$distrib <- renderPlot({
    d <- filtre()
    validate(need(nrow(d) > 5, "Trop peu d'observations."))
    
    ggplot(d, aes(x = .data[[input$var_y]])) +
      geom_histogram(bins = 25, fill = "#2C6E9B", colour = "white") +
      labs(x = NULL, y = "effectif")
  })
  
  output$table <- renderDT({
    filtre() |>
      select(id, district, milieu, age_mere, instruction, cpn_total,
             terme_sa, poids_naissance, doses_12m, retard_cumule) |>
      datatable(options = list(pageLength = 8, scrollX = TRUE),
                rownames = FALSE)
  })
  
  # --- Q1
  output$coefs_q1 <- renderPlot({
    graphe_coefs(m_q1, "Déterminants du poids de naissance",
                 "Coefficients ajustés et IC 95 %", "effet (grammes)")
  })
  
  output$prediction_q1 <- renderUI({
    nouveau <- tibble(
      cpn_total   = as.integer(input$p_cpn),
      cpn_precoce = input$p_precoce,
      terme_sa    = input$p_terme,
      imc_pre     = input$p_imc,
      age_mere    = input$p_age,
      parite      = as.integer(input$p_parite),
      sexe_enfant = factor(input$p_sexe, levels = c("F", "M")),
      milieu      = factor("Urbain", levels = c("Rural", "Urbain")),
      instruction = factor("Secondaire",
                           levels = c("Aucune", "Primaire", "Secondaire", "Superieur"))
    )
    
    p <- predict(m_q1, newdata = nouveau, interval = "prediction")
    
    div(
      style = "text-align:center; padding: 2rem 0;",
      h5("Poids de naissance prédit", class = "text-muted"),
      h1(paste0(round(p[1]), " g"), style = "color:#2C6E9B;"),
      p(sprintf("Intervalle de prédiction à 95 %% : %.0f à %.0f g",
                p[2], p[3]), class = "text-muted"),
      if (p[1] < 2500) div(class = "alert alert-danger d-inline-block",
                           "Sous le seuil OMS de faible poids (2500 g)")
    )
  })
  
  # --- Q2
  output$coefs_q2 <- renderPlot({
    tidy(m_q2, conf.int = TRUE) |>
      filter(term != "(Intercept)") |>
      mutate(across(c(estimate, conf.low, conf.high), exp),
             label = coalesce(jolis_noms[term], term),
             label = fct_reorder(label, estimate),
             signif = if_else(p.value < 0.05, "p < 0,05", "p ≥ 0,05")) |>
      ggplot(aes(x = estimate, y = label, colour = signif)) +
      geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
      geom_pointrange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6) +
      scale_x_log10() +
      scale_colour_manual(values = c("p < 0,05" = "#B5533C", "p ≥ 0,05" = "grey60")) +
      labs(title = "Déterminants du retard vaccinal",
           subtitle = "Ratios multiplicatifs — 1 = aucun effet",
           x = "ratio de retard (échelle log)", y = NULL, colour = NULL) +
      theme(legend.position = "bottom")
  })
  
  output$table_q2 <- renderDT({
    tidy(m_q2, conf.int = TRUE) |>
      filter(term != "(Intercept)") |>
      transmute(
        Variable = coalesce(jolis_noms[term], term),
        `Ratio` = round(exp(estimate), 3),
        `Variation (%)` = round(100 * (exp(estimate) - 1), 1),
        `IC bas (%)` = round(100 * (exp(conf.low) - 1), 1),
        `IC haut (%)` = round(100 * (exp(conf.high) - 1), 1),
        `p` = signif(p.value, 3)
      ) |>
      datatable(options = list(pageLength = 12, dom = "t"), rownames = FALSE)
  })
  
  output$distrib_q2 <- renderPlot({
    a <- ggplot(propre, aes(x = retard_cumule)) +
      geom_histogram(bins = 30, fill = "#B5533C", colour = "white") +
      labs(title = "Échelle brute", subtitle = "Forte asymétrie à droite",
           x = "jours", y = NULL)
    b <- ggplot(propre, aes(x = log_retard)) +
      geom_histogram(bins = 30, fill = "#4C8B6F", colour = "white") +
      labs(title = "Échelle logarithmique", subtitle = "Distribution symétrisée",
           x = "log(jours + 1)", y = NULL)
    patchwork::wrap_plots(a, b, ncol = 2)
  })
  
  # --- Q3
  output$barres_q3 <- renderPlot({
    propre |>
      mutate(groupe = cut(cpn_total, c(-1, 1, 3, 5, 20),
                          labels = c("0-1", "2-3", "4-5", "6+"))) |>
      filter(!is.na(groupe)) |>
      group_by(groupe) |>
      summarise(m = mean(doses_12m), e = sd(doses_12m) / sqrt(n()),
                n = n(), .groups = "drop") |>
      ggplot(aes(x = groupe, y = m)) +
      geom_col(fill = "#6B5B95", width = 0.6) +
      geom_errorbar(aes(ymin = m - 1.96 * e, ymax = m + 1.96 * e), width = 0.15) +
      geom_text(aes(label = paste0("n=", n)), y = 0.4, colour = "white", size = 3.5) +
      labs(subtitle = "Moyenne des doses reçues et IC 95 %",
           x = "consultations prénatales", y = "doses à 12 mois")
  })
  
  output$ajustement_q3 <- renderPlot({
    formules <- list(
      "Non ajusté"      = doses_12m ~ cpn_total,
      "+ distance"      = doses_12m ~ cpn_total + distance_cs,
      "+ instruction"   = doses_12m ~ cpn_total + distance_cs + instruction,
      "+ milieu"        = doses_12m ~ cpn_total + distance_cs + instruction + milieu
    )
    
    map_dfr(names(formules), \(nom) {
      tidy(lm(formules[[nom]], data = propre), conf.int = TRUE) |>
        filter(term == "cpn_total") |>
        mutate(modele = nom)
    }) |>
      mutate(modele = fct_inorder(modele)) |>
      ggplot(aes(x = modele, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                      colour = "#6B5B95", linewidth = 0.7) +
      labs(subtitle = "Coefficient des CPN à mesure des ajustements",
           x = NULL, y = "effet sur les doses reçues") +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })
}

shinyApp(ui, server)