############################################################
#   HURDLE MODELS – RESULTS, FIGURES + TABLES
############################################################

# ================================
# Load packages
# ================================
library(here)
library(glmmTMB)
library(MASS)
library(dplyr)
library(purrr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(tidyr)
library(broom.mixed)
library(flextable)
library(officer)
library(cowplot)

# ================================
# Data preparation
# ================================
salmon.diet <- combined_dietM %>%
  dplyr::select(SampleID, Year, Season, Estuary, block, Complex,
                Chinook, Chum, Coho, Pink, Sockeye) %>%
  mutate(
    chinook = Chinook/100,
    chum = Chum/100,
    coho = Coho/100,
    pink = Pink/100,
    sockeye = Sockeye/100
  )

epsilon <- 1e-4
species_cols <- c("chinook", "chum", "coho", "pink", "sockeye")

salmon.dietm <- salmon.diet %>%
  filter(Estuary!="Belle Chain" | Season!="Spring" | Year!=2013) %>%
  filter(Estuary!="Belle Chain" | Season!="Spring" | Year!=2018)

salmon.dietp <- salmon.dietm %>%
  mutate(across(all_of(species_cols),
                ~ as.integer(. > 0),
                .names = "{.col}_present"))

salmon.diet_nz <- salmon.dietm %>%
  filter(if_any(all_of(species_cols), ~ . > 0)) %>%
  mutate(across(all_of(species_cols), ~ ifelse(. >= 1, 1 - epsilon, .)))

desired_levels <- c("Belle Chain", "Estuary", "Non-Estuary")

salmon.dietp <- salmon.dietp %>%
  mutate(
    Estuary = factor(Estuary, levels = desired_levels),
    Season = factor(Season, levels = c("Spring", "Summer", "Fall")),
    block = factor(block, levels = c("pre-2015", "post-2015"))
  )

salmon.diet_nz <- salmon.diet_nz %>%
  mutate(
    Estuary = factor(Estuary, levels = desired_levels),
    Season = factor(Season, levels = c("Spring", "Summer", "Fall")),
    block = factor(block, levels = c("pre-2015", "post-2015"))
  )

# ================================
# Fit presence (binomial) models
# ================================
model_chinook_presence <- glmmTMB(chinook_present ~ Season + Estuary + block +
                                    (1 | Estuary/Complex) + (1 | Year),
                                  family = binomial, data = salmon.dietp)

model_chum_presence <- glmmTMB(chum_present ~ Season + Estuary + block +
                                 (1 | Estuary/Complex) + (1 | Year),
                               family = binomial, data = salmon.dietp)

model_coho_presence <- glmmTMB(coho_present ~ Season + Estuary + block +
                                 (1 | Estuary/Complex) + (1 | Year),
                               family = binomial, data = salmon.dietp)

model_pink_presence <- glmmTMB(pink_present ~ Season + Estuary + block +
                                 (1 | Estuary/Complex) + (1 | Year),
                               family = binomial, data = salmon.dietp)

model_sockeye_presence <- glmmTMB(sockeye_present ~ Season + Estuary + block +
                                    (1 | Estuary/Complex) + (1 | Year),
                                  family = binomial, data = salmon.dietp)

# ================================
# Fit proportion (beta) models
# ================================
model_chinook_proportion <- glmmTMB(chinook ~ Season + Estuary + block +
                                      (1 | Estuary/Complex) + (1 | Year),
                                    family = beta_family(link="logit"),
                                    data = salmon.diet_nz %>% filter(chinook > 0 & chinook < 1))

model_chum_proportion <- glmmTMB(chum ~ Season + Estuary + block +
                                   (1 | Estuary/Complex) + (1 | Year),
                                 family = beta_family(link="logit"),
                                 data = salmon.diet_nz %>% filter(chum > 0 & chum < 1))

model_coho_proportion <- glmmTMB(coho ~ Season + Estuary + block +
                                   (1 | Estuary/Complex) + (1 | Year),
                                 family = beta_family(link="logit"),
                                 data = salmon.diet_nz %>% filter(coho > 0 & coho < 1))

model_pink_proportion <- glmmTMB(pink ~ Season + Estuary + block +
                                   (1 | Estuary/Complex) + (1 | Year),
                                 family = beta_family(link="logit"),
                                 data = salmon.diet_nz %>% filter(pink > 0 & pink < 1))

model_sockeye_proportion <- glmmTMB(sockeye ~ Season + Estuary + block +
                                      (1 | Estuary/Complex) + (1 | Year),
                                    family = beta_family(link="logit"),
                                    data = salmon.diet_nz %>% filter(sockeye > 0 & sockeye < 1))

# ================================
# Combine models
# ================================
presence_models <- list(
  chinook = model_chinook_presence,
  chum = model_chum_presence,
  coho = model_coho_presence,
  pink = model_pink_presence,
  sockeye = model_sockeye_presence
)

proportion_models <- list(
  chinook = model_chinook_proportion,
  chum = model_chum_proportion,
  coho = model_coho_proportion,
  pink = model_pink_proportion,
  sockeye = model_sockeye_proportion
)

# ================================
# Prediction grid
# ================================
predicted_data <- expand.grid(
  Season = c("Spring", "Summer", "Fall"),
  Estuary = desired_levels,
  block = c("pre-2015", "post-2015")
) %>%
  mutate(
    Estuary = factor(Estuary, levels = desired_levels),
    Season = factor(Season, levels = c("Spring", "Summer", "Fall")),
    block = factor(block, levels = c("pre-2015", "post-2015"))
  ) %>% # Optional filter to remove combinations that were never observed
filter(!(Estuary == "Non-Estuary" & block == "pre-2015"))

# ================================
# Simulation function
# ================================
simulate_species <- function(species, nsim = 10000) {
  pres_model <- presence_models[[species]]
  prop_model <- proportion_models[[species]]
  
  mat_pres <- model.matrix(delete.response(terms(pres_model)), predicted_data)
  mat_prop <- model.matrix(delete.response(terms(prop_model)), predicted_data)
  
  sims <- map(1:nsim, ~ {
    cp <- MASS::mvrnorm(1, fixef(pres_model)$cond, vcov(pres_model)$cond)
    p_pres <- plogis(as.vector(mat_pres %*% cp))
    
    cp2 <- MASS::mvrnorm(1, fixef(prop_model)$cond, vcov(prop_model)$cond)
    p_prop <- plogis(as.vector(mat_prop %*% cp2))
    
    tibble(predicted_data, pres = p_pres, prop = p_prop,
           expected = p_pres * p_prop)
  })
  
  sims_df <- bind_rows(sims, .id = "sim_id")
  
  sims_df %>%
    group_by(Season, Estuary, block) %>%
    summarise(
      pres_mean = mean(pres),
      pres_lower = quantile(pres, .025),
      pres_upper = quantile(pres, .975),
      prop_mean = mean(prop),
      prop_lower = quantile(prop, .025),
      prop_upper = quantile(prop, .975),
      expected_mean = mean(expected),
      expected_lower = quantile(expected, .025),
      expected_upper = quantile(expected, .975),
      .groups = "drop"
    ) %>%
    mutate(species = species)
}

all_summary <- map_df(names(presence_models), simulate_species)

 # ================================
# Tables 
# ================================
all_models <- c(presence_models, proportion_models)
model_results_clean <- imap_dfr(all_models, ~ tidy(.x, effects="fixed") %>% mutate(model=.y)) %>%
  mutate(across(where(is.numeric), ~ round(.,3)))

ft_fixed <- flextable(model_results_clean) %>% autofit()
doc <- read_docx()
doc <- body_add_par(doc, "Fixed Effects", style = "heading 1")
doc <- body_add_flextable(doc, ft_fixed)
print(doc, target="Tab4a_Fixed_Effects.docx")

extract_random_effects <- function(model, name) {
  vc <- VarCorr(model)$cond
  map_dfr(names(vc), function(g) {
    v <- vc[[g]]
    tibble(
      Model = name,
      Grouping = g,
      Term = rownames(v),
      Variance = diag(v),
      Std_Dev = sqrt(diag(v))
    )
  })
}


random_effects_results <- imap_dfr(all_models, extract_random_effects) %>%
  mutate(across(c(Variance, Std_Dev), round, 3))
ft_random <- flextable(random_effects_results) %>% autofit()
doc2 <- read_docx()
doc2 <- body_add_par(doc2, "Random Effects Variance Components", style="heading 1")
doc2 <- body_add_flextable(doc2, ft_random)
print(doc2, target="Tab4b_Random_Effects.docx")


############################################################
#                     FIGURES 
############################################################

library(gridExtra)

############################################################
# FIGURE 5 — Expected dietary contribution (ALL SPECIES)
############################################################

# Create per–species expected-diet plots
plot_list <- map(unique(all_summary$species), function(sp) {
  df <- all_summary %>% filter(species == sp)
  
  ggplot(df, aes(x = Estuary, y = expected_mean * 100, color = block)) +
    geom_point(position = position_dodge(0.3), size = 2.5) +
    geom_errorbar(aes(ymin = expected_lower * 100,
                      ymax = expected_upper * 100),
                  width = 0.3,
                  position = position_dodge(0.3)) +
    facet_wrap(~ Season) +
    theme_bw() +
    labs(title = sp,
         y = "Expected diet (%)")
})

# Arrange and save 
ggsave("Fig5_expected_proportions_all_species.png",
       grid.arrange(grobs = plot_list, ncol = 2),
       width = 12,
       height = 2 * length(plot_list))


############################################################
# FIGURE S1 — Presence + Proportion  (ALL SPECIES)
############################################################

# 3-panel layout per species
plot_s1_list <- map(unique(all_summary$species), function(sp) {
  
  df <- all_summary %>% filter(species == sp)
  
  # Panels 
  p1 <- ggplot(df, aes(Estuary, pres_mean * 100, colour = block)) +
    geom_point(position = position_dodge(0.4), size = 3) +
    geom_errorbar(aes(ymin = pres_lower * 100, ymax = pres_upper * 100),
                  position = position_dodge(0.4), width = 0.2) +
    facet_wrap(~ Season) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12)) +
    labs(title = paste("Presence Probability -", sp), y="Presence (%)")
  
  p2 <- ggplot(df, aes(Estuary, prop_mean * 100, colour = block)) +
    geom_point(position = position_dodge(0.4), size = 3) +
    geom_errorbar(aes(ymin = prop_lower * 100, ymax = prop_upper * 100),
                  position = position_dodge(0.4), width = 0.2) +
    facet_wrap(~ Season) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12)) +
    labs(title = paste("Proportion (when present) -", sp), y="Proportion (%)")
  
  # 3 panels, horizontal layout
  gridExtra::arrangeGrob(p1, p2, ncol = 2)
})

# Stack species vertically 
final_s1 <- do.call(gridExtra::arrangeGrob,
                    c(plot_s1_list, ncol = 1))

# Save final figure
ggsave("FigS1_all_species_presence_proportion.png",
       final_s1,
       width = 12,            
       height = 3 * length(plot_s1_list))   # ≈ 3 vertical units per species
