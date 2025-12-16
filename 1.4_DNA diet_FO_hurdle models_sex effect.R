############################################################
#         HURDLE MODELS — SEX EFFECTS ANALYSIS
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
library(ggpubr)
library(tidyverse)
library(tidyr)
library(broom.mixed)
library(flextable)
library(officer)
library(cowplot)


############################################################
#                 DATA PREPARATION
############################################################

# ================================
# Load & scale diet data
# ================================
diet.sex <- read_csv(here("csv","salmon_seal diet_sex subsample.csv"))

###sample sizes
library(pivottabler)
pt <- PivotTable$new()
pt$addData(diet.sex)
pt$addColumnDataGroups("sex")
pt$addRowDataGroups("Season")
pt$addRowDataGroups("Estuary", addTotal=FALSE)                
pt$defineCalculation(calculationName="Total", summariseExpression="n()")
pt$renderPivot()
pt$evaluatePivot()

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data", 
                         topRowNumber=1, leftMostColumnNumber=1, 
                         applyStyles=TRUE, mapStylesFromCSS=TRUE)
saveWorkbook(wb, file="Tab5_sampleXsex pivot.xlsx", overwrite = TRUE)

# Format and scale diet data
salmon.diet.sex <- diet.sex %>%
  dplyr::select(SampleID, Season, Estuary, sex,
                Chinook, Chum, Coho, Pink, Sockeye) %>%
  mutate(
    chinook = Chinook / 100,
    chum    = Chum / 100,
    coho    = Coho / 100,
    pink    = Pink / 100,
    sockeye = Sockeye / 100
  )


# Set factor levels
salmon.diet.sex$Season <- factor(salmon.diet.sex$Season, levels = c("Spring", "Summer", "Fall"))
salmon.diet.sex$Estuary <- factor(salmon.diet.sex$Estuary, levels = c("Belle Chain","Estuary", "Non-Estuary"))
salmon.diet.sex$sex <- factor(salmon.diet.sex$sex, levels = c("FEMALE", "MALE"))

# ─────────────────────────────────────────────
# Calculate Frequency of Occurrence
# ─────────────────────────────────────────────

# Frequency of occurrence by Estuary, Season, and Sex
fo <- salmon.diet.sex %>%
  group_by(Estuary, Season, sex) %>%
  summarise(
    n = n(),
    chinook = sum(Chinook > 0, na.rm = TRUE),
    chum = sum(Chum > 0, na.rm = TRUE),
    coho = sum(Coho > 0, na.rm = TRUE),
    pink = sum(Pink > 0, na.rm = TRUE),
    sockeye = sum(Sockeye > 0, na.rm = TRUE)
  ) %>%
  mutate(
    freq_CK = chinook / n * 100,
    freq_CM = chum / n * 100,
    freq_CO = coho / n * 100,
    freq_PK = pink / n * 100,
    freq_SE = sockeye / n * 100
  ) %>%
  dplyr::select(Estuary, Season, sex, n, freq_CK, freq_CM, freq_CO, freq_PK, freq_SE) %>%
  rename(
    Chinook = freq_CK,
    Chum = freq_CM,
    Coho = freq_CO,
    Pink = freq_PK,
    Sockeye = freq_SE
  )

# Total sample size per estuary and season
fo2 <- fo %>%
  group_by(Estuary, Season) %>%
  summarise(TotalRun = sum(n, na.rm = TRUE)) %>%
  ungroup()

# Combine
fof <- left_join(fo, fo2, by = c("Estuary", "Season")) %>%
  relocate(TotalRun, .before = Chinook)

# ─────────────────────────────────────────────
# Plot Function: Frequency of Occurrence
# ─────────────────────────────────────────────

generate_species_plot <- function(fof, species_name, species_label) {
  
  # Convert to long format
  fo_long <- fof %>%
    pivot_longer(cols = c(Chinook, Chum, Coho, Pink, Sockeye),
                 names_to = "prey", values_to = "prev") %>%
    filter(prey == species_name)
  
  # Create bar plot
  sp <- ggplot(fo_long, aes(y = prev, x = prey, fill = sex)) + 
    geom_bar(position = "dodge", stat = "identity") +
    scale_y_continuous(limits = c(0, 60)) +
    xlab("") + 
    ylab("Occurrence (%)") +
    geom_text(aes(x = 0.6, y = 57, label = TotalRun), size = 4, inherit.aes = FALSE) +
    geom_text(aes(label = n), color = "grey20", size = 3,
              hjust = 0.5, vjust = -0.5,
              position = position_dodge(width = 1)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.title = element_text(face = "bold"),
          strip.text.y = element_text(angle = -90)) +
    facet_grid(Estuary ~ Season) +
    ggtitle(species_label)
  
  # Save plot
  ggsave(filename = paste0("summary_", species_name, "_freq_by_Estuary_Season_Sex.png"),
         plot = sp, width = 9, height = 9.0)
  
  return(sp)
}

# ─────────────────────────────────────────────
# Generate and Save Plots
# ─────────────────────────────────────────────

summary_chinook <- generate_species_plot(fof, "Chinook", "Chinook")+ theme(legend.position = "none")
summary_chum    <- generate_species_plot(fof, "Chum",    "Chum")
summary_coho    <- generate_species_plot(fof, "Coho",    "Coho")+ theme(legend.position = "none")
summary_pink    <- generate_species_plot(fof, "Pink",    "Pink")
summary_sockeye <- generate_species_plot(fof, "Sockeye", "Sockeye")+ theme(legend.position = "none")

# ─────────────────────────────────────────────
# Combine All into One Summary Figure
# ─────────────────────────────────────────────

summary_salmon_sex <- ggarrange(
  summary_chinook,
  summary_chum,
  summary_coho,
  summary_pink,
  summary_sockeye,
  ncol = 2, nrow = 3
)

ggsave(filename = "Fig6a_summary_ALLsalmon_freq_by_Estuary_Season_Sex.png",
       plot = summary_salmon_sex, 
       width = 10, height = 14)

# ================================
# Presence indicators + non-zero data
# ================================
epsilon <- 1e-4
species_cols <- c("chinook","chum","coho","pink","sockeye")

salmon.dietp_sex <- salmon.diet.sex %>%
  mutate(
    pink_present    = as.integer(pink > 0),
    chinook_present = as.integer(chinook > 0),
    coho_present    = as.integer(coho > 0),
    chum_present    = as.integer(chum > 0),
    sockeye_present = as.integer(sockeye > 0)
  )

# Observed combinations for prediction filtering
observed_combos <- salmon.dietp_sex %>%
  distinct(Season, Estuary, sex)

# Non-zero abundance (for beta models)
salmon.diet_nz_sex <- salmon.diet.sex %>%
  filter(if_any(all_of(species_cols), ~ . > 0)) %>%
  mutate(across(all_of(species_cols),
                ~ ifelse(. >= 1, 1 - epsilon, .)))

# ================================
# Set factor levels consistently
# ================================
factor_levels <- list(
  Season  = c("Spring", "Summer", "Fall"),
  Estuary = c("Belle Chain", "Estuary", "Non-Estuary"),
  sex     = c("FEMALE","MALE")
)

salmon.dietp_sex <- salmon.dietp_sex %>%
  mutate(
    Season  = factor(Season, levels = factor_levels$Season),
    Estuary = factor(Estuary, levels = factor_levels$Estuary),
    sex     = factor(sex,    levels = factor_levels$sex)
  )

salmon.diet_nz_sex <- salmon.diet_nz_sex %>%
  mutate(
    Season  = factor(Season, levels = factor_levels$Season),
    Estuary = factor(Estuary, levels = factor_levels$Estuary),
    sex     = factor(sex,    levels = factor_levels$sex)
  )


############################################################
#           FIT MODELS — PRESENCE (BINOMIAL)
############################################################

model_chinook_presence_sex <- glmmTMB(chinook_present ~ Season + Estuary + sex,
                                      family = binomial, data = salmon.dietp_sex)

model_chum_presence_sex <- glmmTMB(chum_present ~ Season + Estuary + sex,
                                   family = binomial, data = salmon.dietp_sex)

model_coho_presence_sex <- glmmTMB(coho_present ~ Season + Estuary + sex,
                                   family = binomial, data = salmon.dietp_sex)

model_pink_presence_sex <- glmmTMB(pink_present ~ Season + Estuary + sex,
                                   family = binomial, data = salmon.dietp_sex)

model_sockeye_presence_sex <- glmmTMB(sockeye_present ~ Season + Estuary + sex,
                                      family = binomial, data = salmon.dietp_sex)


############################################################
#         FIT MODELS — PROPORTION (BETA FAMILY)
############################################################

model_chinook_proportion_sex <- glmmTMB(
  chinook ~ Season + Estuary + sex,
  family = beta_family(link="logit"),
  data = salmon.diet_nz_sex %>% filter(chinook > 0 & chinook < 1)
)

model_chum_proportion_sex <- glmmTMB(
  chum ~ Season + Estuary + sex,
  family = beta_family(link="logit"),
  data = salmon.diet_nz_sex %>% filter(chum > 0 & chum < 1)
)

model_coho_proportion_sex <- glmmTMB(
  coho ~ Season + Estuary + sex,
  family = beta_family(link="logit"),
  data = salmon.diet_nz_sex %>% filter(coho > 0 & coho < 1)
)

model_pink_proportion_sex <- glmmTMB(
  pink ~ Season + Estuary + sex,
  family = beta_family(link="logit"),
  data = salmon.diet_nz_sex %>% filter(pink > 0 & pink < 1)
)

model_sockeye_proportion_sex <- glmmTMB(
  sockeye ~ Season + Estuary + sex,
  family = beta_family(link="logit"),
  data = salmon.diet_nz_sex %>% filter(sockeye > 0 & sockeye < 1)
)


############################################################
#                STORE MODELS IN LISTS
############################################################

presence_models_sex <- list(
  chinook = model_chinook_presence_sex,
  chum    = model_chum_presence_sex,
  coho    = model_coho_presence_sex,
  pink    = model_pink_presence_sex,
  sockeye = model_sockeye_presence_sex
)

proportion_models_sex <- list(
  chinook = model_chinook_proportion_sex,
  chum    = model_chum_proportion_sex,
  coho    = model_coho_proportion_sex,
  pink    = model_pink_proportion_sex,
  sockeye = model_sockeye_proportion_sex
)


############################################################
#               CREATE PREDICTION GRID
############################################################

predicted_data_full <- expand.grid(
  Season  = c("Spring","Summer","Fall"),
  Estuary = c("Belle Chain","Estuary","Non-Estuary"),
  sex     = c("FEMALE","MALE")
) %>%
  mutate(
    Season  = factor(Season,  levels = factor_levels$Season),
    Estuary = factor(Estuary, levels = factor_levels$Estuary),
    sex     = factor(sex,     levels = factor_levels$sex)
  )

# Filter to observed combinations only
predicted_data <- predicted_data_full %>%
  semi_join(observed_combos, by = c("Season","Estuary","sex"))


############################################################
#                   SIMULATION FUNCTION
############################################################

simulate_species <- function(species, nsim = 10000) {
  
  pres_model <- presence_models_sex[[species]]
  prop_model <- proportion_models_sex[[species]]
  
  mat_pres <- model.matrix(delete.response(terms(pres_model)), predicted_data)
  mat_prop <- model.matrix(delete.response(terms(prop_model)), predicted_data)
  
  sims <- map(1:nsim, ~ {
    cp <- MASS::mvrnorm(1, fixef(pres_model)$cond, vcov(pres_model)$cond)
    cp2 <- MASS::mvrnorm(1, fixef(prop_model)$cond, vcov(prop_model)$cond)
    
    tibble(
      predicted_data,
      pres     = plogis(mat_pres %*% cp),
      prop     = plogis(mat_prop %*% cp2),
      expected = plogis(mat_pres %*% cp) * plogis(mat_prop %*% cp2)
    )
  })
  
  bind_rows(sims, .id="sim_id") %>%
    group_by(Season, Estuary, sex) %>%
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
      .groups="drop"
    ) %>%
    mutate(species = species)
}

# Run simulation across species
all_summary_sex <- map_df(names(presence_models_sex), simulate_species)


############################################################
#                       PLOTTING
############################################################

# ================================
# Helper plot function
# ================================
plot_three_metrics <- function(data, species_name) {
  
  p1 <- ggplot(data, aes(Estuary, pres_mean*100, colour=sex)) +
    geom_point(position=position_dodge(.4), size=3) +
    geom_errorbar(aes(ymin=pres_lower*100, ymax=pres_upper*100),
                  position=position_dodge(.4), width=.2) +
    facet_wrap(~Season) +
    labs(title = paste("Presence Probability -", species_name),
         y="Presence (%)", x="Estuary") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  p2 <- ggplot(data, aes(Estuary, prop_mean*100, colour=sex)) +
    geom_point(position=position_dodge(.4), size=3) +
    geom_errorbar(aes(ymin=prop_lower*100, ymax=prop_upper*100),
                  position=position_dodge(.4), width=.2) +
    facet_wrap(~Season) +
    labs(title = paste("Proportion (when present) -", species_name),
         y="Proportion (%)", x="Estuary") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  p3 <- ggplot(data, aes(Estuary, expected_mean*100, colour=sex)) +
    geom_point(position=position_dodge(.4), size=3) +
    geom_errorbar(aes(ymin=expected_lower*100, ymax=expected_upper*100),
                  position=position_dodge(.4), width=.2) +
    facet_wrap(~Season) +
    labs(title = paste("Expected Diet Proportion -", species_name),
         y="Expected (%)", x="Estuary") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
  
  list(presence=p1, proportion=p2, expected=p3)
}

# Generate for all species
plots_sex <- map(names(presence_models_sex), function(sp) {
  plot_three_metrics(all_summary_sex %>% filter(species==sp), sp)
})


############################################################
#                 SUMMARY TABLES + EXPORT
############################################################

summary_table_sex <- all_summary_sex %>%
  mutate(
    Presence   = sprintf("%.1f (%.1f–%.1f)", pres_mean*100, pres_lower*100, pres_upper*100),
    Proportion = sprintf("%.1f (%.1f–%.1f)", prop_mean*100, prop_lower*100, prop_upper*100),
    Expected   = sprintf("%.1f (%.1f–%.1f)", expected_mean*100, expected_lower*100, expected_upper*100)
  ) %>%
  dplyr::select(Species=species, Season, Estuary, sex,
         Presence, Proportion, Expected) %>%
  arrange(Species, Season, Estuary, sex)


############################################################
#                FIXED EFFECT TABLE (WORD)
############################################################

all_models_sex <- list(
  chinook_presence     = model_chinook_presence_sex,
  chinook_proportion   = model_chinook_proportion_sex,
  chum_presence        = model_chum_presence_sex,
  chum_proportion      = model_chum_proportion_sex,
  coho_presence        = model_coho_presence_sex,
  coho_proportion      = model_coho_proportion_sex,
  pink_presence        = model_pink_presence_sex,
  pink_proportion      = model_pink_proportion_sex,
  sockeye_presence     = model_sockeye_presence_sex,
  sockeye_proportion   = model_sockeye_proportion_sex
)

model_results_clean <- imap_dfr(all_models_sex, ~
                                  tidy(.x, effects="fixed") %>% mutate(model=.y)
) %>%
  dplyr::select(model, term, estimate, std.error, statistic, p.value) %>%
  rename(
    Model = model,
    Term = term,
    Estimate = estimate,
    `Std. Error` = std.error,
    `z value` = statistic,
    `p value` = p.value
  ) %>%
  mutate(across(c(Estimate, `Std. Error`, `z value`, `p value`), round, 3))

flextable(model_results_clean) %>% autofit()


############################################################
#          FIGURE — EXPECTED DIET (ALL SPECIES)
############################################################

plot_species <- function(df, species_label) {
  ggplot(df, aes(Estuary, expected_mean*100, color=sex)) +
    geom_point(position=position_dodge(.3), size=2.5) +
    geom_errorbar(aes(ymin=expected_lower*100, ymax=expected_upper*100),
                  width=.3, position=position_dodge(.3)) +
    facet_wrap(~Season) +
    labs(title = paste0(toupper(substr(species_label,1,1)),
                        substr(species_label,2,nchar(species_label))),
         y="Expected Proportion (%)", x="Estuary") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=45, hjust=1))
}

plot_list <- map(unique(all_summary_sex$species), function(sp) {
  plot_species(all_summary_sex %>% filter(species==sp), sp)
})

grid.arrange(grobs=plot_list, ncol=2)

ggsave("Fig6b_expected_proportions_all_species_SEX.png",
       grid.arrange(grobs=plot_list, ncol=2),
       width=12, height=2*length(plot_list))


############################################################
#     SUPPLEMENTAL FIGURE — PRESENCE + PROPORTION
############################################################

species_rows <- map(
  names(presence_models_sex),
  function(sp) {
    sp_data <- all_summary_sex %>% filter(species==sp)
    plots <- plot_three_metrics(sp_data, sp)
    
    p1 <- plots$presence + theme(legend.position="none")
    p2 <- plots$proportion
    
    cowplot::plot_grid(p1, p2, ncol=2)
  }
)

full_plot <- cowplot::plot_grid(plotlist=species_rows, ncol=1)

ggsave(
  filename="FigS4_presence_proportion_all_species_SEX.png",
  plot=full_plot,
  width=12,
  height=4 * length(species_rows),
  limitsize=FALSE
)


############################################################
#                     MODEL REPORTS
############################################################

library(report)

# Presence model reports
report(model_chinook_presence_sex)
report(model_chum_presence_sex)
report(model_coho_presence_sex)
report(model_pink_presence_sex)
report(model_sockeye_presence_sex)

# Proportion model reports
report(model_chinook_proportion_sex)
report(model_chum_proportion_sex)
report(model_coho_proportion_sex)
report(model_pink_proportion_sex)
report(model_sockeye_proportion_sex)
