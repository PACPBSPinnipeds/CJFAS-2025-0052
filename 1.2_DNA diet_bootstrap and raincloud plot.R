# ======================================================================
# SETUP & LIBRARIES
# ======================================================================

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")
options(scipen=999)

library(tidyverse)
library(boot)
library(cowplot)
library(ggpubr)
library(ggplot2)
library(dplyr)



# ======================================================================
# FACTOR LEVEL DEFINITIONS & DATA PREPARATION
# ======================================================================

## Set factor levels
combined_dietM$Estuary <- factor(combined_dietM$Estuary,
                                levels = c("Belle Chain", "Estuary", "Non-Estuary"))

combined_dietM$Season <- factor(combined_dietM$Season,
                               levels = c("Spring", "Summer", "Fall"))

combined_dietM$block <- factor(combined_dietM$block,
                              levels = c("pre-2015", "post-2015"))

## Extract data for each species
CK_pointsALL <- combined_dietM %>% dplyr::select(Estuary, Season, block, Chinook)
CO_pointsALL <- combined_dietM %>% dplyr::select(Estuary, Season, block, Coho)
CM_pointsALL <- combined_dietM %>% dplyr::select(Estuary, Season, block, Chum)
PK_pointsALL <- combined_dietM %>% dplyr::select(Estuary, Season, block, Pink)
SE_pointsALL <- combined_dietM %>% dplyr::select(Estuary, Season, block, Sockeye)

## Add small values to avoid empty groups
CK_pointsALLm <- CK_pointsALL %>%
  add_row(Estuary="Belle Chain", Season="Fall", block="pre-2015", Chinook=0.001) %>%
  mutate(Estuary=factor(Estuary, levels=levels(combined_dietM$Estuary)),
         Season=factor(Season, levels=levels(combined_dietM$Season)),
         block=factor(block, levels=levels(combined_dietM$block)))

CM_pointsALLm <- CM_pointsALL %>%
  add_row(Estuary="Belle Chain", Season="Spring", block="post-2015", Chum=0.001) %>%
  mutate(Estuary=factor(Estuary, levels=levels(combined_dietM$Estuary)),
         Season=factor(Season, levels=levels(combined_dietM$Season)),
         block=factor(block, levels=levels(combined_dietM$block)))

PK_pointsALLm <- PK_pointsALL %>%
  add_row(Estuary="Belle Chain", Season="Summer", block="post-2015", Pink=0.001) %>%
  mutate(Estuary=factor(Estuary, levels=levels(combined_dietM$Estuary)),
         Season=factor(Season, levels=levels(combined_dietM$Season)),
         block=factor(block, levels=levels(combined_dietM$block)))

SE_pointsALLm <- SE_pointsALL %>%
  add_row(Estuary="Belle Chain", Season="Spring", block="post-2015", Sockeye=0.001) %>%
  add_row(Estuary="Belle Chain", Season="Spring", block="pre-2015", Sockeye=0.001) %>%
  mutate(Estuary=factor(Estuary, levels=levels(combined_dietM$Estuary)),
         Season=factor(Season, levels=levels(combined_dietM$Season)),
         block=factor(block, levels=levels(combined_dietM$block)))


# ======================================================================
# BOOTSTRAPPING FUNCTIONS
# ======================================================================

## Basic statistic
boot_mean <- function(d, i) mean(d[i])

## Process species 
process_species <- function(species_data, species_name) {
  
  species_data_nest <- species_data %>%
    dplyr::group_by(block, Estuary, Season) %>%
    tidyr::nest()
  
  species_data_nest <- species_data_nest %>%
    dplyr::mutate(booted = purrr::map(.x = data,
                                      ~ boot::boot(data = .x[[species_name]],
                                                   statistic = boot_mean,
                                                   R = 10000,
                                                   stype = "i")))
  
  species_data_nest <- species_data_nest %>%
    dplyr::mutate(booted_ci = purrr::map(.x = booted,
                                         ~ boot::boot.ci(.x, conf = 0.95, type = "basic")))
  
  species_booted <- species_data_nest %>%
    dplyr::mutate(statistic = purrr::map(.x = booted_ci, ~ .x$t0),
                  lower_ci = purrr::map(.x = booted_ci, ~ .x$basic[[4]]),
                  upper_ci = purrr::map(.x = booted_ci, ~ .x$basic[[5]])) %>%
    dplyr::select(-data, -booted, -booted_ci) %>%
    tidyr::unnest(cols = c(statistic, lower_ci, upper_ci))
  
  species_booted %>% add_column(species = species_name)
}


# ======================================================================
# BOOTSTRAP EACH SPECIES
# ======================================================================

chinook_bootedALL_F <- process_species(CK_pointsALLm, "Chinook")
coho_bootedALL_F    <- process_species(CO_pointsALL,  "Coho")
chum_bootedALL_F    <- process_species(CM_pointsALLm, "Chum")
pink_bootedALL_F    <- process_species(PK_pointsALLm, "Pink")
sockeye_bootedALL_F <- process_species(SE_pointsALLm, "Sockeye")


# ======================================================================
# ASSEMBLE METADATA FOR PLOTTING
# ======================================================================

species_list <- list(
  CK = list(data = CK_pointsALLm, boot = chinook_bootedALL_F, ylab = "Chinook (%)"),
  CO = list(data = CO_pointsALL,  boot = coho_bootedALL_F,    ylab = "Coho (%)"),
  CM = list(data = CM_pointsALLm, boot = chum_bootedALL_F,    ylab = "Chum (%)"),
  PK = list(data = PK_pointsALLm, boot = pink_bootedALL_F,    ylab = "Pink (%)"),
  SE = list(data = SE_pointsALLm, boot = sockeye_bootedALL_F, ylab = "Sockeye (%)")
)


# ======================================================================
# RAINCLOUD PLOT FUNCTION
# ======================================================================

generate_plot <- function(species_data, species_boot, ylab_text) {
  
  ggplot(species_data,
         aes(x = Estuary, y = !!sym(names(species_data)[4]),
             fill = Estuary, colour = Estuary)) +
    
    geom_flat_violin(position = position_nudge(x=.25), adjust=2) +
    geom_point(position = position_jitter(width=.15), size=.5) +
    
    geom_point(data = species_boot,
               aes(x = Estuary, y = statistic),
               position = position_nudge(.25), colour="BLACK") +
    
    geom_errorbar(data = species_boot,
                  aes(x = Estuary, y = statistic, ymin = lower_ci, ymax = upper_ci),
                  width = 0.1, size=.8, colour="BLACK",
                  position = position_nudge(.25)) +
    
    ylab(ylab_text) + xlab("") +
    guides(fill=FALSE, colour=FALSE) +
    theme(axis.text.x = element_text(angle=90)) +
    facet_grid(Season ~ block) +
    scale_colour_brewer(palette="Dark2") +
    scale_fill_brewer(palette="Dark2")
}


# ======================================================================
# GENERATE ALL PLOTS & SAVE
# ======================================================================

plots <- list()
for (sp in names(species_list)) {
  plots[[sp]] <- generate_plot(species_list[[sp]]$data,
                               species_list[[sp]]$boot,
                               species_list[[sp]]$ylab)
}

summary_all.val <- ggarrange(
  plots$CK + rremove("x.text"),
  plots$PK + rremove("x.text"),
  plots$CO + rremove("x.text"),
  plots$SE,
  plots$CM,
  ncol = 2, nrow = 3
)

ggsave(summary_all.val,
       filename = "Fig4_summary_salmon_all_values.png",
       width = 12, height = 12)


# ======================================================================
# BUILD LONG-FORMAT SPECIES DATA
# ======================================================================

long_data_list <- list(
  Chinook = CK_pointsALLm %>% select(block, Estuary, Chinook) %>% rename(value = Chinook),
  Coho    = CO_pointsALL   %>% select(block, Estuary, Coho)    %>% rename(value = Coho),
  Chum    = CM_pointsALLm  %>% select(block, Estuary, Chum)    %>% rename(value = Chum),
  Pink    = PK_pointsALLm  %>% select(block, Estuary, Pink)    %>% rename(value = Pink),
  Sockeye = SE_pointsALLm  %>% select(block, Estuary, Sockeye) %>% rename(value = Sockeye)
)

long_data_all <- bind_rows(
  lapply(names(long_data_list), function(sp) {
    long_data_list[[sp]] %>% mutate(species = sp)
  })
) %>% filter(!is.na(value))


# ======================================================================
# FULL BOOTSTRAP BY (BLOCK × ESTUARY × SPECIES)
# ======================================================================

## CI extraction
get_bootstrap_ci <- function(vec, R = 10000) {
  b <- boot(vec, statistic = boot_mean, R = R)
  ci <- tryCatch(boot.ci(b, conf=0.95, type="basic"),
                 error = function(e) return(NULL))
  
  if (!is.null(ci)) {
    tibble(
      mean = b$t0,
      lower_CI = max(0, ci$basic[4]),
      upper_CI = ci$basic[5]
    )
  } else {
    tibble(mean=NA, lower_CI=NA, upper_CI=NA)
  }
}

## Compute
boot_by_block_estuary <- long_data_all %>%
  group_by(block, Estuary, species) %>%
  summarise(n = n(), get_bootstrap_ci(value), .groups = "drop")

boot_by_block_allestuaries <- long_data_all %>%
  group_by(block, species) %>%
  summarise(n=n(), get_bootstrap_ci(value), .groups="drop") %>%
  mutate(Estuary = "All Estuaries") %>%
  select(block, Estuary, everything())

## Merge
final_bootstrap_table <- bind_rows(
  boot_by_block_estuary,
  boot_by_block_allestuaries
) %>%
  arrange(species, block, Estuary) %>%
  mutate(
    mean = round(mean, 2),
    lower_CI = round(lower_CI, 2),
    upper_CI = round(upper_CI, 2),
    summary = paste0(mean, " (", lower_CI, "-", upper_CI, ")")
  )

print(final_bootstrap_table)


# ======================================================================
# EXPORT TABLE
# ======================================================================

write_csv(final_bootstrap_table,
          "Tab2_bootstrapped_means_and_CIs_by_block_estuary_species.csv")

