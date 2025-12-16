#========================================
# Bootstrapping All-Year Salmon Data
#========================================

library(tidyverse)
library(magrittr)
library(boot)
options(scipen = 999)

#----------------------------------------
# Generalized bootstrapping + plotting function
#----------------------------------------
bootstrap_species_plot <- function(data, species_col, species_name) {
  
  # Fill missing combinations with small value
  data_filled <- data %>%
    dplyr::select(Estuary, Season, Year, all_of(species_col)) %>%
    complete(Estuary, Season, Year, fill = setNames(list(0.001), species_col))
  
  # Summary statistics
  summary_stats <- data_filled %>%
    ungroup() %>%
    group_by(Year, Estuary, Season) %>%
    summarise(
      N   = length(.data[[species_col]]),
      tot = mean(.data[[species_col]])
    )
  
  # Nest the data
  nested_data <- data_filled %>%
    group_by(Year, Estuary, Season) %>%
    tidyr::nest()
  
  # Bootstrap function
  boot_mean <- function(d, i) mean(d[i])
  
  # Apply bootstrap
  nested_data %<>%
    mutate(
      booted = purrr::map(
        data,
        ~ boot::boot(data = .x[[species_col]], statistic = boot_mean, R = 10000, stype = "i")
      ),
      booted_ci = purrr::map(
        booted,
        ~ boot::boot.ci(.x, conf = 0.95, type = "basic")
      )
    )
  
  # Extract statistics and CIs
  booted_results <- nested_data %>%
    mutate(
      statistic = purrr::map(booted_ci, ~ .x$t0),
      lower_ci  = purrr::map(booted_ci, ~ .x$basic[[4]]),
      upper_ci  = purrr::map(booted_ci, ~ .x$basic[[5]])
    ) %>%
    dplyr::select(-data, -booted, -booted_ci) %>%
    tidyr::unnest(cols = c(statistic, lower_ci, upper_ci)) %>%
    left_join(summary_stats) %>%
    mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall")))
  
  # Generate plot
  p <- ggplot(booted_results, aes(x = Year, y = statistic, colour = Estuary)) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.1) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = 2014.5, linetype = "dotted", color = "black", size = 0.5) +
    facet_grid(Season ~ ., scales = "free") +
    ggtitle(species_name) +
    theme(
      strip.text.x = element_text(size = 13),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_rect(fill = "white", colour = "black")
    )
  
  return(list(data = booted_results, plot = p))
}

#----------------------------------------
# Apply to all salmon species
#----------------------------------------
chinook_results <- bootstrap_species_plot(combined_dietM, "Chinook", "Chinook")
coho_results    <- bootstrap_species_plot(combined_dietM, "Coho", "Coho")
chum_results    <- bootstrap_species_plot(combined_dietM, "Chum", "Chum")
pink_results    <- bootstrap_species_plot(combined_dietM, "Pink", "Pink")
sockeye_results <- bootstrap_species_plot(combined_dietM, "Sockeye", "Sockeye")

#----------------------------------------
# Save bootstrapped results
#----------------------------------------
chinook_bootedALLyr_F <- chinook_results$data
coho_bootedALLyr_F    <- coho_results$data
chum_bootedALLyr_F    <- chum_results$data
pink_bootedALLyr_F    <- pink_results$data
sockeye_bootedALLyr_F <- sockeye_results$data

#----------------------------------------
# Display plots
#----------------------------------------
# chinook_results$plot
# coho_results$plot
# chum_results$plot
# pink_results$plot
# sockeye_results$plot

# save plots to files
# ggsave("Chinook_plot.png", chinook_results$plot, width = 8, height = 6)

#========================================
# BOOTSTRAP & PLOTTING: ALL SALMON SPECIES COWICHAN
#========================================

library(tidyverse)
library(magrittr)
library(boot)
options(scipen=999)

#----------------------------------------
# General function for bootstrapping a species
#----------------------------------------
bootstrap_species <- function(data, species_col, species_name) {
  
  # Fill missing combinations
  data_filled <- data %>%
    dplyr::select(Estuary, Season, Year, all_of(species_col)) %>%
    complete(Estuary, Season, Year, fill = setNames(list(0.001), species_col))
  
  # Summary stats
  summary_stats <- data_filled %>%
    ungroup() %>%
    group_by(Year, Estuary, Season) %>%
    summarise(N = length(.data[[species_col]]),
              tot = mean(.data[[species_col]]))
  
  # Nest for bootstrapping
  nested_data <- data_filled %>%
    group_by(Year, Estuary, Season) %>%
    tidyr::nest()
  
  # Bootstrap function
  boot_mean <- function(d, i) mean(d[i])
  
  nested_data %<>%
    mutate(
      booted = purrr::map(data, ~ boot::boot(data = .x[[species_col]], statistic = boot_mean, R = 10000, stype = "i")),
      booted_ci = purrr::map(booted, ~ boot::boot.ci(.x, conf = 0.95, type = "basic")),
      statistic = purrr::map(booted_ci, ~ .x$t0),
      lower_ci  = purrr::map(booted_ci, ~ .x$basic[[4]]),
      upper_ci  = purrr::map(booted_ci, ~ .x$basic[[5]])
    ) %>%
    dplyr::select(-data, -booted, -booted_ci) %>%
    unnest(cols = c(statistic, lower_ci, upper_ci)) %>%
    left_join(summary_stats) %>%
    mutate(Species = species_name,
           Season = factor(Season, levels = c("Spring", "Summer", "Fall")))
  
  return(nested_data)
}

#----------------------------------------
# Prepare Cowichan dataset
#----------------------------------------
Cowichan <- combined_dietM %>% filter(Location == "Cowichan")
Cowichan$Estuary <- as.character(Cowichan$Estuary)
CowichanM <- within(Cowichan, {
  f <- Estuary == 'Estuary' & Location == 'Cowichan'
  Estuary[f] <- 'Cowichan'
})
CowichanM <- CowichanM %>% dplyr::select(-f)

#----------------------------------------
# Bootstrap all species for Cowichan
#----------------------------------------
species_list <- c("Chinook", "Coho", "Chum", "Pink", "Sockeye")

cowichan_booted <- purrr::map_dfr(species_list, ~ bootstrap_species(CowichanM, .x, .x))

#----------------------------------------
#Combine with ALLyr dataset
#----------------------------------------
# Add species column to ALLyr datasets if not already present
allyr_list <- list(chinook_bootedALLyr_F, coho_bootedALLyr_F, chum_bootedALLyr_F, pink_bootedALLyr_F, sockeye_bootedALLyr_F)
allyr_booted <- purrr::map2_dfr(allyr_list, species_list, ~ mutate(.x, Species = .y))

# Combine datasets
alldatayr_bootstrapmeans <- bind_rows(allyr_booted, cowichan_booted)

#----------------------------------------
# Faceted plots
#----------------------------------------
# Estuary vs Non-Estuary
estuarycontrast <- alldatayr_bootstrapmeans %>% filter(Estuary %in% c("Estuary","Non-Estuary"))

plot_estuary <- ggplot(estuarycontrast, aes(x=Year, y=statistic, colour=Estuary)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1) +
  geom_line() + geom_point() +
  geom_vline(xintercept = 2014.5, linetype="dotted", color="black", size=0.5) +
  facet_grid(Species ~ Season, scales="free") +
  theme(strip.text = element_text(size=14),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="white", colour="black")) +
  ylab("diet (%)")

ggsave(plot_estuary, filename="FS2_summary_year_estuary_and_non-estuary.png", width=10, height=10)

# Cowichan vs Belle Chain
sitecontrast <- alldatayr_bootstrapmeans %>% filter(Estuary %in% c("Cowichan","Belle Chain"))

plot_site <- ggplot(sitecontrast, aes(x=Year, y=statistic, colour=Estuary)) +
  geom_errorbar(aes(ymin=lower_ci, ymax=upper_ci), width=.1) +
  geom_line() + geom_point() +
  geom_vline(xintercept = 2014.5, linetype="dotted", color="black", size=0.5) +
  facet_grid(Species ~ Season, scales="free") +
  theme(strip.text = element_text(size=14),
        axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        panel.grid = element_blank(),
        panel.background = element_rect(fill="white", colour="black")) +
  ylab("diet (%)")

ggsave(plot_site, filename="FS3_summary_year_BC_and_Cowichan.png", width=10, height=10)
