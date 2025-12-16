############################################################
#   SALMON DIET — PREVALENCE ANALYSES (PRE VS POST 2015)
############################################################

# ================================
# Define factor levels
# ================================
season_levels <- c("Spring", "Summer", "Fall")
estuary_levels <- c("Belle Chain", "Estuary", "Non-Estuary")

# ================================
# Data preparation
# ================================
data.diet <- combined_dietM %>%
  dplyr::select(SampleID, Year, Season, Location, Estuary,
                Chinook, Chum, Coho, Pink, Sockeye)

# ================================
# Mean diet % (pre vs post)
# ================================
salmon.data.pre <- data.diet %>%
  filter(Year < 2015) %>%
  ungroup() %>%
  dplyr::select(Chinook, Coho, Chum, Pink, Sockeye) %>%
  summarise_all(mean)

salmon.data.post <- data.diet %>%
  filter(Year > 2014) %>%
  dplyr::select(Estuary, Chinook, Coho, Chum, Pink, Sockeye) %>%
  group_by(Estuary) %>%
  summarise_all(mean)

# ================================
# Reshape to long format + presence/absence
# ================================
diet_long <- data.diet %>%
  pivot_longer(
    cols = 6:10,
    names_to = "prey",
    values_to = "percent",
    values_drop_na = TRUE
  ) %>%
  mutate(
    PA = if_else(percent > 0, 1, 0),
    Run = 1
  )

# ================================
# Prevalence calculation function
# ================================
calculate_prev <- function(data, year_min = NULL, year_max = NULL) {
  
  filtered_data <- data
  
  if (!is.null(year_min)) filtered_data <- filtered_data %>% filter(Year >= year_min)
  if (!is.null(year_max)) filtered_data <- filtered_data %>% filter(Year <= year_max)
  
  filtered_data %>%
    group_by(Season, Estuary, prey) %>%
    summarise(
      Pos = sum(PA, na.rm = TRUE),
      TotalRun = sum(Run, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Prev = 100 * (Pos / TotalRun)) %>%
    arrange(desc(Prev)) %>%
    filter(TotalRun > 1)
}

# ================================
# Calculate prevalence (pre vs post 2015)
# ================================
prevpre  <- calculate_prev(diet_long, year_max = 2014)
prevpost <- calculate_prev(diet_long, year_min = 2015)

# ================================
# Apply factor levels
# ================================
prevpre <- prevpre %>%
  mutate(
    Season  = factor(Season,  levels = season_levels),
    Estuary = factor(Estuary, levels = estuary_levels)
  )

prevpost <- prevpost %>%
  mutate(
    Season  = factor(Season,  levels = season_levels),
    Estuary = factor(Estuary, levels = estuary_levels)
  )

# ================================
# Assign factor levels to final datasets
# ================================
salprevpre <- prevpre %>%
  mutate(
    Season  = factor(Season,  levels = season_levels),
    Estuary = factor(Estuary, levels = estuary_levels)
  )

salprevpost <- prevpost %>%
  mutate(
    Season  = factor(Season,  levels = season_levels),
    Estuary = factor(Estuary, levels = estuary_levels)
  )

# ================================
# Plotting theme
# ================================
prev_theme <- theme(
  axis.text.x = element_text(colour = "grey20", size = 12),
  axis.title.y = element_text(size = 14),
  strip.text.y = element_text(angle = 0, size = 7),
  panel.grid.major.x = element_line(color = "grey"),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank()
)

# ================================
# Factor ordering for prey species
# ================================
salmon_species <- c("Sockeye", "Pink", "Coho", "Chum", "Chinook")

salprevpre$prey  <- factor(salprevpre$prey,  levels = salmon_species)
salprevpost$prey <- factor(salprevpost$prey, levels = salmon_species)

# ================================
# Plotting function
# ================================
generate_plot <- function(data, title) {
  
  ggplot(data, aes(y = Prev, x = prey)) +
    geom_bar(position = "dodge", stat = "identity") +
    ylim(0, 70) +
    xlab("") +
    ylab("Prevalence %") +
    geom_text(aes(label = Pos),
              color = "grey20", size = 3,
              hjust = -0.1, vjust = 0.2,
              position = position_dodge(width = 1)) +
    geom_text(aes(x = 5, y = 55, label = TotalRun, group = TotalRun),
              size = 4, inherit.aes = FALSE) +
    coord_flip() +
    theme_bw() +
    prev_theme +
    facet_grid(Season ~ Estuary, drop = FALSE) +
    theme(
      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 12, angle = -90)
    ) +
    ggtitle(title)
}

# ================================
# Generate plots
# ================================
prevplotpost <- generate_plot(salprevpost, "Post-2015 Salmon Prevalence")
prevplotpre  <- generate_plot(salprevpre,  "Pre-2015 Salmon Prevalence")

# ================================
# Combine plots
# ================================
summary_prev <- ggpubr::ggarrange(
  prevplotpre + ggpubr::rremove("x.text"),
  prevplotpost,
  labels = c("A", "B"),
  ncol = 1,
  nrow = 2,
  hjust = 0
)

# ================================
# Save plot + export tables
# ================================
ggsave(
  summary_prev,
  filename = "Fig3_summary_salmon_prev.png",
  width = 10.5,
  height = 12.0
)

