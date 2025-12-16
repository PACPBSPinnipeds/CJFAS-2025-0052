############################################################
#   ALLUVIAL PLOTS – SALMON, HAKE & HERRING CO-OCCURRENCE
############################################################

## Load packages ----
library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(ggalluvial)
library(pals)
library(ggpubr)


## Data preparation ----
# Extract salmon, hake, herring for samples with any salmon present
salmon <- post2015diet %>%
  mutate(salmon = Chinook + Chum + Coho + Pink + Sockeye) %>%
  filter(salmon > 0) %>%
  dplyr::select(SampleID, Season, salmon, Hake, Herring)

# Reshape to long format (salmon, Hake, Herring)
diet_long <- salmon %>%
  pivot_longer(
    cols = 3:5,
    names_to = "prey",
    values_to = "percent",
    values_drop_na = TRUE
  )


## Categorize diet proportions ----
dietcat <- diet_long %>%
  mutate(
    diet_cat = ifelse(
      percent == 0, "ABSENT",
      ifelse(
        percent > 0 & percent < 33, "LOW",
        ifelse(
          percent > 32.9 & percent < 66, "MED",
          ifelse(percent > 65.9 & percent < 101, "HIGH", "x")
        )
      )
    )
  )

# Factor levels: prey type
dietcat$prey <- factor(
  dietcat$prey,
  levels = c("Hake", "Herring", "salmon")
)

# Factor levels: diet categories
dietcat$diet_cat <- factor(
  dietcat$diet_cat,
  levels = c("ABSENT", "LOW", "MED", "HIGH")
)


## Subset for salmon vs hake alluvial plot ----
dietcatMODhake <- dietcat %>%
  filter(prey == "salmon" | prey == "Hake")


## Plot 1: Salmon vs Hake alluvial ----
plotsal <- ggplot(
  dietcatMODhake,
  aes(
    x = prey,
    stratum = diet_cat,
    alluvium = SampleID,
    fill = diet_cat,
    label = diet_cat
  )
) +
  scale_fill_manual(values = as.vector(stepped3(4))) +
  geom_flow(stat = "alluvium") +
  geom_stratum() +
  geom_text(stat = "stratum") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16),
    axis.title.x = element_blank()
  )


## Subset for salmon vs herring alluvial plot ----
dietcatMODher <- dietcat %>%
  filter(prey == "salmon" | prey == "Herring")


## Plot 2: Salmon vs Herring alluvial ----
plotsalher <- ggplot(
  dietcatMODher,
  aes(
    x = prey,
    stratum = diet_cat,
    alluvium = SampleID,
    fill = diet_cat,
    label = diet_cat
  )
) +
  scale_fill_manual(values = as.vector(stepped3(4))) +
  geom_flow(stat = "alluvium") +
  geom_stratum() +
  geom_text(stat = "stratum") +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 16),
    axis.title.x = element_blank()
  )


## Combine alluvial panels ----
summary_alluvial.co_occur_DNA <- ggarrange(
  plotsal,
  plotsalher,
  labels = c("a)", "b)"),
  hjust = -3,
  vjust = 2,
  ncol = 2,
  nrow = 1
)


## Save figure ----
ggsave(
  summary_alluvial.co_occur_DNA,
  filename = paste("Fig9_summary_alluvial.co_occur_DNA", ".png", sep = ""),
  width = 10.5,
  height = 6.0
)
