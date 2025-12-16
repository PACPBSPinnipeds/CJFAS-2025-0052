# rm(list = ls())
# gc()
# .rs.restartR()   # if in RStudio

#############################
# Load Libraries
#############################
library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(glmmTMB)
library(AICcmodavg)
library(knitr)
library(kableExtra)
library(magrittr)
library(report)


# ================================
# Load scat hard part data
# ================================
hardparts <- read_csv(here("csv","scat_hardparts_post2015.csv"))


###basic sample numbers
hake_salmon  <-  hardparts %>%
  filter(prey.name %in% c("HAKE", "SALMON")) %>%
  group_by(sampleID) %>%
  filter(all(c("HAKE", "SALMON") %in% prey.name)) %>%
  distinct(sampleID) 

herring_salmon  <-hardparts %>%
  filter(prey.name %in% c("HERRING", "SALMON")) %>%
  group_by(sampleID) %>%
  filter(all(c("HERRING", "SALMON") %in% prey.name)) %>%
  distinct(sampleID) 

herring <-  hardparts %>% filter(prey.name=="HERRING") %>% distinct(sampleID)
hake <-  hardparts %>% filter(prey.name=="HAKE") %>% distinct(sampleID)
salmon <-  hardparts %>% filter(prey.name=="SALMON") %>% distinct(sampleID)

#####################
# Create species table for Salmon
species_table <- hardparts %>%
  filter(prey.name == "SALMON") %>%
  group_by(Season, Estuary, Year, Complex, prey.name, sizeclass) %>%
  summarize(count = sum(count), n = n(), .groups = "drop") %>%
  mutate(size = as.character(sizeclass), 
         bones_per_scat = count / n) %>%
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall")))


# Change size class labels to their discrete lengths
size_class_map <- c(
  "SIZE.UNDET" = NA,
  "SIZE.XSM" = "<15CM", 
  "SIZE.VSM" = "16-29CM", 
  "SIZE.SM" = "30-45CM", 
  "SIZE.SM.MED" = "30-59CM", 
  "SIZE.MED" = "46-59CM", 
  "SIZE.MED.LG" = "46-90CM", 
  "SIZE.LG" = "60-90CM", 
  "SIZE.VLG" = "91-130CM", 
  "SIZE.LG.VLG" = "60-130CM", 
  "SIZE.XLG" = ">130CM"
)

species_table2 <- species_table %>%
  mutate(size = recode(sizeclass, !!!size_class_map)) %>%
  drop_na(size)

# Generate data table of hard parts 
species_table3 <- species_table2 %>%
  dplyr::select(Season, Estuary, prey.name, size, n) %>%
  group_by(Season, Estuary, prey.name, size) %>% summarize(total = sum(n), .groups = "drop") %>% 
  pivot_wider(names_from = size, values_from = total, values_fill = list(total = 0)) %>%
  ungroup()

# Reorder columns and split overlapping size categories
species_table4 <- species_table3 %>%
  mutate(
    `30-45CM` = `30-45CM` + `30-59CM` / 2,
    `46-59CM` = `46-59CM` + `30-59CM` / 2 + `46-90CM` / 2,
    `60-90CM` = `60-90CM` + `46-90CM` / 2
  ) %>%
  dplyr::select(-`30-59CM`, -`46-90CM`)

# Pivot longer
species_table5 <- species_table4 %>%
  pivot_longer(cols = 4:8, names_to = "size", values_to = "totals", values_drop_na = FALSE)

# Set factor levels for size
species_table5$size <- factor(species_table5$size, 
                              levels = c("60-90CM", "46-90CM", "46-59CM", "30-59CM", "30-45CM", 
                                         "16-29CM", "<15CM"))

# Create mid-point for size categories
mid_points <- c("<15CM" = 8, "16-29CM" = 22.5, "30-45CM" = 37.5, "46-59CM" = 52.5, "60-90CM" = 75)
species_table5$mid <- species_table5$size %>% recode(!!!mid_points)

###Occurrence of salmon bones 
s<-ggplot(data=species_table5, aes(x=size, y=totals)) +
  geom_bar(stat="identity") +  
  facet_grid(Season~Estuary) +
  ylim(0, 100) +
  theme(strip.text.x = element_text(size = 16))+
  theme(strip.text.y = element_text(size = 16)) +
  theme(axis.text.x = element_text(size = 15)) +
    theme(axis.text.y = element_text(size = 16)) +
  theme(axis.title = element_text(size = 16)) +
  theme( panel.grid.major.y =  element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(colour = "grey"), panel.background = element_rect(fill = "white",colour='black')) +
  coord_flip()
ggsave(s, filename = "Fig7_summary_Occurrence of salmon bones .png", width = 12, height = 12.0)

# Fit GLMM with Tweedie distribution
zi.hard1 <- glmmTMB(mid ~ Estuary + Season, family = tweedie(link = "log"), ziformula = ~1, 
                    weights = totals, data = species_table5)

# Model summary
summary(zi.hard1)
report(zi.hard1)

# Create prediction data frame
newdata <- expand.grid(Season = c("Spring", "Summer", "Fall"),
                       Estuary = c("Belle Chain", "Estuary", "Non-Estuary"),
                       totals = 1.5)

pred.hp <- predict(zi.hard1, newdata, type = "response", se = TRUE)
model.predicts.hp <- data.frame(newdata, pred.hp)

# Plot predictions
hpplot <- ggplot(model.predicts.hp, aes(x = Estuary, y = fit, colour = Estuary)) + 
  geom_errorbar(aes(ymin = fit - se.fit, ymax = fit + se.fit), width = .3) +
  geom_point(size = 3) +
  facet_grid(.~Season, scales = "free") +
  theme(strip.text.x = element_text(size = 13)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) +
  labs(y = "Predicted Length (cm)") +
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.background = element_rect(fill = "white", colour = 'black'))

ggsave(hpplot, filename = "Fig8_model_predicts_hard_part_mean_size.png", width = 10, height = 8)

##
library(broom.mixed)
library(dplyr)
library(knitr)
library(kableExtra)

# Extract and format
hard_table <- tidy(zi.hard1) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  na.omit() %>%
  rename(
    Parameter = term,
    Coefficient = estimate,
    Std_Error = std.error,
    z = statistic,
    p = p.value
  ) %>%
  mutate(across(c(Coefficient, Std_Error, z), round, 3),
         p = round(p, 5))


library(flextable)
library(officer)

# Create flextable
ft <- flextable(hard_table)

# Save to Word
doc <- read_docx() %>%
  body_add_par("GLMM Model Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Tab8_hardparts zimodel_table.docx")


# ================================
# sex effect
# ================================

# Filter and Preprocess Data
sex<-hardparts %>% filter(sex=="MALE"|sex=="FEMALE") %>% filter(sizeclass!="SIZE.UNDET")


sex_table <- sex %>% filter(prey.name=="SALMON") %>% group_by(sampleID, Season, Estuary,sex, prey.name, sizeclass) %>% 
  summarize(count = sum(count),n=n()) %>% mutate(size=as.character(sizeclass)) %>% mutate(bones_per_scat=count/n)  
sex_table$Season <- factor(sex_table$Season, levels = c("Spring", "Summer","Fall"))  


sex_table2 <- sex_table %>% 
  # mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.UNDET",NA,size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.XSM","<15CM",size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.VSM","16-29CM",size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.SM","30-45CM",size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.SM.MED","30-59CM",size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.MED","46-59CM",size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.MED.LG","46-90CM",size)) %>%
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.LG","60-90CM",size)) %>% 
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.VLG","91-130CM",size)) %>% 
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.LG.VLG","60-130CM",size)) %>% 
  mutate(size = ifelse(prey.name=="SALMON" & sizeclass=="SIZE.XLG",">130CM",size))


# generate data table of hardparts
sex_table3<- sex_table2 %>% filter(prey.name=="SALMON") %>% 
  dplyr::select(Season,Estuary,sex, prey.name,size,count,n,bones_per_scat) %>% 
  group_by(Season,Estuary,sex,prey.name,size) %>%  
  pivot_wider(names_from=size,values_from=n) %>% ungroup #number of bones per spp per scat

# replace NAs with zeros
sex_table3[is.na(sex_table3)] <- 0

# reorder categories
sex_table3<- sex_table3 %>% dplyr::select(Season,Estuary,sex, prey.name,`<15CM`,`16-29CM`,`30-45CM`,`30-59CM`,`46-59CM`,`46-90CM`,`60-90CM`)#,`NA`

# split larger category in to half and add to higher and lower size categories
sex_table4<- sex_table3 %>% mutate(`30-45CM`=`30-45CM`+`30-59CM`/2) %>%
  mutate(`46-59CM`=`46-59CM`+`30-59CM`/2 + `46-90CM`/2) %>%
  mutate(`60-90CM`=`60-90CM`+`46-90CM`/2) %>%
  dplyr::select(-`30-59CM`,-`46-90CM`)   # Currently no fish larger than 90CM


sex_table5<- sex_table4 %>% group_by(Season,Estuary, sex) %>% pivot_longer(cols=5:9,names_to = "size",
                                                                           values_to = "totals", 
                                                                           values_drop_na = FALSE)
sex_table5$size <- factor(sex_table5$size, levels = c("60-90CM" ,"46-59CM","30-45CM","16-29CM", "<15CM")) 


# Midpoint calculation for size categories
mid_values <- c("<15CM" = 8, "16-29CM" = 22.5, "30-45CM" = 37.5, "46-59CM" = 52.5, "60-90CM" = 75)
salmonsex_table <- sex_table5 %>%
  mutate(mid = recode(size, !!!mid_values))

# Fit GLMM with Tweedie distribution
zi.hard1sex <- glmmTMB(mid ~ sex + Season + Estuary, 
                       family = tweedie(link = "log"), ziformula = ~1, 
                       weights = totals, data = salmonsex_table)

summary(zi.hard1sex)

# Predictions for plotting
newdata <- expand.grid(Season = c("Spring", "Summer", "Fall"), 
                       Estuary = c("Estuary", "Non-Estuary", "Belle Chain"),
                       sex = c("MALE", "FEMALE"),
                       totals = 1.5)

pred.hp <- predict(zi.hard1sex, newdata, type = "response", se = TRUE)
model.predicts.hp <- data.frame(newdata, pred.hp)

model.predicts.hp <- model.predicts.hp %>%
  mutate(Season = factor(Season, levels = c("Spring", "Summer", "Fall")),
         Estuary = factor(Estuary, levels = c("Estuary", "Non-Estuary", "Belle Chain")),
         sex = factor(sex, levels = c("FEMALE", "MALE")))

# Plot predictions
dodge <- position_dodge(width = 0.3)
hpplot <- ggplot(model.predicts.hp, aes(x = Estuary, y = fit, colour = sex)) + 
  geom_errorbar(aes(ymin = fit - se.fit, ymax = fit + se.fit), width = .3, position = dodge) +
  geom_point(size = 3, position = dodge)

hp_f <- hpplot + facet_grid(. ~ Season, scales = "free") +
  theme(strip.text.x = element_text(size = 13)) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.title.x = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(legend.text = element_text(size = 16)) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), panel.background = element_rect(fill = "white", colour = 'black'))

print(hp_f)

# Save plot
ggsave(hp_f, filename = "FigS5_model_predicts_hard_part_mean_size_sex.png", width = 10, height = 8)

# Reporting
summary(zi.hard1sex)
report(zi.hard1sex)


##
# library(broom.mixed)
# library(dplyr)
# library(knitr)
# library(kableExtra)

# Extract and format
hard_table <- tidy(zi.hard1sex) %>%
  dplyr::select(term, estimate, std.error, statistic, p.value) %>%
  na.omit() %>%
  rename(
    Parameter = term,
    Coefficient = estimate,
    Std_Error = std.error,
    z = statistic,
    p = p.value
  ) %>%
  mutate(across(c(Coefficient, Std_Error, z), round, 3),
         p = round(p, 3))

# library(flextable)
# library(officer)

# Create flextable
ft <- flextable(hard_table)

# Save to Word
doc <- read_docx() %>%
  body_add_par("GLMM Model Summary", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "TabS4_hard_table_sex.docx")


# ================================
# co-occurence sizes for salmon, hake and herring 
# ================================

# Convert counts to presence/absence (PA) and aggregate by sample and size class
hp_longPA <- hardparts %>%  mutate(PA = replace(count, count >0, 1)) %>%  # presence/absence
  dplyr::select(sampleID, Season, Year, Location, SubSiteID, Complex, Estuary, 
         prey.name, sizeclass, PA) %>% 
  group_by(sampleID, Season, Year, Location, SubSiteID,Complex, Estuary, 
           prey.name, sizeclass) %>% 
  summarise(PA = sum(PA)) %>% # sum PA by group
  mutate(PAg = replace(PA, PA >0, 1)) %>% # convert to binary PA again
  unite(sp.size, prey.name:sizeclass, remove = FALSE) # combine species and sizeclass

# Spread data to wide format: sampleID x species.sizeclass
hp_group <- hp_longPA %>% ungroup() %>% 
  dplyr::select(sampleID,sp.size, PAg) %>% 
  group_by(sampleID,sp.size)   %>% 
  summarise(total=sum(PAg)) %>% 
  spread(key=sp.size, value=total) %>% 
  replace(is.na(.), 0) %>% 
  ungroup(sampleID) 

# ================================
# SCATS: Salmon vs Hake
# ================================

# Filter samples with salmon and hake present
scats_salmonwhake <- hp_group %>% filter(SALMON_SIZE.LG>0|SALMON_SIZE.MED.LG>0|
                                           SALMON_SIZE.SM.MED>0|SALMON_SIZE.VSM>0|
                                           SALMON_SIZE.MED>0|SALMON_SIZE.SM>0|
                                           SALMON_SIZE.XSM>0) %>% 
  dplyr::select(-HAKE_SIZE.UNDET) %>% 
  filter(HAKE_SIZE.LG>0|HAKE_SIZE.MED.LG>0|
           HAKE_SIZE.SM.MED>0|HAKE_SIZE.VSM>0|
           HAKE_SIZE.MED>0|HAKE_SIZE.SM>0|
           HAKE_SIZE.XSM>0|HAKE_SIZE.VLG>0) %>% 
  dplyr::select(sampleID,SALMON_SIZE.LG, SALMON_SIZE.MED.LG, 
         SALMON_SIZE.SM.MED,SALMON_SIZE.VSM, SALMON_SIZE.MED, 
         SALMON_SIZE.SM, SALMON_SIZE.XSM, HAKE_SIZE.LG, HAKE_SIZE.MED, 
         HAKE_SIZE.MED.LG, HAKE_SIZE.SM, HAKE_SIZE.SM.MED, 
         HAKE_SIZE.VLG,HAKE_SIZE.VSM,HAKE_SIZE.XSM) %>% 
  group_by(sampleID) 

# Pivot longer for species-sizeclass combination
hake <- scats_salmonwhake %>% pivot_longer(cols=2:16, names_to = "sp.sizeclass",
                                           values_to = "count", 
                                           values_drop_na = TRUE) %>% 
                              mutate(size=sp.sizeclass)


# Convert size class codes to readable length ranges
hake$sp.sizeclass <- factor(hake$sp.sizeclass, levels = c("SALMON_SIZE.XSM", "SALMON_SIZE.VSM", "SALMON_SIZE.SM", 
                                                          "SALMON_SIZE.SM.MED", "SALMON_SIZE.MED", "SALMON_SIZE.MED.LG", 
                                                          "SALMON_SIZE.LG", "HAKE_SIZE.XSM", "HAKE_SIZE.VSM", "HAKE_SIZE.SM", 
                                                          "HAKE_SIZE.SM.MED", "HAKE_SIZE.MED", "HAKE_SIZE.MED.LG", "HAKE_SIZE.LG", 
                                                          "HAKE_SIZE.VLG", "HAKE_SIZE.XLG"))

# Map factor levels to actual length ranges
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.XSM"] <- "<15CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.VSM"] <- "16-29CM" 
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.SM"] <- "30-45CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.SM.MED"] <- "30-59CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.MED"] <- "46-59CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.MED.LG"] <- "46-90CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="SALMON_SIZE.LG"] <- "60-90CM"


levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.XSM"] <- "<8CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.VSM"] <- "8-11CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.SM"] <- "12-20CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.MED"] <- "21-29CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.SM.MED"] <- "12-29CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.LG"] <- "30-34CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.MED.LG"] <- "21-34CM"
levels(hake$sp.sizeclass)[levels(hake$sp.sizeclass)=="HAKE_SIZE.VLG"] <- "35-70CM"

# Separate species and sizeclass
hake2 <- hake %>% ungroup() %>%  separate(size, into = c("sp", "sizeclass"), sep = "_") 

# Summarize total count per sample, species, size class
hake3 <- hake2 %>% ungroup() %>%  
  group_by(sampleID,sp, sp.sizeclass) %>% filter(sp.sizeclass!="SIZE.UNDET") %>% 
  summarize(total = sum(count))

# Assign midpoint lengths for plotting
hake3$mid<-0
hake3 <- hake3 %>% ungroup %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="<15CM",8,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="16-29CM",22.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="30-45CM",37.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="30-59CM",44.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="46-59CM",52.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="46-90CM",68,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="60-90CM",75,mid)) %>% 
  
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="<8CM",4,mid)) %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="8-11CM",9.5,mid)) %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="12-20CM",15.5,mid)) %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="21-29CM",25,mid)) %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="12-29CM",20.5,mid)) %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="30-34CM",32,mid))  %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="21-34CM",27.5,mid)) %>%
  mutate(mid = ifelse(sp=="HAKE" & sp.sizeclass=="35-70CM",52.5,mid)) #%>% 

# Multiply total by midpoints for plotting
hake4 <- hake3 %>% mutate(plot=total * mid)

##classify occurrences based on size
hake5 <- hake4 %>% dplyr::select(sampleID, sp, plot) %>%  filter(if_all(everything(.), ~. != 0)) %>% 
  group_by(sampleID, sp) %>% slice_max(plot, n=1) %>% ungroup() %>% 
  group_by(sampleID) %>%  pivot_wider(names_from = sp, values_from = plot) %>% 
  mutate(exceeds = case_when(HAKE>2.1*SALMON~">2x HAKE",
                             HAKE<2.1*SALMON & HAKE>SALMON~"HAKE",
                             SALMON>2.8*HAKE~">2x SALMON",
                             SALMON<2.8*HAKE & SALMON>HAKE~"SALMON",
                             SALMON==HAKE~"same"
  ))

#plot 1:1 line
plot(hake5$SALMON, hake5$HAKE)
abline(0,1)
abline(0,2)

#plot estimated maximum size of prey consumed by salmon and hake
data.sal<- data.frame(x=c(0,10,30,50,70),
                      y1=c(0,3.5,10.5,17.5,24.5),
                      y2=c(0,0.14,.42,0.7,0.98))

data.hake<- data.frame(y=c(0,10,30,50,70),
                       x1=c(0,4.8,14.3,23.8,33.3),
                       x2=c(0,0.07,0.2,0.33,0.47))


# ================================
# Co-occurrence plots: Salmon vs Hake
# ================================
sh <- ggplot(hake5, aes(x = SALMON, y=HAKE)) +
  geom_jitter(width = 0.75,height = 0.75, aes(colour = exceeds)) +
  geom_abline(slope=1, intercept=0, linetype = "longdash") + 
  geom_abline(slope=2, intercept=0, linetype = "dotdash") +
  geom_abline(slope=25, intercept=0, linetype = "dotted") +
  geom_abline(slope=0.5, intercept=0, linetype = "dotdash") + 
  geom_abline(slope=0.04, intercept=0, linetype = "dotted") +
  scale_x_continuous( expand = c(0,0) , limits = c(0,70) )+
  scale_y_continuous( expand = c(0,0), limits = c(0,70) ) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


sh2 <- ggplot() +
  geom_jitter(data= hake5, aes(x = SALMON, y=HAKE), width = 0.5,height = 0.5) +
  geom_abline(slope=1, intercept=0, linetype = "longdash") + 
  geom_abline(slope=2, intercept=0, linetype = "dotted") +
  geom_abline(slope=0.5, intercept=0, linetype = "dotted") +
  scale_x_continuous( expand = c(0,0) , limits = c(0,70) )+
  scale_y_continuous( expand = c(0,0), limits = c(0,70) ) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_line(data = data.sal, aes(x = x, y = y1), color = "black", alpha = 0.1) +
  geom_line(data = data.sal, aes(x = x, y= y2), color = "black", alpha = 0.1) + 
  geom_ribbon(data=data.sal, aes(x=x, ymin=y2, ymax=y1), fill="darkgrey", alpha = 0.4)   +
  geom_line(data = data.hake, aes(x = x1, y = y), color = "black", alpha = 0.1) +
  geom_line(data = data.hake, aes(x = x2, y= y), color = "black", alpha = 0.1) + 
  geom_ribbon(data=data.hake, aes(y=y, xmin=x2, xmax=x1), fill="grey", alpha = 0.2)   


# ================================
# Repeat same procedure for Salmon vs Herring
# ================================
# [The code for herring follows exactly the same structure as hake above, including size factor mapping, midpoints, plot calculation, and classification]


##scats with HERRING

scats_salmonwherring <- hp_group %>% filter(SALMON_SIZE.LG>0|SALMON_SIZE.MED.LG>0|
                                              SALMON_SIZE.SM.MED>0|SALMON_SIZE.VSM>0|
                                              SALMON_SIZE.MED>0|SALMON_SIZE.SM>0|
                                              SALMON_SIZE.XSM>0) %>% 
  # dplyr::select(-HERRING_SIZE.UNDET) %>% 
  filter(HERRING_SIZE.LG>0|HERRING_SIZE.MED.LG>0|
           HERRING_SIZE.SM.MED>0|HERRING_SIZE.VSM>0|
           HERRING_SIZE.MED>0|HERRING_SIZE.SM>0|
           HERRING_SIZE.XSM>0|HERRING_SIZE.VLG>0) %>% 
  dplyr::select(sampleID,SALMON_SIZE.LG, SALMON_SIZE.MED.LG, 
         SALMON_SIZE.SM.MED,SALMON_SIZE.VSM, SALMON_SIZE.MED, 
         SALMON_SIZE.SM, SALMON_SIZE.XSM, HERRING_SIZE.LG, HERRING_SIZE.MED, 
         HERRING_SIZE.MED.LG, HERRING_SIZE.SM, HERRING_SIZE.SM.MED, 
         HERRING_SIZE.VLG,HERRING_SIZE.VSM,HERRING_SIZE.XSM) %>% 
  group_by(sampleID) 


herring <- scats_salmonwherring %>% pivot_longer(cols=2:16, names_to = "sp.sizeclass",
                                                 values_to = "count", 
                                                 values_drop_na = TRUE) %>% 
  mutate(size=sp.sizeclass)


herring$sp.sizeclass <- factor(herring$sp.sizeclass, levels = c("SALMON_SIZE.XSM", "SALMON_SIZE.VSM", "SALMON_SIZE.SM", 
                                                                "SALMON_SIZE.SM.MED", "SALMON_SIZE.MED", "SALMON_SIZE.MED.LG", 
                                                                "SALMON_SIZE.LG", "HERRING_SIZE.XSM", "HERRING_SIZE.VSM", "HERRING_SIZE.SM", 
                                                                "HERRING_SIZE.SM.MED", "HERRING_SIZE.MED", "HERRING_SIZE.MED.LG", "HERRING_SIZE.LG", 
                                                                "HERRING_SIZE.VLG"))

levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.XSM"] <- "<15CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.VSM"] <- "16-29CM" 
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.SM"] <- "30-45CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.SM.MED"] <- "30-59CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.MED"] <- "46-59CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.MED.LG"] <- "46-90CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="SALMON_SIZE.LG"] <- "60-90CM"


levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.XSM"] <- "<8CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.VSM"] <- "8-10CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.SM"] <- "11-15CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.MED"] <- "16-29CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.SM.MED"] <- "11-29CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.LG"] <- "30-35CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.MED.LG"] <- "16-35CM"
levels(herring$sp.sizeclass)[levels(herring$sp.sizeclass)=="HERRING_SIZE.VLG"] <- "36-40CM"


herring2 <- herring %>% ungroup() %>%  separate(size, into = c("sp", "sizeclass"), sep = "_") 

herring3 <- herring2 %>% ungroup() %>%  
  group_by(sampleID,sp, sp.sizeclass) %>% filter(sp.sizeclass!="SIZE.UNDET") %>% 
  summarize(total = sum(count))


herring3$mid<-0
herring3 <- herring3 %>% ungroup %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="<15CM",8,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="16-29CM",22.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="30-45CM",37.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="30-59CM",44.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="46-59CM",52.5,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="46-90CM",68,mid)) %>%
  mutate(mid = ifelse(sp=="SALMON" & sp.sizeclass=="60-90CM",75,mid)) %>% 
  
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="<8CM",4,mid)) %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="8-10CM",9,mid)) %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="11-15CM",13,mid)) %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="16-29CM",22.5,mid)) %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="11-29CM",20,mid)) %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="30-35CM",32.5,mid))  %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="16-35CM",25.5,mid)) %>%
  mutate(mid = ifelse(sp=="HERRING" & sp.sizeclass=="36-40CM",38,mid)) 


herring4 <- herring3 %>% mutate(plot=total * mid)

##switch to PA

herring5 <- herring4 %>% dplyr::select(sampleID, sp, plot) %>%  filter(if_all(everything(.), ~. != 0)) %>% 
  group_by(sampleID, sp) %>% slice_max(plot, n=1) %>% ungroup() %>% 
  group_by(sampleID) %>%  pivot_wider(names_from = sp, values_from = plot) %>% 
  mutate(exceeds = case_when(HERRING>2*SALMON~">2x HERRING",
                             HERRING<2*SALMON & HERRING>SALMON~"HERRING",
                             SALMON>2.8*HERRING~">2x SALMON",
                             SALMON<2.8*HERRING & SALMON>HERRING~"SALMON",
                             SALMON==HERRING~"same"
  ))

plot(herring5$SALMON, herring5$HERRING)
abline(0,1)
abline(0,2)


shr <- ggplot(herring5, aes(x = SALMON, y=HERRING)) +
  geom_jitter(width = 0.75,height = 0.75, aes(colour = exceeds)) +
  geom_abline(slope=1, intercept=0, linetype = "longdash") + 
  geom_abline(slope=2, intercept=0, linetype = "dotdash") +
  geom_abline(slope=25, intercept=0, linetype = "dotted") +
  geom_abline(slope=0.5, intercept=0, linetype = "dotdash") + 
  geom_abline(slope=0.04, intercept=0, linetype = "dotted") +
  scale_x_continuous( expand = c(0,0) , limits = c(0,70) )+
  scale_y_continuous( expand = c(0,0), limits = c(0,45) ) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


shr2 <- ggplot() +
  geom_jitter(data= herring5, aes(x = SALMON, y=HERRING), width = 0.5,height = 0.5) +
  geom_abline(slope=1, intercept=0, linetype = "longdash") + 
  geom_abline(slope=2, intercept=0, linetype = "dotted") +
  geom_abline(slope=0.5, intercept=0, linetype = "dotted") +
  scale_x_continuous( expand = c(0,0) , limits = c(0,70) )+
  scale_y_continuous( expand = c(0,0), limits = c(0,70) ) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_line(data = data.sal, aes(x = x, y = y1), color = "black", alpha = 0.1) +
  geom_line(data = data.sal, aes(x = x, y= y2), color = "black", alpha = 0.1) + 
  geom_ribbon(data=data.sal, aes(x=x, ymin=y2, ymax=y1), fill="darkgrey", alpha = 0.4) 


# ================================
# Combine and save plots
# ================================
library(ggpubr)
summary_size.co_occur <- ggarrange(sh2,
                                   shr2, labels = c("a)", "b)"), 
                                   hjust=-3.5,
                                   vjust=2,
                                   ncol = 1, nrow = 2)


ggsave(summary_size.co_occur,filename=paste("Fig10_summary_size.co_occur", ".png",sep=""), 
       width = 10.5, height = 12) 

