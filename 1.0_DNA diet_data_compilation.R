library(tidyverse)
library(janitor)
library(lubridate)
library(here)

#load datasets
pre2015diet<-read_csv(here("csv","salmon_seal diet_pre2015.csv")) 
post2015diet<-read_csv(here("csv","salmon_seal diet_post2015.csv")) 

#1st, create pivot table of samples per site

post2015diet$Season <-  factor(post2015diet$Season, levels = c("Spring", "Summer", "Fall"))

#Create Table S1 Locations (latitude/longitude) and number of samples collected in each season and year from individual sites 2015-2019.
library(pivottabler)
pt <- PivotTable$new()
pt$addData(post2015diet)
pt$addColumnDataGroups("Year")
pt$addRowDataGroups("Location")
pt$addRowDataGroups("Latitude", addTotal=FALSE)
pt$addRowDataGroups("Longitude", addTotal=FALSE)
pt$addRowDataGroups("Season", addTotal=FALSE)                #     
pt$defineCalculation(calculationName="Total", summariseExpression="n()")
pt$renderPivot()
pt$evaluatePivot()

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data", 
                         topRowNumber=1, leftMostColumnNumber=1, 
                         applyStyles=TRUE, mapStylesFromCSS=TRUE)
saveWorkbook(wb, file="TS1_sampleXsite pivot.xlsx", overwrite = TRUE)

##--------------------------------------
##COMBINE PRE- AND POST-2015 DATASETS
combined_diet <- bind_rows(pre2015diet, post2015diet) 

#partition out Belle Chain
combined_dietM <- within(combined_diet, {
  f <- Estuary == 'Estuary' & Location == 'Belle Chain'
  Estuary[f] <- 'Belle Chain'
}) 

combined_dietM <- combined_dietM %>% dplyr::select(-f) 
combined_dietM$oldEstuary=combined_diet$Estuary
combined_dietM  <-  combined_dietM %>% mutate(block=as.factor(Year)) 

#create a time block variable-pre and post 2015 i.e. 2012-2014 and current dataset 2015-19
levels(combined_dietM$block)[levels(combined_dietM$block)=="2012"] <- "pre-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2013"] <- "pre-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2014"] <- "pre-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2015"] <- "post-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2016"] <- "post-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2017"] <- "post-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2018"] <- "post-2015"
levels(combined_dietM$block)[levels(combined_dietM$block)=="2019"] <- "post-2015"
combined_dietM$block <- factor(combined_dietM$block, levels = c("pre-2015" , "post-2015"))
combined_dietM$Estuary <- factor(combined_dietM$Estuary, levels = c("Estuary" , "Non-Estuary", "Belle Chain"))

#Create Table 1 Totals for scat samples collected 2012-14 (data from Thomas et al. 2022) and  2015-19 (this study) in different seasons and sampling locales
library(pivottabler)
pt <- PivotTable$new()
pt$addData(combined_dietM)
pt$addColumnDataGroups("Estuary")
pt$addRowDataGroups("Year")
pt$addRowDataGroups("Season", addTotal=FALSE)                
pt$defineCalculation(calculationName="Total", summariseExpression="n()")
pt$renderPivot()
pt$evaluatePivot()

library(openxlsx)
wb <- createWorkbook(creator = Sys.getenv("USERNAME"))
addWorksheet(wb, "Data")
pt$writeToExcelWorksheet(wb=wb, wsName="Data", 
                         topRowNumber=1, leftMostColumnNumber=1, 
                         applyStyles=TRUE, mapStylesFromCSS=TRUE)
saveWorkbook(wb, file="T1_sampleXblock pivot.xlsx", overwrite = TRUE)

