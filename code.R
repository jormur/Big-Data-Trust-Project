#load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(ggeasy)
library(glmnet)
library(haven)
library(hdm)
library(hrbrthemes)
library(leaflet)
library(magrittr)
library(maps)
library(mapproj)
library(openxlsx)
library(plm)
library(plotly)
library(readxl)
library(sf)
library(tidyverse)
library(tmap)
library(viridis)



### IMPORTING DATA TO R ########
options(warn = -1)

#SET YOUR WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd("/Users/maryceciarelli/Desktop/Big Data for Social Analysis/Group project")

full_dataset <- read.csv("ESS-Data-Wizard.csv")

View(full_dataset)



### DATA CLEANING #########
#We deleted the following variables, since we don't really need them

full_dataset <- full_dataset %>%
  select(-prob, -stratum, - psu, - dweight, - pspwght, - pweight, - anweight)

summary(full_dataset)


full_dataset <- subset(full_dataset, 
                       !netusoft %in% c(7,8,9)
                       & !netustm %in% c(6666, 7777, 8888, 9999)
                       & !nwspol %in% c(7777,8888,9999)
                       & !ppltrst %in% c(77,88,99)
                       & !pplfair %in% c(77,88,99)
                       & !pplhlp %in% c(77,88,99)
                       & !actrolga %in% c(7,8,9)
                       & !cptppola %in% c(7,8,9)
                       & !euftf %in% c(77,88,99)
                       & !freehms %in% c(7,8,9)
                       & !gincdif %in% c(7,8,9)
                       & !hmsacld %in% c(7,8,9)
                       & !hmsfmlsh %in% c(7,8,9)
                       & !lrscale %in% c(77,88,99)
                       & !polintr %in% c(7,8,9)
                       & !prtdgcl %in% c(6,7,8,9)
                       & !psppipla %in% c(7,8,9)
                       & !psppsgva %in% c(7,8,9)
                       & !pstplonl %in% c(7,8,9)
                       & !sgnptit %in% c(7,8,9)
                       & !stfdem %in% c(77,88,99)
                       & !stfeco %in% c(77,88,99)
                       & !stfedu %in% c(77,88,99)
                       & !stfgov %in% c(77,88,99)
                       & !stfhlth %in% c(77,88,99)
                       & !stflife %in% c(77,88,99)
                       & !trstep %in% c(77,88,99)
                       & !trstlgl %in% c(77,88,99)
                       & !trstplc %in% c(77,88,99)
                       & !trstplt %in% c(77,88,99)
                       & !trstprl %in% c(77,88,99)
                       & !trstprt %in% c(77,88,99)
                       & !trstun %in% c(77,88,99)
                       & !vote %in% c(7,8,9)
                       & !imsmetn %in% c(7,8,9)
                       & !imdfetn %in% c(7,8,9)
                       & !impcntr %in% c(7,8,9)
                       & !imbgeco %in% c(77,88,99)
                       & !imueclt %in% c(77,88,99)
                       & !imwbcnt %in% c(77,88,99)
                       & !aesfdrk %in% c(7,8,9)
                       & !atchctr %in% c(77,88,99)
                       & !atcherp %in% c(77,88,99)
                       & !brncntr %in% c(7,8,9)
                       & !crmvct %in% c(7,8,9)
                       & !ctzcntr %in% c(7,8,9)
                       & !dscrgrp %in% c(7,8,9)
                       & !happy %in% c(77,88,99)
                       & !rlgdgr %in% c(77,88,99)
                       & !sclact %in% c(7,8,9)
                       & !sclmeet %in% c(77,88,99)
                       & !gndr %in% c(9)
                       & !agea %in% c(999)
                       & !eisced %in% c(0,77,88,99)
                       & !hinctnta %in% c(77,88,99)
                       & !iorgact %in% c(77,88,99)
                       & !marsts %in% c(66,77,88,99)
                       & !wkdcorga %in% c(66,77,88,99)
                       & !impfree %in% c(7,8,9)
                       & !impsafe %in% c(7,8,9)
                       & !imptrad %in% c(7,8,9)
                       & !ipbhprp %in% c(7,8,9)
                       & !iphlppl %in% c(7,8,9)
                       & !ipfrule %in% c(7,8,9)
                       & !iphlppl %in% c(7,8,9)
                       & !iprspot %in% c(7,8,9)
                       & !ipstrgv %in% c(7,8,9)
                       & !ipudrst %in% c(7,8,9))

summary(full_dataset)                       


#from the summary we can see that the two variables (imsmetn, imdfetn) present NA values, therefore we are going to eliminate those obs from the full dataset using na.omit()

full_dataset <- full_dataset %>% filter(complete.cases(.))

summary(full_dataset)

#Now that we have eliminated all the missing values/NAs, we can proceed and assign the correct classification to the different variables 

str(full_dataset)

full_dataset$proddate <- as.Date(full_dataset$proddate, "%d.%m.%Y")

full_dataset$netustm <- as.numeric(full_dataset$netustm)
full_dataset$nwspol <- as.numeric(full_dataset$nwspol)

# full_dataset$pstplonl <- as.factor(full_dataset$pstplonl)
# full_dataset$sgnptit <- as.factor(full_dataset$sgnptit)
# full_dataset$vote <- as.factor(full_dataset$vote)
# full_dataset$brncntr <- as.factor(full_dataset$brncntr)
# full_dataset$crmvct <- as.factor(full_dataset$crmvct)
# full_dataset$ctzcntr <- as.factor(full_dataset$ctzcntr)
# full_dataset$dscrage <- as.factor(full_dataset$dscrage)
# full_dataset$dscrdsb <- as.factor(full_dataset$dscrdsb)
# full_dataset$dscretn <- as.factor(full_dataset$dscretn)
# full_dataset$dscrgnd <- as.factor(full_dataset$dscrgnd)
# full_dataset$dscrgrp <- as.factor(full_dataset$dscrgrp)
# full_dataset$dscrlng <- as.factor(full_dataset$dscrlng)
# full_dataset$dscrntn <- as.factor(full_dataset$dscrntn)
# full_dataset$dscrrce <- as.factor(full_dataset$dscrrce)
# full_dataset$dscrrlg <- as.factor(full_dataset$dscrrlg)
# full_dataset$dscrsex <- as.factor(full_dataset$dscrsex)
# full_dataset$gndr <- as.factor(full_dataset$gndr)


full_dataset$trstep<-as.numeric(full_dataset$trstep)
full_dataset$trstlgl<-as.numeric(full_dataset$trstlgl)
full_dataset$trstplc<-as.numeric(full_dataset$trstplc)
full_dataset$trstplt<-as.numeric(full_dataset$trstplt)
full_dataset$trstprl<-as.numeric(full_dataset$trstprl)
full_dataset$trstprt<-as.numeric(full_dataset$trstprt)
full_dataset$trstun<-as.numeric(full_dataset$trstun)

# full_dataset$cntry <- as.factor(full_dataset$cntry)

summary(full_dataset)

#Finally, we can create other variables, for example agerange and year

# full_dataset <- full_dataset %>%
#   mutate(agerange = case_when(agea >= 70 ~ ">=70",
#                                agea < 70 & agea >= 60 ~ "60-69",
#                                agea < 60 & agea >= 50 ~ "50-59",
#                                agea < 50 & agea >= 40 ~ "40-49",
#                                agea < 40 & agea >= 30 ~ "30-39",
#                                agea < 30 & agea >= 18 ~ "18-29",
#                                TRUE ~ "<18"))


# We want R to recognize the new variable agerange as factor. We will directly write over the variable.
# The reason behind is that is easier to work with factor variables rather than character variables
# Moreover, for some of these variables the order of the different levels actually matters to us.
# full_dataset$agerange <- as.factor(full_dataset$agerange)


# We make sure that the variable is now recognized as factor, using the class() function
# class(full_dataset$agerange)


# Now, what we have to do is to display the levels and eventually set the order we want them to be
# levels(full_dataset$agerange)

# As we can see, the levels are not in the right order. To change them, we rewrite over the variables and set the order manually
# full_dataset$agerange <- factor((full_dataset$agerange), levels = c("<18","18-29", "30-39","40-49", "50-59","60-69",">=70"))


# We check that everything is good
# levels(full_dataset$agerange)

#Now we create the variable year, which will help us identify the time period the survey covers. The reason behind is that the already existing variable proddate is not accurate enough and refers to the period the survey is released but not the period considered
# For our new variable, we will consider the year in which the survey was ongoing, which means round 8 - 2017, round 9 - 2019, round 10 - 2021
# 
# full_dataset <- full_dataset %>%
#   mutate(year = case_when(essround == 8 ~ "2017",
#                           essround == 9 ~ "2019",
#                           essround == 10 ~ "2021"))


# full_dataset$year <- as.Date(full_dataset$year, "%Y")
# class(full_dataset$year)






####################################### CONTROL VARIABLES



# STEP I: CONTROL VARIABLES - Download, scraping and data cleaning



# Data Source 1 :WORLD BANK DATABANK CONTROL VARIABLES


# After being downloaded from the World Bank website, the original data sets have been pre-cleaned with spreadsheets
# We can now import each variable in a separate data frame:

# Get the list of sheet names from the Excel file

#EDIT TO YOUR WORKING DIRECTORY
# excel_file_world_bank <- "/Users/valentincatteau/Desktop/Education/3. NCCU/2. S2 - Spring 2023/3. Big Data for Social Analysis/Assignments/Group project/Final paper/P_Data_Extract_From_World_Development_Indicators_clean.xlsx"
excel_file_world_bank <- "Data/P_Data_Extract_From_World_Development_Indicators_clean.xlsx"
sheet_names <- excel_sheets(excel_file_world_bank)

# Create an empty list to store the data frames
data_list <- list()

# Loop through each sheet and read the data into a data frame
for (sheet_name in sheet_names) {
  world_bank_indicators <- read_excel(excel_file_world_bank, sheet = sheet_name)
  data_list[[sheet_name]] <- world_bank_indicators
}

# Access individual data frames using their sheet names

population <- data_list[["population"]]
population <- population %>%
  rename(pop_2017 = "2017", pop_2019 = "2019", pop_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(population)

GDP_per_capita <- data_list[["GDP_per_capita"]]
GDP_per_capita <- GDP_per_capita %>%
  rename(GDP_per_capita_2017 = "2017", GDP_per_capita_2019 = "2019", GDP_per_capita_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(GDP_per_capita)

GDP_growth_per_capita <- data_list[["GDP_growth_per_capita"]]
GDP_growth_per_capita <- GDP_growth_per_capita %>%
  rename(GDP_growth_per_capita_2017 = "2017", GDP_growth_per_capita_2019 = "2019", GDP_growth_per_capita_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(GDP_growth_per_capita)

inflation <- data_list[["inflation"]]
inflation <- inflation %>%
  rename(inflation_2017 = "2017", inflation_2019 = "2019", inflation_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(inflation)

inflation_GDP_deflator <- data_list[["inflation_GDP_deflator"]]
inflation_GDP_deflator <- inflation_GDP_deflator %>%
  rename(inflation_GDP_deflator_2017 = "2017", inflation_GDP_deflator_2019 = "2019", inflation_GDP_deflator_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(inflation_GDP_deflator)

revenue <- data_list[["revenue"]]
revenue <- revenue %>%
  rename(revenue_2017 = "2017", revenue_2019 = "2019", revenue_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(revenue)

tax_revenue <- data_list[["tax_revenue"]]
tax_revenue <- tax_revenue %>%
  rename(tax_revenue_2017 = "2017", tax_revenue_2019 = "2019", tax_revenue_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(tax_revenue)

expense <- data_list[["expense"]]
expense <- expense %>%
  rename(expense_2017 = "2017", expense_2019 = "2019", expense_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(expense)

health_expenditure <- data_list[["health_expenditure"]]
health_expenditure <- health_expenditure %>%
  rename(health_expenditure_2017 = "2017", health_expenditure_2019 = "2019", health_expenditure_latest = "latest") %>%
  select(-`Series Code`, -`Series Name`, -`2020`, -`2021`)
view(health_expenditure)

edu_expenditure <- data_list[["edu_expenditure"]]
edu_expenditure <- edu_expenditure %>%
  rename(edu_expenditure_2017 = "2017", edu_expenditure_2019 = "2019", edu_expenditure_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(edu_expenditure)

employment_ratio <- data_list[["employment_ratio"]]
employment_ratio <- employment_ratio %>%
  rename(employment_ratio_2017 = "2017", employment_ratio_2019 = "2019", employment_ratio_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(employment_ratio)

labor_force_rate <- data_list[["labor_force_rate"]]
labor_force_rate <- labor_force_rate %>%
  rename(labor_force_rate_2017 = "2017", labor_force_rate_2019 = "2019", labor_force_rate_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(labor_force_rate)

unemployment <- data_list[["unemployment"]]
unemployment <- unemployment %>%
  rename(unemployment_2017 = "2017", unemployment_2019 = "2019", unemployment_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(unemployment)

poverty_ratio <- data_list[["poverty_ratio"]]
poverty_ratio <- poverty_ratio %>%
  rename(poverty_ratio_2017 = "2017", poverty_ratio_2019 = "2019", poverty_ratio_2021 = "latest") %>%
  select(-`Series Code`, -`Series Name`, -`2018`, -`2020`)
view(poverty_ratio)

gini_index <- data_list[["gini_index"]]
gini_index <- gini_index %>%
  rename(gini_index_2017 = "2017", gini_index_2019 = "2019", gini_index_2021 = "latest") %>%
  select(-`Series Code`, -`Series Name`, -`2016`, -`2018`, -`2020`)
view(gini_index)

life_expectancy <- data_list[["life_expectancy"]]
life_expectancy <- life_expectancy %>%
  rename(life_expectancy_2017 = "2017", life_expectancy_2019 = "2019", life_expectancy_2021 = "2021") %>%
  select(-`Series Code`, -`Series Name`)
view(life_expectancy)


# Data Source 2: OTHER CONTROL VARIABLES


# REPEAT the importing and cleaning process for other control variables from different data sources


# HDI (Human Development Index)

#EDIT TO YOUR WORKING DIRECTORY
HDI <- read.csv("Data/HDI.csv")
# HDI <- read.csv("HDI.csv")
HDI <- HDI %>%
  filter(iso3 == "CHE"
         | iso3 == "CZE"
         | iso3 == "CZE"
         | iso3 == "EST"
         | iso3 == "FIN"
         | iso3 == "FRA"
         | iso3 == "HUN"
         | iso3 == "ISL"
         | iso3 == "ITA"
         | iso3 == "LTU"
         | iso3 == "NLD"
         | iso3 == "NOR"
         | iso3 == "PRT"
         | iso3 == "SVN") %>%
  select(iso3, country, hdi_2017, hdi_2019, hdi_2021)
HDI <- HDI %>%
  mutate(HDI, cntry = recode(iso3,
                             "CHE" = "CH",
                             "CZE" = "CZ",
                             "EST" = "EE",
                             "FIN" = "FI",
                             "FRA" = "FR",
                             "HUN" = "HU",
                             "ISL" = "IS",
                             "ITA" = "IT",
                             "LTU" = "LT",
                             "NLD" = "NL",
                             "NOR" = "NO",
                             "PRT" = "PT",
                             "SVN" = "SI")) %>%
  select(cntry, everything()) %>%
  select(-iso3, -country)

view(HDI)

# Democracy Index

democracy <- read_excel("Data/democracy_index.xlsx")
# democracy <- read_excel("democracy_index.xlsx")
democracy <- democracy %>%
  rename(cntry = "alpha-2")

view(democracy)

# Crime Index

crime <- read_excel("Data/crime_index.xlsx")
# crime <- read_excel("crime_index.xlsx")

view(crime)

# Corruption Index

corruption <- read_excel("Data/corruption_index.xlsx")
# corruption <- read_excel("corruption_index.xlsx")
corruption <- corruption %>%
  mutate(corruption, cntry = recode(ISO3,
                                    "CHE" = "CH",
                                    "CZE" = "CZ",
                                    "EST" = "EE",
                                    "FIN" = "FI",
                                    "FRA" = "FR",
                                    "HUN" = "HU",
                                    "ISL" = "IS",
                                    "ITA" = "IT",
                                    "LTU" = "LT",
                                    "NLD" = "NL",
                                    "NOR" = "NO",
                                    "PRT" = "PT",
                                    "SVN" = "SI")) %>%
  select(cntry, `CPI score 2017`, `CPI score 2019`, `CPI score 2021`) %>%
  rename(corruption_index_2017 = "CPI score 2017", corruption_index_2019 = "CPI score 2019", corruption_index_2021 = "CPI score 2021")

view(corruption)




# Data Source 3: COVID PANDEMIC CONTROL VARIABLES



# REPEAT the importing process for covid-related control variables from different data sources


  #from the WorldBank:


# Import only specify sheets from tthe Excel file to individual data frames:


# Death Rate

death_rate <- read_excel("Data/worldbank_covid.xlsx", 1)
view(death_rate)

# % of Population above 65

pop_above_65 <- read_excel("Data/worldbank_covid.xlsx", 2)
view(pop_above_65)

# Hospital beds per 1000 inhabitants

beds_per_1000 <- read_excel("Data/worldbank_covid.xlsx", 3)
view(beds_per_1000)


  # from the WHO

# Out-of-Pocket Health Expenditure per capita

out_of_pocket_exp <- read_excel("Data/additional_expenditure.xlsx", 1)
view(out_of_pocket_exp)

# Private Health Expenditure per capita

private_health_exp <- read_excel("Data/additional_expenditure.xlsx", 2)
view(private_health_exp)



#STEP II: MERGE all the control variables into a new data frame called control_variables



# Create a new data frame with variables "essround" and "cntry"

control_variables <- read_excel("Data/essround.xlsx")
# control_variables <- read_excel("essround.xlsx")
view(control_variables)


# add variables one by one, while assigning separate values for round 8, 9 and 10:


#1. WORLD BANK VARIABLES

# population

control_variables <- merge(control_variables, population[, c("cntry", "pop_2017", "pop_2019", "pop_2021")], by = "cntry", all.x = TRUE)
control_variables$population <- ifelse(control_variables$essround == 8,
                                       control_variables$pop_2017,
                                       ifelse(control_variables$essround == 9,
                                              control_variables$pop_2019,
                                              control_variables$pop_2021))
control_variables <- control_variables %>%
  select(-pop_2017, -pop_2019, -pop_2021)

# GDP_per_capita

control_variables <- merge(control_variables, GDP_per_capita[, c("cntry", "GDP_per_capita_2017", "GDP_per_capita_2019", "GDP_per_capita_2021")], by = "cntry", all.x = TRUE)
control_variables$GDP_per_capita <- ifelse(control_variables$essround == 8,
                                           control_variables$GDP_per_capita_2017,
                                           ifelse(control_variables$essround == 9,
                                                  control_variables$GDP_per_capita_2019,
                                                  control_variables$GDP_per_capita_2021))
control_variables <- control_variables %>%
  select(-GDP_per_capita_2017, -GDP_per_capita_2019, -GDP_per_capita_2021)

# GDP_growth_per_capita

control_variables <- merge(control_variables, GDP_growth_per_capita[, c("cntry", "GDP_growth_per_capita_2017", "GDP_growth_per_capita_2019", "GDP_growth_per_capita_2021")], by = "cntry", all.x = TRUE)
control_variables$GDP_growth_per_capita <- ifelse(control_variables$essround == 8,
                                                  control_variables$GDP_growth_per_capita_2017,
                                                  ifelse(control_variables$essround == 9,
                                                         control_variables$GDP_growth_per_capita_2019,
                                                         control_variables$GDP_growth_per_capita_2021))
control_variables <- control_variables %>%
  select(-GDP_growth_per_capita_2017, -GDP_growth_per_capita_2019, -GDP_growth_per_capita_2021)

# inflation

control_variables <- merge(control_variables, inflation[, c("cntry", "inflation_2017", "inflation_2019", "inflation_2021")], by = "cntry", all.x = TRUE)
control_variables$inflation <- ifelse(control_variables$essround == 8,
                                      control_variables$inflation_2017,
                                      ifelse(control_variables$essround == 9,
                                             control_variables$inflation_2019,
                                             control_variables$inflation_2021))
control_variables <- control_variables %>%
  select(-inflation_2017, -inflation_2019, -inflation_2021)

# inflation_GDP_deflator

control_variables <- merge(control_variables, inflation_GDP_deflator[, c("cntry", "inflation_GDP_deflator_2017", "inflation_GDP_deflator_2019", "inflation_GDP_deflator_2021")], by = "cntry", all.x = TRUE)
control_variables$inflation_GDP_deflator <- ifelse(control_variables$essround == 8,
                                                   control_variables$inflation_GDP_deflator_2017,
                                                   ifelse(control_variables$essround == 9,
                                                          control_variables$inflation_GDP_deflator_2019,
                                                          control_variables$inflation_GDP_deflator_2021))
control_variables <- control_variables %>%
  select(-inflation_GDP_deflator_2017, -inflation_GDP_deflator_2019, -inflation_GDP_deflator_2021)

# revenue

control_variables <- merge(control_variables, revenue[, c("cntry", "revenue_2017", "revenue_2019", "revenue_2021")], by = "cntry", all.x = TRUE)
control_variables$revenue <- ifelse(control_variables$essround == 8,
                                    control_variables$revenue_2017,
                                    ifelse(control_variables$essround == 9,
                                           control_variables$revenue_2019,
                                           control_variables$revenue_2021))
control_variables <- control_variables %>%
  select(-revenue_2017, -revenue_2019, -revenue_2021)

# tax_revenue

control_variables <- merge(control_variables, tax_revenue[, c("cntry", "tax_revenue_2017", "tax_revenue_2019", "tax_revenue_2021")], by = "cntry", all.x = TRUE)
control_variables$tax_revenue <- ifelse(control_variables$essround == 8,
                                        control_variables$tax_revenue_2017,
                                        ifelse(control_variables$essround == 9,
                                               control_variables$tax_revenue_2019,
                                               control_variables$tax_revenue_2021))
control_variables <- control_variables %>%
  select(-tax_revenue_2017, -tax_revenue_2019, -tax_revenue_2021)

# expense

control_variables <- merge(control_variables, expense[, c("cntry", "expense_2017", "expense_2019", "expense_2021")], by = "cntry", all.x = TRUE)
control_variables$expense <- ifelse(control_variables$essround == 8,
                                    control_variables$expense_2017,
                                    ifelse(control_variables$essround == 9,
                                           control_variables$expense_2019,
                                           control_variables$expense_2021))
control_variables <- control_variables %>%
  select(-expense_2017, -expense_2019, -expense_2021)

# health_expenditure

control_variables <- merge(control_variables, health_expenditure[, c("cntry", "health_expenditure_2017", "health_expenditure_2019", "health_expenditure_latest")], by = "cntry", all.x = TRUE)
control_variables$health_expenditure <- ifelse(control_variables$essround == 8,
                                               control_variables$health_expenditure_2017,
                                               ifelse(control_variables$essround == 9,
                                                      control_variables$health_expenditure_2019,
                                                      control_variables$health_expenditure_latest))
control_variables <- control_variables %>%
  select(-health_expenditure_2017, -health_expenditure_2019, -health_expenditure_latest)

# edu_expenditure

control_variables <- merge(control_variables, edu_expenditure[, c("cntry", "edu_expenditure_2017", "edu_expenditure_2019", "edu_expenditure_2021")], by = "cntry", all.x = TRUE)
control_variables$edu_expenditure <- ifelse(control_variables$essround == 8,
                                            control_variables$edu_expenditure_2017,
                                            ifelse(control_variables$essround == 9,
                                                   control_variables$edu_expenditure_2019,
                                                   control_variables$edu_expenditure_2021))
control_variables <- control_variables %>%
  select(-edu_expenditure_2017, -edu_expenditure_2019, -edu_expenditure_2021)

# employment_ratio

control_variables <- merge(control_variables, employment_ratio[, c("cntry", "employment_ratio_2017", "employment_ratio_2019", "employment_ratio_2021")], by = "cntry", all.x = TRUE)
control_variables$employment_ratio <- ifelse(control_variables$essround == 8,
                                             control_variables$employment_ratio_2017,
                                             ifelse(control_variables$essround == 9,
                                                    control_variables$employment_ratio_2019,
                                                    control_variables$employment_ratio_2021))
control_variables <- control_variables %>%
  select(-employment_ratio_2017, -employment_ratio_2019, -employment_ratio_2021)

# labor_force_rate

control_variables <- merge(control_variables, labor_force_rate[, c("cntry", "labor_force_rate_2017", "labor_force_rate_2019", "labor_force_rate_2021")], by = "cntry", all.x = TRUE)
control_variables$labor_force_rate <- ifelse(control_variables$essround == 8,
                                             control_variables$labor_force_rate_2017,
                                             ifelse(control_variables$essround == 9,
                                                    control_variables$labor_force_rate_2019,
                                                    control_variables$labor_force_rate_2021))
control_variables <- control_variables %>%
  select(-labor_force_rate_2017, -labor_force_rate_2019, -labor_force_rate_2021)

# unemployment

control_variables <- merge(control_variables, unemployment[, c("cntry", "unemployment_2017", "unemployment_2019", "unemployment_2021")], by = "cntry", all.x = TRUE)
control_variables$unemployment <- ifelse(control_variables$essround == 8,
                                         control_variables$unemployment_2017,
                                         ifelse(control_variables$essround == 9,
                                                control_variables$unemployment_2019,
                                                control_variables$unemployment_2021))
control_variables <- control_variables %>%
  select(-unemployment_2017, -unemployment_2019, -unemployment_2021)

# poverty_ratio

control_variables <- merge(control_variables, poverty_ratio[, c("cntry", "poverty_ratio_2017", "poverty_ratio_2019", "poverty_ratio_2021")], by = "cntry", all.x = TRUE)
control_variables$poverty_ratio <- ifelse(control_variables$essround == 8,
                                          control_variables$poverty_ratio_2017,
                                          ifelse(control_variables$essround == 9,
                                                 control_variables$poverty_ratio_2019,
                                                 control_variables$poverty_ratio_2021))
control_variables <- control_variables %>%
  select(-poverty_ratio_2017, -poverty_ratio_2019, -poverty_ratio_2021)

# gini_index

control_variables <- merge(control_variables, gini_index[, c("cntry", "gini_index_2017", "gini_index_2019", "gini_index_2021")], by = "cntry", all.x = TRUE)
control_variables$gini_index <- ifelse(control_variables$essround == 8,
                                       control_variables$gini_index_2017,
                                       ifelse(control_variables$essround == 9,
                                              control_variables$gini_index_2019,
                                              control_variables$gini_index_2021))
control_variables <- control_variables %>%
  select(-gini_index_2017, -gini_index_2019, -gini_index_2021)

# life_expectancy

control_variables <- merge(control_variables, life_expectancy[, c("cntry", "life_expectancy_2017", "life_expectancy_2019", "life_expectancy_2021")], by = "cntry", all.x = TRUE)
control_variables$life_expectancy <- ifelse(control_variables$essround == 8,
                                            control_variables$life_expectancy_2017,
                                            ifelse(control_variables$essround == 9,
                                                   control_variables$life_expectancy_2019,
                                                   control_variables$life_expectancy_2021))
control_variables <- control_variables %>%
  select(-life_expectancy_2017, -life_expectancy_2019, -life_expectancy_2021)



#2. OTHER VARIABLES

# hdi_index

control_variables <- merge(control_variables, HDI[, c("cntry", "hdi_2017", "hdi_2019", "hdi_2021")], by = "cntry", all.x = TRUE)
control_variables$hdi_index <- ifelse(control_variables$essround == 8,
                                      control_variables$hdi_2017,
                                      ifelse(control_variables$essround == 9,
                                             control_variables$hdi_2019,
                                             control_variables$hdi_2021))
control_variables <- control_variables %>%
  select(-hdi_2017, -hdi_2019, -hdi_2021)

# corruption_index

control_variables <- merge(control_variables, corruption[, c("cntry", "corruption_index_2017", "corruption_index_2019", "corruption_index_2021")], by = "cntry", all.x = TRUE)
control_variables$corruption_index <- ifelse(control_variables$essround == 8,
                                             control_variables$corruption_index_2017,
                                             ifelse(control_variables$essround == 9,
                                                    control_variables$corruption_index_2019,
                                                    control_variables$corruption_index_2021))
control_variables <- control_variables %>%
  select(-corruption_index_2017, -corruption_index_2019, -corruption_index_2021)

# crime_index

control_variables <- merge(control_variables, crime[, c("cntry", "crime_index_2017", "crime_index_2019", "crime_index_2021")], by = "cntry", all.x = TRUE)
control_variables$crime_index <- ifelse(control_variables$essround == 8,
                                        control_variables$crime_index_2017,
                                        ifelse(control_variables$essround == 9,
                                               control_variables$crime_index_2019,
                                               control_variables$crime_index_2021))
control_variables <- control_variables %>%
  select(-crime_index_2017, -crime_index_2019, -crime_index_2021)

# democracy_index

control_variables <- merge(control_variables, democracy[, c("cntry", "democracy_index_2017", "democracy_index_2019", "democracy_index_2021")], by = "cntry", all.x = TRUE)
control_variables$democracy_index <- ifelse(control_variables$essround == 8,
                                            control_variables$democracy_index_2017,
                                            ifelse(control_variables$essround == 9,
                                                   control_variables$democracy_index_2019,
                                                   control_variables$democracy_index_2021))
control_variables <- control_variables %>%
  select(-democracy_index_2017, -democracy_index_2019, -democracy_index_2021)



#3. COVID VARIABLES



#death_rate

control_variables <- merge(control_variables, death_rate[, c("cntry", "death_rate_2017", "death_rate_2019", "death_rate_2021")], by = "cntry", all.x = TRUE)
control_variables$death_rate <- ifelse(control_variables$essround == 8,
                                       control_variables$death_rate_2017,
                                       ifelse(control_variables$essround == 9,
                                              control_variables$death_rate_2019,
                                              control_variables$death_rate_2021))
control_variables <- control_variables %>%
  select(-death_rate_2017, -death_rate_2019, -death_rate_2021)

# pop_above_65

control_variables <- merge(control_variables, pop_above_65[, c("cntry", "pop_above_65_2017", "pop_above_65_2019", "pop_above_65_2021")], by = "cntry", all.x = TRUE)
control_variables$pop_above_65 <- ifelse(control_variables$essround == 8,
                                         control_variables$pop_above_65_2017,
                                         ifelse(control_variables$essround == 9,
                                                control_variables$pop_above_65_2019,
                                                control_variables$pop_above_65_2021))
control_variables <- control_variables %>%
  select(-pop_above_65_2017, -pop_above_65_2019, -pop_above_65_2021)

# beds_per_1000

control_variables <- merge(control_variables, beds_per_1000[, c("cntry", "beds_per_1000_2017", "beds_per_1000_2018", "beds_per_1000_latest")], by = "cntry", all.x = TRUE)
control_variables$beds_per_1000 <- ifelse(control_variables$essround == 8,
                                          control_variables$beds_per_1000_2017,
                                          ifelse(control_variables$essround == 9,
                                                 control_variables$beds_per_1000_2018,
                                                 control_variables$beds_per_1000_latest))
control_variables <- control_variables %>%
  select(-beds_per_1000_2017, -beds_per_1000_2018, -beds_per_1000_latest)

# out_of_pocket_exp

control_variables <- merge(control_variables, out_of_pocket_exp[, c("cntry", "out_of_pocket_exp_2017", "out_of_pocket_exp_2019", "out_of_pocket_exp_2020")], by = "cntry", all.x = TRUE)
control_variables$beds_per_1000 <- ifelse(control_variables$essround == 8,
                                          control_variables$out_of_pocket_exp_2017,
                                          ifelse(control_variables$essround == 9,
                                                 control_variables$out_of_pocket_exp_2019,
                                                 control_variables$out_of_pocket_exp_2020))
control_variables <- control_variables %>%
  select(-out_of_pocket_exp_2017, -out_of_pocket_exp_2019, -out_of_pocket_exp_2020)

# private_health_exp

control_variables <- merge(control_variables, private_health_exp[, c("cntry", "private_health_exp_2017", "private_health_exp_2019", "private_health_exp_2020")], by = "cntry", all.x = TRUE)
control_variables$private_health_exp <- ifelse(control_variables$essround == 8,
                                               control_variables$private_health_exp_2017,
                                               ifelse(control_variables$essround == 9,
                                                      control_variables$private_health_exp_2019,
                                                      control_variables$private_health_exp_2020))
control_variables <- control_variables %>%
  select(-private_health_exp_2017, -private_health_exp_2019, -private_health_exp_2020)

view(control_variables)


#STEP III: MERGE the control_variables data frame with the original full_dataset


full_dataset <- full_dataset %>%
  left_join(control_variables, by = c("cntry", "essround"))

# reorder variables to have basic variables population, GDP_per_capita and GDP_growth_per_capita in the first columns (the other control variables are the last columns)

full_dataset <- full_dataset %>%
  select(name, essround, edition, proddate, idno, cntry, population, GDP_per_capita, GDP_growth_per_capita, everything())

view(full_dataset)


###################### METHOD #############################################
#DON'T FORMAT THE VARIABLES AS FACTORS AND EXCLUDE THE AGE RANGE ADDITION
# We're looking to employ double LASSO selection

# We first have to identify our preliminary dependent and independent variables and their corresponding indexes 
grep("trstprl", colnames(full_dataset))
grep("nwspol", colnames(full_dataset))

# We assign them to the variables
# y <- full_dataset[, 40]
# X <- full_dataset[, -c(1:6,12,40)]
# d <- full_dataset[,12]
# 
# #To prove the validity of our choice of methodology, we will procedurally employ different regeression methods to show their weaknesses.
# #First, normal OLS regression:
# OLS <- summary(lm(trstprl ~., data = full_dataset))$coefficients[1, ]
# 
# #Secondly, Single step selection LASSO and Post-OLS
# 
# lasso <- rlasso(y~., data = X, post = FALSE) # = Run the Rigorous LASSO = #
# selected <- which(coef(lasso)[-c(1:2)] !=0) # = Select relevant variables = #
# formula <- paste(c("y ~ d", names(selected)), collapse = "+")
# SS <- summary(lm(formula, data = X))$coefficients[1, ]
# 
# #And finally, double selection
# DS <- rlassoEffects(trstprl~. , I=~nwspol, data=full_dataset)
# DS <-  summary(DS)$coefficients[1,]
# 
# results <- rbind(OLS,SS,DS)
# results
#Here we see that it is through double selection that we have the lowest standard error
#The other models present extremely small/insignificant coefficient estimates

### FORMAL IMPLEMENTATION ###
#Therefore, we move on with the formal implementation of the double selection method.
y <- full_dataset[, 40]
X <- as.matrix(full_dataset[, -c(1:6,12,40)])
d <- full_dataset[, 12]
varnames <- colnames(full_dataset)

doubleselect <- rlassoEffect(x=X, y=y, d=d, method = "double selection")
selected_vars <- doubleselect$selection.index
selected_data <- full_dataset[, selected_vars]
selected_data <- cbind(full_dataset$idno, full_dataset$cntry, full_dataset$trstprl, full_dataset$nwspol,
                       full_dataset$proddate, selected_data)

summary(doubleselect)
confint(doubleselect)
print(doubleselect)
plot(doubleselect)

### FIXED EFFECTS MODEL ###
#Now we can look to implement the fixed effects model estimation

#It was found that there are duplicate IDs between countries
#This is probably a reporting error stemming from the individual collection of surveys.
#Therefore, we can generate a new unique ID for each observation
# selected_data$idno <- sample(nrow(selected_data))

# 'y' is the response variable
# 'selected_data' is the matrix of selected variables
panel_data <- pdata.frame(selected_data, index = c("full_dataset.idno"))

# Fit fixed effects model
FE_model <- plm(full_dataset.trstprl ~ ., data = panel_data, model = "within")

# Obtain model summary
summary(FE_model)

######################################################################





### DATA VISUALIZATION #####

#Before applying the model, we want to explore and visualize our dataset. In this section we will perform some plots that will help us to quickly visualize our data.
#The purpose of these analyses is purely descriptive and we cannot identify any significant relationships. Nevertheless, it is still very useful for us as a starting point to understand and observe our data and finally build our model.


# Observations per country - bubble map

# First, we create our background


world_map <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify()

View(world_map)
ggplot(world_map)

ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill="deepskyblue", alpha=0.7)

# We will filter the library for the countries we need

world_map <- maps::map("world", ".", exact = FALSE, plot = FALSE, fill = TRUE) %>% fortify() %>%
  filter(region %in% c("Switzerland", "Czech Republic", "Estonia", "Finland","France", "Hungary","Iceland","Italy","Lithuania","Netherlands","Norway","Portugal","Slovenia"))

ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill="deepskyblue", alpha=0.7)

# We now create our dataframe with all the information we need for each country
map_df <- full_dataset %>%
  select(idno, cntry) %>%
  group_by(cntry) %>%
  summarize(observations = n())

View(map_df)

#Now we are going to add lat and long manually for each country (-> CH,CZ,EE,FI,FR,HU,IS,IT,LT,NL,NO,PT,SI)

long <- c(8.227512, 15.472962, 25.013607, 25.748152, 1.8883335, 19.5060937, -18.1059013, 12.674297, 23.7499997, 5.6343227, 8.7876653, -8.1353519, 14.8153333)
lat <- c(46.818188, 49.817493, 58.595272, 61.924110, 46.603354, 47.1817585, 64.9841821, 42.6384261, 55.3500003, 52.2434979, 61.1529386, 39.6621648, 46.1199444)

map_df <- map_df %>%
  mutate(latitude = lat, longitude = long)

# For better understanding we are going to change the name of the countries from acronym to the full form

map_df$cntry <- c("Switzerland", "Czech Republic", "Estonia", "Finland","France", "Hungary","Iceland","Italy","Lithuania","Netherlands","Norway","Portugal","Slovenia")
map_df

countrynames <- map_df$cntry

ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill="#87CEFA", alpha=0.7) +
  geom_point( data = map_df, aes(x=longitude, y=latitude, size=observations,),  color="mediumblue",  alpha=0.6) +
  scale_size_continuous(range=c(4, 18)) +
  geom_text (data = map_df, label = countrynames, aes(x=long, y=lat), hjust=0.5, vjust=-2.7, size=3) +
  theme_minimal() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
  ggtitle("Observations distribution, by country") + ggeasy::easy_center_title()

# At this point we also want to visualize the real population of each country and compare it with our observations. This will help us to understand if our sample is balanced 
# Population is displayed in millions and refers to the population of 2021

pop_2021 <- population$pop_2021

map_df <- map_df %>%
  mutate(population = pop_2021)

ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill="#87CEFA", alpha=0.7) +
  geom_point( data = map_df, aes(x=longitude, y=latitude, size=pop_2021,),  color="mediumblue",  alpha=0.6) +
  scale_size_continuous(range=c(4, 18)) +
  geom_text (data = map_df, label = countrynames, aes(x=long, y=lat), hjust=0.5, vjust=-2.7, size=3) +
  theme_minimal() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank()) +
  ggtitle("Population distribution, by country") + ggeasy::easy_center_title() 

#Gender distribution overall - pie chart
gender_ov_count <- table(full_dataset$gndr)
male_percent = round((gender_ov_count[1]/(gender_ov_count[1]+gender_ov_count[2]))*100, digits = 2)
female_percent = round((gender_ov_count[2]/(gender_ov_count[1]+gender_ov_count[2]))*100, digits = 2)
new_labels = c(paste('Male - ', male_percent, '%', sep = ''), paste('Female - ', female_percent, '%', sep = ''))
pie(gender_ov_count, labels = new_labels, main = 'Distribution of gender overall', col=c("#00BFFF", "#104E8B"), border = "white")



#Age distribution overall - bar plot

#For this plot, we will create another variables to show 3 age ranges and compare it with the distribution of population age in EU from 2011 to 2021

full_dataset <- full_dataset %>%
  mutate(agerange = case_when(agea >= 65 ~ "65+ years",
                              agea < 65 & agea >= 15 ~ "15-64 years",
                              TRUE ~ "0-14 years"))

# We want R to recognize the new variable agerange as factor. We will directly write over the variable.
# The reason behind is that is easier to work with factor variables rather than character variables
# Moreover, for some of these variables the order of the different levels actually matters to us.
full_dataset$agerange <- as.factor(full_dataset$agerange)
# We make sure that the variable is now recognized as factor, using the class() function
class(full_dataset$agerange)
# Now, what we have to do is to display the levels and eventually set the order we want them to be
levels(full_dataset$agerange)
# As we can see, the levels are not in the right order. To change them, we rewrite over the variables and set the order manually
full_dataset$agerange <- factor((full_dataset$agerange), levels = c("0-14 years", "15-64 years","65+ years"))

# We check that everything is good
levels(full_dataset$agerange)

age_range_overall_count <- table(full_dataset$agerange)
#barplot(age_range_overall_count, beside = T, main = 'Distribution of age', xlab = 'Age Range', 
 #      ylab = 'Count', ylim = c(0,7000), col=c("#87CEFA", "#104E8B", "#56B4E9"), border = "white")


#Age distribution overall - pie chart
prop_age_range <- prop.table(age_range_overall_count)
df_age_range <- as.data.frame(prop_age_range)

View(df_age_range)

lbs <- round(prop_age_range/sum(prop_age_range)*100, digits = 1)
lbs <- c(paste("0-14 years", "-", lbs[1], "%"), paste("15-64 years", "-", lbs[2], "%"), paste("65+ years", "-", lbs[3], "%"))

pie(prop_age_range, labels = lbs, main ='Distribution of age overall',
    col=c("#87CEFA", "#104E8B", "#56B4E9"), border = "white")  


#Average level trust - circular bar plot

# Let's first create our dataframe

trust_df <- full_dataset %>%
  select(cntry, trstep, trstlgl, trstplc, trstplt, trstprl, trstprt, trstun) %>%
  group_by(cntry) %>%
  summarize(EU = mean(trstep), legal_system = mean(trstlgl), police = mean(trstplc), politicians = mean(trstplt), parliament = mean(trstprl), political_parties = mean(trstprt), united_nations = mean(trstun))

# For this plot, we will change the variable cntry in a factor
trust_df$cntry <- as.factor(trust_df$cntry)

View(trust_df)
# Now we have to reshape the dataframe

trust_df <- trust_df %>%
  gather("trust_for", "level_trst", 2:8)
label_data <- trust_df
trust_df$level_trst <- as.numeric(trust_df$level_trst)
label_data$trust_for <- as.factor(label_data$trust_for)

View(trust_df)

# Set a number of 'empty bar'
empty_bar <- 2

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(trust_df))
colnames(to_add) <- colnames(trust_df)
trust_df <- rbind(trust_df, to_add)
trust_df$id <- seq(1, nrow(trust_df))

# Get the name and the y position of each label
label_data <- trust_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# Make the plot
ggplot(trust_df, aes(x=as.factor(id), y=level_trst)) +       
  geom_bar(stat="identity", fill=alpha("#87CEFA", 0.3)) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar(start = 0) + 
  geom_text(data=label_data, aes(x=id, y=level_trst+10, label=trust_for, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

# Now we add spaces between groups

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame( matrix(NA, empty_bar*nlevels(trust_df$cntry), ncol(trust_df)) )
View(to_add)
colnames(to_add) <- colnames(trust_df)
to_add$cntry <- rep(levels(trust_df$cntry), each=empty_bar)
View(to_add)
trust_df <- rbind(trust_df, to_add)
trust_df <- trust_df %>% arrange(cntry)
trust_df$id <- seq(1, nrow(trust_df))

View(trust_df)

# Get the name and the y position of each label
label_data <- trust_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- trust_df %>% 
  group_by(cntry) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))


# Make the plot
ggplot(trust_df, aes(x= as.factor(id), y=level_trst, fill=cntry)) +  
  geom_bar(stat="identity", alpha=0.6) +
  ylim(-20,20) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=level_trst +1, label = trust_for, hjust=hjust), color="black",alpha=0.8, size=2, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -8, label=cntry), hjust=c(rep(0.4,14)), colour = "black", alpha=1, size=4, inherit.aes = FALSE)



#Internet use and media exposure to politics in different countries
mediaexp_df <- full_dataset %>%
  select(cntry, netustm, nwspol) %>%
  group_by(cntry) %>%
  summarize(internet = mean(netustm), tv_radio_newspaper = mean(nwspol))

mediaexp_df$cntry <- as.factor(mediaexp_df$cntry)

mediaexp_df <- mediaexp_df %>%
  gather("channel", "avg_use", 2:3)

View(mediaexp_df)
col_palette = c("#87CEFA", "#104E8B")

ggplot(mediaexp_df, aes(x= cntry, y=avg_use, fill=channel)) +  
  geom_bar(stat="identity") +
  ylim(0,400) +
  scale_fill_manual(values= col_palette) +
  ggtitle("Internet use and media exposure to politics in different countries") +
  theme_classic() +
  ylab("Average use (minutes per day)") +
  xlab("Country")



# Evolution of trust overtime

# We prepare our dataframe


#we create the variable year, which will help us identify the time period the survey covers. The reason behind is that the already existing variable proddate is not accurate enough and refers to the period the survey is released but not the period considered
# For our new variable, we will consider the year in which the survey was ongoing, which means round 8 - 2017, round 9 - 2019, round 10 - 2021

full_dataset <- full_dataset %>%
  mutate(year = case_when(essround == 8 ~ "2017",
                          essround == 9 ~ "2019",
                          essround == 10 ~ "2021"))

full_dataset$year <- as.factor(full_dataset$year)

trust_df <- full_dataset %>%
  select(cntry, year, trstep, trstlgl, trstplc, trstplt, trstprl, trstprt, trstun) %>%
  group_by(year) %>%
  summarize(EU = mean(trstep), legal_system = mean(trstlgl), police = mean(trstplc), politicians = mean(trstplt), parliament = mean(trstprl), political_parties = mean(trstprt), united_nations = mean(trstun))


View(trust_df)
trust_df <- trust_df %>%
  gather("trust_for", "level_trst", 2:8)
View(trust_df)

trust_df$level_trst <- as.numeric(trust_df$level_trst)
label_data$trust_for <- as.factor(label_data$trust_for)

View(trust_df)

ggplot(data = trust_df, aes(x = year, y = level_trst, group = trust_for, color = trust_for)) +
  geom_line() + geom_point() +
  ggtitle("Evolution of trust from 2017 to 2021") +
  theme_ipsum() +
  ylab("Average trust")


#Evolution of media exposure over time 
mediaexp_df <- full_dataset %>%
  select(cntry, year, netustm, nwspol) %>%
  group_by(year) %>%
  summarize(internet = mean(netustm), tv_radio_newspaper = mean(nwspol))


View(mediaexp_df)
mediaexp_df <- mediaexp_df %>%
  gather("channel", "avg_use", 2:3)
View(mediaexp_df)

ggplot(data = mediaexp_df, aes(x = year, y = avg_use, group = channel, color = channel)) +
  geom_line() + geom_point() +
  ggtitle("Evolution of media exposure overtime") +
  theme_ipsum() +
  ylab("Average use (minutes per day)")


#Corr-Plot
# create a matrix of data
data_cor <- full_dataset[, c(7:50)]
data_cor
# calculate the correlation matrix
corr_matrix <- cor(data_cor)

corrplot(corr_matrix, order="hclust",
         addrect=4,
         tl.cex = 0.7,
         tl.col = "black")

corrplot

########Chloropleth Map#########

library(dplyr, warn.conflicts = FALSE)

# EDIT YOUR WORKING DIRECTORY
europe_map <- st_read("Data/NUTS_RG_20M_2021_3035.shp/NUTS_RG_20M_2021_3035.shp")

glimpse(europe_map)
europe_map$CNTR_CODE
europe_map$NUTS_NAME
europe_map$NAME_LATN

europe_map <- europe_map |>
  rename(cntry=CNTR_CODE)

dataset_trstprl <- full_dataset[, c(6,37)]

outcome_trstprl <- full_join(europe_map, dataset_trstprl, by= "cntry")

colors=c("#87CEFA", "#00BFFF", "#56B4E9", "#1C86EE", "#1874CD", "#104E8B","#00008B")

map_outcome_trstprl <- tm_shape(outcome_trstprl) +
  tm_fill(col = "trstprl", palette = "Greens", n = 8,
          title = "The Level of Trust in Government", style = "cont") +
  tm_borders(col = "white", lwd = 0.01) +
  tm_compass(type = "arrow", position = c("right", "bottom"), size = 2) +
  tm_scale_bar(text.size = 0.5, position = c("center", "bottom")) +
  tm_layout(title.size = 2.5, legend.text.size = 0.8, legend.position = c("left", "bottom"))  # Adjusted legend position

map_outcome_trstprl


#####Modified 2#####
tmap_save(map_outcome_trstprl, filename = "outcome_trstprl_map.png", width = 1500, height = 1500, dpi = 300)


