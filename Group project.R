#load packages
library(data.table)
library(dplyr)
library(tidyverse)

###  ROUND 10 (2020 - 2022)  ###

round_10 <- read.csv("ESS10.csv")

#### filter for the countries which adhered to the survey for the time span of interest ###


# We chose the variables of interest to create a subset of the country data:

sub_final_10 <- round_10 %>%
  select(essround, idno, cntry, nwspol, netusoft, netustm, ppltrst, pplfair, polintr, psppsgva, actrolga, psppipla, cptppola, trstprl,
          trstlgl, trstplc, trstplt, trstprt , trstep, trstun, vote, lrscale,stflife, stfeco, 
          stfgov, stfdem, stfedu, stfhlth, gincdif, freehms,hmsfmlsh, hmsacld, euftf, impcntr, 
          imbgeco, imueclt, imwbcnt, happy, sclmeet, inprdsc, sclact, health, atchctr,
          rlgblg, rlgdnm, rlgdgr, dscrgrp, dscrrce, dscrntn, dscrrlg, dscrlng, 
          dscretn, dscrage, dscrgnd, dscrsex, dscrdsb, dscroth, dscrdk, dscrref, 
          dscrnap, dscrna, ctzcntr, brncntr, hhmmb, gndr, agea, marsts, 
          edulvlb, hincsrca, hinctnta, hincfel, imprich, ipeqopt, ipshabt, impsafe, ipfrule, 
          ipudrst, impfree, iphlppl, ipsuces, ipstrgv, ipbhprp, iprspot, impenv, imptrad)

#NOTE: in round 10 we have some questions regarding climate change and understanding of democracy, what do we want todo with them?

# We deleted all the "Refusal", "Don't know" or "No answer" values:

sub_final_10 <- subset(sub_final_10, 
                      !ppltrst %in% c(77,88,99)
                      & !pplfair %in% c(77,88,99)
                      & !nwspol %in% c(7777,8888,9999)
                      & !netusoft %in% c(7,8,9)
                      & !netustm %in% c(6666, 7777, 8888, 9999)
                      & !polintr %in% c(7,8,9)
                      & !psppsgva %in% c(7,8,9)
                      & !actrolga %in% c(7,8,9)
                      & !psppipla %in% c(7,8,9)
                      & !cptppola %in% c(7,8,9)
                      & !trstprl %in% c(77,88,99)
                      & !trstlgl %in% c(77,88,99)
                      & !trstplc %in% c(77,88,99)
                      & !trstplt %in% c(77,88,99)
                      & !trstprt %in% c(77,88,99)
                      & !trstep %in% c(77,88,99)
                      & !trstun %in% c(77,88,99)
                      & !vote %in% c(7,8,9)
                      & !lrscale %in% c(77,88,99)
                      & !stflife %in% c(77,88,99)
                      & !stfeco %in% c(77,88,99)
                      & !stfgov %in% c(77,88,99)
                      & !stfdem %in% c(77,88,99)
                      & !stfedu %in% c(77,88,99)
                      & !stfhlth %in% c(77,88,99)
                      & !gincdif %in% c(7,8,9)
                      & !freehms %in% c(7,8,9)
                      & !euftf %in% c(77,88,99)
                      & !impcntr %in% c(7,8,9)
                      & !imbgeco %in% c(77,88,99)
                      & !imueclt %in% c(77,88,99)
                      & !imwbcnt %in% c(77,88,99)
                      & !happy %in% c(77,88,99)
                      & !sclmeet %in% c(77,88,99)
                      & !inprdsc %in% c(77,88,99)
                      & !sclact%in% c(7,8,9)
                      & !health %in% c(7,8,9)
                      & !atchctr %in% c(77,88,99)
                      & !rlgblg %in% c(7,8,9)
                      & !rlgdnm %in% c(66,77,88,99)
                      & !rlgdgr %in% c(66,77,88,99)
                      & !dscrgrp %in% c(7,8,9)
                      & !ctzcntr %in% c(7,8,9)
                      & !brncntr %in% c(7,8,9)
                      & !hhmmb %in% c(77,88,99)
                      & !gndr %in% c(9)
                      & !agea %in% c(999)
                      & !marsts %in% c(66,77,88,99)
                      & !edulvlb %in% c(5555,7777,8888,9999)
                      & !hincsrca %in% c(77,88,99)
                      & !hinctnta %in% c(77,88,99)
                      & !hincfel %in% c(7,8,9)
                      & !imprich %in% c(7,8,9)
                      & !ipeqopt %in% c(7,8,9)
                      & !ipshabt %in% c(7,8,9)
                      & !impsafe %in% c(7,8,9)
                      & !ipfrule %in% c(7,8,9)
                      & !ipudrst %in% c(7,8,9)
                      & !impfree %in% c(7,8,9)
                      & !iphlppl %in% c(7,8,9)
                      & !ipsuces %in% c(7,8,9)
                      & !ipstrgv %in% c(7,8,9)
                      & !ipbhprp %in% c(7,8,9)
                      & !iprspot %in% c(7,8,9)
                      & !impenv %in% c(7,8,9)
                      & !imptrad %in% c(7,8,9))


# The numeric variable nwspol indicated the minutes spent watching political news.
# In other years, the variable tvpol indicated the same thing, however, using categories depending on the time span spent watching political news.
# To measure this variable on the same scale, we made a categorical variable of the variable nwspol, assigning its values to the categories of the tvpol variable:


#sub_final_10 <- sub_final_10 %>%
#mutate(nwspol = case_when(nwspol > 180 ~ 7,
                            #nwspol > 150 & nwspol <= 180 ~ 6,
                            #nwspol > 120 & nwspol <=150 ~ 5,
                            #nwspol > 90 & nwspol <= 120 ~ 4,
                            #nwspol > 60 & nwspol <= 90 ~ 3,
                            #nwspol > 30 & nwspol <= 60 ~ 2,
                            #nwspol <= 30 ~ 1,
                            #nwspol == 0 ~ 0,
                            #TRUE ~ "NA"))

# We eliminated the NA values from the new variable:
#sub_final_10 <- subset(sub_final_10, !nwspol %in% c("NA"))






###  ROUND 9 (2018 - 2020)  ###

#load data
round_9 <- read.csv("ESS9e03_1.csv")

#### filter for the countries  ###



# Selection of the  variables of interest 

sub_final_9 <- round_9 %>%
  select (essround, idno, cntry, nwspol, netusoft, netustm, ppltrst, pplfair, polintr, psppsgva, actrolga, psppipla, cptppola, trstprl,
          trstlgl, trstplc, trstplt, trstprt , trstep, trstun, vote, lrscale,stflife, stfeco, 
          stfgov, stfdem, stfedu, stfhlth, gincdif, freehms,hmsfmlsh, hmsacld,euftf, impcntr, 
          imbgeco, imueclt, imwbcnt, happy, sclmeet, inprdsc, sclact, health, atchctr,
          rlgblg, rlgdnm, rlgdgr, dscrgrp, dscrrce, dscrntn, dscrrlg, dscrlng, 
          dscretn, dscrage, dscrgnd, dscrsex, dscrdsb, dscroth, dscrdk, dscrref, 
          dscrnap, dscrna, ctzcntr, brncntr, hhmmb, gndr, agea, marsts, 
          edulvlb, hincsrca, hinctnta, hincfel, imprich, ipeqopt, ipshabt, impsafe, ipfrule, 
          ipudrst, impfree, iphlppl, ipsuces, ipstrgv, ipbhprp, iprspot, impenv, imptrad)


# Removal of all the "Refusal", "Don't know" or "No answer" values:

sub_final_9 <- subset(sub_final_9, 
                      !ppltrst %in% c(77,88,99)
                      & !pplfair %in% c(77,88,99)
                      & !nwspol %in% c(7777,8888,9999)
                      & !netusoft %in% c(7,8,9)
                      & !netustm %in% c(6666, 7777, 8888, 9999)
                      & !polintr %in% c(7,8,9)
                      & !psppsgva %in% c(7,8,9)
                      & !actrolga %in% c(7,8,9)
                      & !psppipla %in% c(7,8,9)
                      & !cptppola %in% c(7,8,9)
                      & !trstprl %in% c(77,88,99)
                      & !trstlgl %in% c(77,88,99)
                      & !trstplc %in% c(77,88,99)
                      & !trstplt %in% c(77,88,99)
                      & !trstprt %in% c(77,88,99)
                      & !trstep %in% c(77,88,99)
                      & !trstun %in% c(77,88,99)
                      & !vote %in% c(7,8,9)
                      & !lrscale %in% c(77,88,99)
                      & !stflife %in% c(77,88,99)
                      & !stfeco %in% c(77,88,99)
                      & !stfgov %in% c(77,88,99)
                      & !stfdem %in% c(77,88,99)
                      & !stfedu %in% c(77,88,99)
                      & !stfhlth %in% c(77,88,99)
                      & !gincdif %in% c(7,8,9)
                      & !freehms %in% c(7,8,9)
                      & !euftf %in% c(77,88,99)
                      & !impcntr %in% c(7,8,9)
                      & !imbgeco %in% c(77,88,99)
                      & !imueclt %in% c(77,88,99)
                      & !imwbcnt %in% c(77,88,99)
                      & !happy %in% c(77,88,99)
                      & !sclmeet %in% c(77,88,99)
                      & !inprdsc %in% c(77,88,99)
                      & !sclact%in% c(7,8,9)
                      & !health %in% c(7,8,9)
                      & !atchctr %in% c(77,88,99)
                      & !rlgblg %in% c(7,8,9)
                      & !rlgdnm %in% c(66,77,88,99)
                      & !rlgdgr %in% c(66,77,88,99)
                      & !dscrgrp %in% c(7,8,9)
                      & !ctzcntr %in% c(7,8,9)
                      & !brncntr %in% c(7,8,9)
                      & !hhmmb %in% c(77,88,99)
                      & !gndr %in% c(9)
                      & !agea %in% c(999)
                      & !marsts %in% c(66,77,88,99)
                      & !edulvlb %in% c(5555,7777,8888,9999)
                      & !hincsrca %in% c(77,88,99)
                      & !hinctnta %in% c(77,88,99)
                      & !hincfel %in% c(7,8,9)
                      & !imprich %in% c(7,8,9)
                      & !ipeqopt %in% c(7,8,9)
                      & !ipshabt %in% c(7,8,9)
                      & !impsafe %in% c(7,8,9)
                      & !ipfrule %in% c(7,8,9)
                      & !ipudrst %in% c(7,8,9)
                      & !impfree %in% c(7,8,9)
                      & !iphlppl %in% c(7,8,9)
                      & !ipsuces %in% c(7,8,9)
                      & !ipstrgv %in% c(7,8,9)
                      & !ipbhprp %in% c(7,8,9)
                      & !iprspot %in% c(7,8,9)
                      & !impenv %in% c(7,8,9)
                      & !imptrad %in% c(7,8,9))


###  ROUND 8 (2016 - 2018)  ###

round_8 <- read.csv("ESS8e02_2.csv")

#### filter for the countries which adhered to the survey for the time span of interest ###


sub_final_8 <- round_8 %>%
  select(essround, idno, cntry, nwspol, netusoft, netustm, ppltrst, pplfair, polintr, psppsgva, actrolga, psppipla, cptppola, trstprl,
         trstlgl, trstplc, trstplt, trstprt , trstep, trstun, vote, lrscale,
         stflife, stfeco, stfgov, stfdem, stfedu, stfhlth, gincdif, freehms, hmsfmlsh, hmsacld,
         euftf, impcntr, imbgeco, imueclt, imwbcnt, happy,
         sclmeet, inprdsc, sclact, health, atchctr, rlgblg, rlgdnm,
         rlgdgr, dscrgrp, dscrrce, dscrntn, dscrrlg, dscrlng, dscretn, dscrage,
         dscrgnd, dscrsex, dscrdsb, dscroth, dscrdk, dscrref, dscrnap, dscrna,
         ctzcntr, brncntr, hhmmb,
         gndr, agea, marsts, edulvlb, hincsrca, hinctnta, hincfel, imprich, ipeqopt,
         ipshabt, impsafe, ipfrule, ipudrst, impfree, iphlppl, ipsuces,
         ipstrgv, ipbhprp, iprspot, impenv, imptrad)



sub_final_8 <- subset(sub_final_8, !ppltrst %in% c(77,88,99)
                      & !pplfair %in% c(77,88,99)
                      & !nwspol %in% c(7777,8888,9999)
                      & !netusoft %in% c(7,8,9)
                      & !netustm %in% c(6666, 7777, 8888, 9999)
                      & !polintr %in% c(7,8,9)
                      & !psppsgva %in% c(7,8,9)
                      & !actrolga %in% c(7,8,9)
                      & !psppipla %in% c(7,8,9)
                      & !cptppola %in% c(7,8,9)
                      & !trstprl %in% c(77,88,99)
                      & !trstlgl %in% c(77,88,99)
                      & !trstplc %in% c(77,88,99)
                      & !trstplt %in% c(77,88,99)
                      & !trstprt %in% c(77,88,99)
                      & !trstep %in% c(77,88,99)
                      & !trstun %in% c(77,88,99)
                      & !vote %in% c(7,8,9)
                      & !lrscale %in% c(77,88,99)
                      & !stflife %in% c(77,88,99)
                      & !stfeco %in% c(77,88,99)
                      & !stfgov %in% c(77,88,99)
                      & !stfdem %in% c(77,88,99)
                      & !stfedu %in% c(77,88,99)
                      & !stfhlth %in% c(77,88,99)
                      & !gincdif %in% c(7,8,9)
                      & !freehms %in% c(7,8,9)
                      & !euftf %in% c(77,88,99)
                      & !impcntr %in% c(7,8,9)
                      & !imbgeco %in% c(77,88,99)
                      & !imueclt %in% c(77,88,99)
                      & !imwbcnt %in% c(77,88,99)
                      & !happy %in% c(77,88,99)
                      & !sclmeet %in% c(77,88,99)
                      & !inprdsc %in% c(77,88,99)
                      & !sclact%in% c(7,8,9)
                      & !health %in% c(7,8,9)
                      & !atchctr %in% c(77,88,99)
                      & !rlgblg %in% c(7,8,9)
                      & !rlgdnm %in% c(66,77,88,99)
                      & !rlgdgr %in% c(66,77,88,99)
                      & !dscrgrp %in% c(7,8,9)
                      & !ctzcntr %in% c(7,8,9)
                      & !brncntr %in% c(7,8,9)
                      & !hhmmb %in% c(77,88,99)
                      & !gndr %in% c(9)
                      & !agea %in% c(999)
                      & !marsts %in% c(66,77,88,99)
                      & !edulvlb %in% c(5555,7777,8888,9999)
                      & !hincsrca %in% c(77,88,99)
                      & !hinctnta %in% c(77,88,99)
                      & !hincfel %in% c(7,8,9)
                      & !imprich %in% c(7,8,9)
                      & !ipeqopt %in% c(7,8,9)
                      & !ipshabt %in% c(7,8,9)
                      & !impsafe %in% c(7,8,9)
                      & !ipfrule %in% c(7,8,9)
                      & !ipudrst %in% c(7,8,9)
                      & !impfree %in% c(7,8,9)
                      & !iphlppl %in% c(7,8,9)
                      & !ipsuces %in% c(7,8,9)
                      & !ipstrgv %in% c(7,8,9)
                      & !ipbhprp %in% c(7,8,9)
                      & !iprspot %in% c(7,8,9)
                      & !impenv %in% c(7,8,9)
                      & !imptrad %in% c(7,8,9))




# We finally checked if we missed some weird values, and eventually eliminate them
summary(sub_final_10)
summary(sub_final_9)
summary(sub_final_8)


# We MERGED the data sets 

merged_data <- rbind(sub_final_10, sub_final_9, sub_final_8)

merged_data <- data.frame(merged_data)


# For easier understanding, we created a new variable for the education level

merged_data <- merged_data %>%
  mutate(education = case_when(edulvlb >= 510 ~ "high",
                               edulvlb >= 311 & edulvlb < 510 ~ "medium",
                               TRUE ~ "low"))

# We created a new categorical variable to define the belonging of people to different age ranges


merged_data <- merged_data %>%
  mutate(age_range = case_when(agea >= 70 ~ ">=70",
                               agea < 70 & agea >= 60 ~ "60-69",
                               agea < 60 & agea >= 50 ~ "50-59",
                               agea < 50 & agea >= 40 ~ "40-49",
                               agea < 40 & agea >= 30 ~ "30-39",
                               agea < 30 & agea >= 18 ~ "18-29",
                               TRUE ~ "<18"))



# Before proceeding, we want R to recognize the variables age_range, edulvlb as factors. We will directly write over the two variables.
# The reason behind is that is easier to work with factor variables rather than character variables
# Moreover, for some of these variables the order of the different levels actually matters to us.

merged_data$age_range <- as.factor(merged_data$age_range)
merged_data$education <- as.factor(merged_data$education)

# We make sure that the two variables are now recognized as factors, using the class() function
class(merged_data$age_range)
class(merged_data$education)

# Now, what we have to do is to display the levels and eventually set the order we want them to be

levels(merged_data$age_range)
levels(merged_data$education)

# As we can see, the levels are not in the right order. To change them, we rewrite over the variables and set the order manually

merged_data$age_range <- factor((merged_data$age_range), levels = c("18-29", "30-39","40-49", "50-59","60-69",">=70"))
merged_data$education <- factor((merged_data$education), levels = c("low", "medium", "high"))

# We check that everything is good
levels(merged_data$age_range)
levels(merged_data$education)

# Finally, we change the class of a set of variables which are categorized as numeric on range from 0-1, but they should be dummy (categorical)
merged_data$vote <- as.factor(merged_data$vote)
merged_data$dscrgrp <- as.factor(merged_data$dscrgrp)
merged_data$dscrrce <- as.factor(merged_data$dscrrce)
merged_data$dscrntn <- as.factor(merged_data$dscrntn)
merged_data$dscrrlg <- as.factor(merged_data$dscrrlg)
merged_data$dscrlng <- as.factor(merged_data$dscrlng)
merged_data$dscretn <- as.factor(merged_data$dscretn)
merged_data$dscrage <- as.factor(merged_data$dscrage)
merged_data$dscrgnd <- as.factor(merged_data$dscrgnd)
merged_data$dscrsex <- as.factor(merged_data$dscrsex)
merged_data$dscrdsb <- as.factor(merged_data$dscrdsb)
merged_data$dscroth <- as.factor(merged_data$dscroth)
merged_data$dscrdk <- as.factor(merged_data$dscrdk)
merged_data$dscrref <- as.factor(merged_data$dscrref)
merged_data$dscrnap <- as.factor(merged_data$dscrnap)
merged_data$dscrna <- as.factor(merged_data$dscrna)
merged_data$ctzcntr <- as.factor(merged_data$ctzcntr)
merged_data$brncntr <- as.factor(merged_data$brncntr)
merged_data$gndr <- as.factor(merged_data$gndr)
merged_data$rlgblg <- as.factor(merged_data$rlgblg)
merged_data$rlgdnm <- as.factor(merged_data$rlgdnm)

# Also, essround, id. number and party voted should be categorical and not numerical
merged_data$essround <- as.factor(merged_data$essround)
merged_data$idno <- as.factor(merged_data$idno)


# Final check
summary(merged_data)


