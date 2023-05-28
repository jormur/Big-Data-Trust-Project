#load packages
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggeasy)
library(plotly)
library(leaflet)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(maps)
library(mapproj)

### IMPORTING DATA TO R ########

setwd("/Users/maryceciarelli/Desktop/Big Data for Social Analysis/Group project")

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

full_dataset$pstplonl <- as.factor(full_dataset$pstplonl)
full_dataset$sgnptit <- as.factor(full_dataset$sgnptit)
full_dataset$vote <- as.factor(full_dataset$vote)
full_dataset$brncntr <- as.factor(full_dataset$brncntr)
full_dataset$crmvct <- as.factor(full_dataset$crmvct)
full_dataset$ctzcntr <- as.factor(full_dataset$ctzcntr)
full_dataset$dscrage <- as.factor(full_dataset$dscrage)
full_dataset$dscrdsb <- as.factor(full_dataset$dscrdsb)
full_dataset$dscretn <- as.factor(full_dataset$dscretn)
full_dataset$dscrgnd <- as.factor(full_dataset$dscrgnd)
full_dataset$dscrgrp <- as.factor(full_dataset$dscrgrp)
full_dataset$dscrlng <- as.factor(full_dataset$dscrlng)
full_dataset$dscrntn <- as.factor(full_dataset$dscrntn)
full_dataset$dscrrce <- as.factor(full_dataset$dscrrce)
full_dataset$dscrrlg <- as.factor(full_dataset$dscrrlg)
full_dataset$dscrsex <- as.factor(full_dataset$dscrsex)
full_dataset$gndr <- as.factor(full_dataset$gndr)


full_dataset$trstep<-as.numeric(full_dataset$trstep)
full_dataset$trstlgl<-as.numeric(full_dataset$trstlgl)
full_dataset$trstplc<-as.numeric(full_dataset$trstplc)
full_dataset$trstplt<-as.numeric(full_dataset$trstplt)
full_dataset$trstprl<-as.numeric(full_dataset$trstprl)
full_dataset$trstprt<-as.numeric(full_dataset$trstprt)
full_dataset$trstun<-as.numeric(full_dataset$trstun)

full_dataset$cntry <- as.factor(full_dataset$cntry)

summary(full_dataset)

#Finally, we can create other variables, for example agerange and year

full_dataset <- full_dataset %>%
  mutate(agerange = case_when(agea >= 70 ~ ">=70",
                               agea < 70 & agea >= 60 ~ "60-69",
                               agea < 60 & agea >= 50 ~ "50-59",
                               agea < 50 & agea >= 40 ~ "40-49",
                               agea < 40 & agea >= 30 ~ "30-39",
                               agea < 30 & agea >= 18 ~ "18-29",
                               TRUE ~ "<18"))


# We want R to recognize the new variable agerange as factor. We will directly write over the variable.
# The reason behind is that is easier to work with factor variables rather than character variables
# Moreover, for some of these variables the order of the different levels actually matters to us.
full_dataset$agerange <- as.factor(full_dataset$agerange)


# We make sure that the variable is now recognized as factor, using the class() function
class(full_dataset$agerange)


# Now, what we have to do is to display the levels and eventually set the order we want them to be
levels(full_dataset$agerange)

# As we can see, the levels are not in the right order. To change them, we rewrite over the variables and set the order manually
full_dataset$agerange <- factor((full_dataset$agerange), levels = c("<18","18-29", "30-39","40-49", "50-59","60-69",">=70"))


# We check that everything is good
levels(full_dataset$agerange)

#Now we create the variable year, which will help us identify the time period the survey covers. The reason behind is that the already existing variable proddate is not accurate enough and refers to the period the survey is released but not the period considered
# For our new variable, we will consider the year in which the survey was ongoing, which means round 8 - 2017, round 9 - 2019, round 10 - 2021

full_dataset <- full_dataset %>%
  mutate(year = case_when(essround == 8 ~ "2017",
                          essround == 9 ~ "2019",
                          essround == 10 ~ "2021"))


full_dataset$year <- as.Date(full_dataset$year, "%Y")
class(full_dataset$year)

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
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank())


# At this point we also want to visualize the real population of each country and compare it with our observations. This will help us to understand if our sample is balanced 
# Population is displayed in millions and refers to 2021

population <- c(8.703,10.51,1.331,5.541,67.75,9.71,0.372520,59.11,2.801,17.53,5.408,10.33,2.108)

map_df <- map_df %>%
  mutate(population = population)

ggplot() +
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group), fill="#87CEFA", alpha=0.7) +
  geom_point( data = map_df, aes(x=longitude, y=latitude, size=population,),  color="mediumblue",  alpha=0.6) +
  scale_size_continuous(range=c(4, 18)) +
  geom_text (data = map_df, label = countrynames, aes(x=long, y=lat), hjust=0.5, vjust=-2.7, size=3) +
  theme_minimal() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.title = element_blank())

#Gender distribution overall - pie chart
gender_ov_count <- table(full_dataset$gndr)
male_percent = round((gender_ov_count[1]/(gender_ov_count[1]+gender_ov_count[2]))*100, digits = 2)
female_percent = round((gender_ov_count[2]/(gender_ov_count[1]+gender_ov_count[2]))*100, digits = 2)
new_labels = c(paste('Male - ', male_percent, '%', sep = ''), paste('Female - ', female_percent, '%', sep = ''))
pie(gender_ov_count, labels = new_labels, main = 'Distribution of gender overall', col=c("#00BFFF", "#104E8B"), border = "white")



#Age distribution overall - bar plot

age_range_overall_count <- table(full_dataset$agerange)
barplot(age_range_overall_count, beside = T, main = 'Distribution of age', xlab = 'Age Range', 
        ylab = 'Count', ylim = c(0,2000), col=c("#87CEFA", "#00BFFF", "#56B4E9", "#1C86EE", "#1874CD", "#104E8B","#00008B"), border = "white")


#Age distribution overall - pie chart
prop_age_range <- prop.table(age_range_overall_count)
df_age_range <- as.data.frame(prop_age_range)

View(df_age_range)

lbs <- round(prop_age_range/sum(prop_age_range)*100, digits = 1)
lbs <- c(paste("<18", "-", lbs[1], "%"), paste("18-20", "-", lbs[2], "%"), paste("30-39", "-", lbs[3], "%"), paste("40-49", "-", lbs[4], "%"), paste("50-59", "-", lbs[5], "%"), paste("60-69", "-", lbs[6], "%"), paste(">=70", "-", lbs[7], "%"))
lbs
pie(prop_age_range, labels = lbs, main ='Distribution of age overall',
    col=c("#87CEFA", "#00BFFF", "#56B4E9", "#1C86EE", "#1874CD", "#104E8B","#00008B"), border = "white")   ######### TO IMPROVE ########


#Average level trust - circular bar plot

# Let's first create our dataframe

trust_df <- full_dataset %>%
  select(cntry, trstep, trstlgl, trstplc, trstplt, trstprl, trstprt, trstun) %>%
  group_by(cntry) %>%
  summarize(EU = mean(trstep), legal_system = mean(trstlgl), politicians = mean(trstplt), parliament = mean(trstprt), party = mean(trstprt), united_nations = mean(trstun))

# Now we have to reshape the dataframe

trust_df <- trust_df %>%
  gather("trust_for", "level_trst", 2:7)

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


# Make the plot
ggplot(trust_df, aes(x= as.factor(id), y=level_trst, fill=cntry)) +  
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-20,30) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=level_trst+10, label = trust_for, hjust=hjust), color="black", fontface="bold",alpha=0.5, size=3, angle= label_data$angle, inherit.aes = FALSE )
### TO IMPROVE #####


# Evolution of trust overtime

# We prepare our dataframe
install.packages("viridis")
install.packages("viridisLite")
install.packages("hrbrthemes")

library(viridis)
library(hrbrthemes)

trust_df <- full_dataset %>%
  select(cntry, year, trstep, trstlgl, trstplc, trstplt, trstprl, trstprt, trstun) %>%
  group_by(year) %>%
  summarize(EU = mean(trstep), legal_system = mean(trstlgl), politicians = mean(trstplt), parliament = mean(trstprt), party = mean(trstprt), united_nations = mean(trstun))


View(trust_df)
trust_df <- trust_df %>%
  gather("trust_for", "level_trst", 2:7)
View(trust_df)

trust_df$level_trst <- as.numeric(trust_df$level_trst)
label_data$trust_for <- as.factor(label_data$trust_for)

View(trust_df)

ggplot(data = trust_df, aes(x = year, y = level_trst, group = trust_for, color = trust_for)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Evolution of trust from 2016 to 2022") +
  theme_ipsum() +
  ylab("Average trust")
