library(tidyverse)
library(readr)
library(ggrepel)
library("readxl")
library(dplyr)
#install.packages("janitor")
library(janitor)
library(reshape2)
library("openxlsx")

# Add the continents
Data <- read.csv("C:/Users/eyesi/Desktop/Data Visualisation/data/wthcontinent.csv", header=TRUE)
Data<-Data[!(Data$code_3==""),]
Data<-Data[!(Data$continent==""),]

#Sub Data
Datasub<-subset(Data,select=c(continent, year, fossil_fuel_consumption, low_carbon_consumption))
View(Datasub)

#Wide to Long Format 
Datasub<-rename(Datasub, coal=coal_consumption, nuclear=nuclear_consumption,renewable=renewables_consumption)
Datasub_long<-melt(Datasub, id=c("continent","year"), value.name="consumption", variable.name ="energy_type")
View(Datasub_long)
write.csv(Datasub_long, file="Sub_WthContinent_long.csv")

group<-group_by(Datasub_long, continent, year, energy_type)
meansumry<-summarise(group, meanconsmp=mean(consumption,na.rm=TRUE))
as.data.frame(meansumry)
str(meansumry)
meansumry<-meansumry[!(is.na(meansumry$year)),]
write.csv(meansumry, file="Sub-Meansumry.csv")
View(meansumry)
sum(is.na(meansumry$year))
sum(is.na(meansumry$meanconsmp))
meansumry$continent<-as.factor(meansumry$continent)
levels(meansumry$continent)


