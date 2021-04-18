##########################################
#  Cansu Kebabci-Wealth indexes added    #  
##########################################
library(plyr)
library(tidyverse)
library(readr)
library(ggrepel)
library("readxl")
library(dplyr)
#install.packages("janitor")
library(janitor)
library(reshape2)
library("openxlsx")

setwd("C:/Users/eyesi/Desktop/Data Visualisation/data")
data <- read_excel("owid-energy-data_new.xlsx")
View(data)
data$gdp.billion <- data$gdp/10^9
names(data)

#To see in which countries are lacking in iso_code 
data_group<-group_by(data,country)
nmiss_country<-summarise(data_group, nmiss=sum(is.na(iso_code)))
nmiss_filt<-filter(nmiss_country, nmiss!=0)
View(nmiss_filt)
#Actually those are continents...recorded in data beside of the countries 
#Remove them
data<-data[!(data$iso_code==""),]

# Add the continents
countryContinent <- read.csv("C:/Users/eyesi/Desktop/Data Visualisation/dataviz_energy-main/countryContinent.csv", header=TRUE)
countryContinent <- countryContinent %>% select(code_3,continent,sub_region)
View(countryContinent)

#Continents+Original data
Data <- full_join(countryContinent, data, by=c("code_3"="iso_code"))
View(Data)
Data[is.na(Data)]<-""
write.xlsx(Data, file="wthcontinent.xlsx")
names(Data)
Data$contnew<-as.factor(Data$continent)
levels(Data$contnew)

#Checking misingness in primary consumption data 
data_group2<-group_by(data,year)
year_consmp<-select(data_group2,primary_energy_consumption,year)
View(year_consmp)
nmiss_consmp<-summarise(data_group2, sum(is.na(primary_energy_consumption)))
nmiss_filt<-filter(nmiss_consmp, nmiss!=0)
View(nmiss_filt)
#There is no missing data of primary_energy consumption for any taken year-country 

#Adding Gini Index 
Gini_World <- read_excel("WID_Data_Metadata/Gini_World.xlsx")
View(Gini_World)
Gini_World<-remove_empty(Gini_World, which = c("rows","cols"), quiet = FALSE)
Gini_World <-Gini_World %>% select(-Percentile)
View(Gini_World)
#Wide to Long Format 
Gini_World_long<-melt(Gini_World, id="Year", value.name="Gini_indx", variable.name ="country")
View(Gini_World_long)
#Missing Data 
gini_group<-group_by(Gini_World_long,Year)
gini_group
nmiss_gini<-summarise(gini_group, nmiss_gini=sum(is.na(Gini_indx)))
nmiss_filt<-filter(nmiss_gini, nmiss_gini!=0)
View(nmiss_filt)
#In total there are 210 countries and before 1980 about 190 of countries do not have any input for gini index 
#by year -country grouping 
gini_group<-group_by(Gini_World_long,Year, country)
gini_group
nmiss_gini<-summarise(gini_group, nmiss_gini=sum(is.na(Gini_indx)))
nmiss_filt<-filter(nmiss_gini, nmiss_gini!=0)
View(nmiss_filt)
#Apperently before 1980 there is no considerable data 

#Combining Gini_index data + Continentdata
Gini_World_long2<-arrange(Gini_World_long, country,Year)
View(Gini_World_long2)
Final1 <- full_join(Gini_World_long, Data, by=c("country"="country", "Year"="year"))
View(Final1)
#removal of the country=world? data 
Final1<-Final1[!Final1$country=="World",]
write.xlsx(Final1, file="Final1.xlsx")

#Adding HDI index 
HDI_World <- read_excel("C:/Users/eyesi/Desktop/Data Visualisation/data/HDI.xlsx")
View(HDI_World)
HDI_World <-HDI_World %>% select(-"HDI Rank")
names(HDI_World)

#Long Format of HDI_World
HDI_World_long<-melt(HDI_World, id="Country", value.name="HDI_indx", variable.name ="Year")
View(HDI_World_long)
#It starts from 1990 
#Considerig the issue in Gini data and HDI, the graph should be plotted for 1990 onwards

#Combining HDI_index + Continentdata 
Final1990onw<-subset(Final1, Year>=1990)
View(Final1990onw)
HDI_World_long$Year<-as.factor(HDI_World_long$Year)
Final1990onw$Year<-as.factor(Final1990onw$Year)
HDI_World_long<-arrange(HDI_World_long, Country)
Final2<-full_join(HDI_World_long, Final1990onw, by=c("Country"="country", "Year"="Year"))
write.csv(Final2, file="Final2_.csv")

Final2[is.na(Final2)]<-""
write.csv(Final2, file="Final2.csv")

