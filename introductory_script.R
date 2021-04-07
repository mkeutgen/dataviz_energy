###################################
#### Maxime Keutgen 2 March RScript
###################################

library(tidyverse)
library(readr)
library(ggrepel)
data <- read.csv("owid-energy-data.csv")
data$gdp.billion <- data$gdp/10^9
# Add the continents
countryContinent <- read.csv("~/Documents/MStatistics/DataVisualisation/DataVizGit/countryContinent.csv", header=TRUE)
countryContinent <- countryContinent %>% select(code_3,continent,sub_region)

data <- full_join(data,countryContinent,by=c("iso_code"="code_3"))

# First naive visualisations, GDP regressed on Energy Productions, faceted by continents



gdp.energy.consumption <- data %>% select(gdp.billion,primary_energy_consumption,country,continent,year,population) %>% filter(year=="2016") %>% filter(continent !="NA")

ggplot(aes(x=primary_energy_consumption,y=gdp.billion,color=continent),data = gdp.energy.consumption)+
  geom_point()+theme_classic()+
  scale_y_continuous(name = "GDP in billions $",breaks = c(1000,2000,3000,4000),labels = c("1000","2000","3000","4000"))+
  scale_x_continuous(name="Primary Energy Consumption in terawatt-hours (TWh)")+
  geom_label(aes(label=country,alpha=.8))
summary(lm(gdp~primary_energy_consumption,data = gdp.energy.consumption.eu))


gdp.energy.consumption.eu <- gdp.energy.consumption %>% filter(continent=="Europe")
ggplot(aes(x=primary_energy_consumption,y=gdp.billion,color=population),data = gdp.energy.consumption.eu)+
  geom_point()+geom_label_repel(aes(label=country,alpha=.8))+theme_classic()+scale_y_continuous(name = "GDP in billions $",breaks = c(1000,2000,3000,4000),labels = c("1000","2000","3000","4000"))+scale_y_continuous(name="Primary Energy Consumption in terawatt-hours (TWh)")
summary(lm(gdp~primary_energy_consumption,data = gdp.energy.consumption.eu))
gdp.energy.consumption.eu$country

##### New code, 3th of March 2021
library(wesanderson)
ggplot(aes(x=primary_energy_consumption,y=gdp.billion,color=population),data = gdp.energy.consumption)+
  geom_point(size=4)+
  scale_y_continuous(name="GDP in US billions ($)")+geom_label(aes(label=country,color=log(population)),alpha=.1,nudge_y = 100)+
  scale_x_continuous(name="Primary Energy Consumption in terawatt-hours (TWh)")+scale_color_viridis_c()+
  facet_wrap(continent~.,scales = "free")+theme(legend.position = "bottom")

gdp.lowcarbonenergy.df <- data %>% select(gdp.billion,country,low_carbon_elec_per_capita,continent)
ggplot(aes(y=fossil_fuel_consumption,x=continent),data=data)+geom_boxplot()
t <- pivot_longer(data,cols = c("coal_share_energy","gas_share_energy","nuclear_share_energy","hydro_share_energy","renewables_share_energy","oil_share_energy"),names_to = "share energy",values_to = "value")
t <- t %>% filter(year=="2013")
ggplot(aes(x=t$`share energy`,y=value,fill=continent),data = t)+geom_violin(scale = "width",alpha=.6)+scale_fill_viridis_d()+theme_light()+coord_polar()
?pivot_longer()

europe$coal
## Continents
europe <- data %>% filter(sub_region=="Southern Europe")
ggplot(aes(y=coal_cons_per_capita,x=year,color=country),data=europe)+geom_line()+facet_wrap(.~sub_region,scales = "free")+theme(legend.position = "bottom")

# Mean by continents 
t <- data %>% group_by(continent,year) %>% 
  select(coal_elec_per_capita,oil_elec_per_capita,hydro_elec_per_capita,nuclear_elec_per_capita,solar_elec_per_capita,wind_elec_per_capita,gas_elec_per_capita) %>%
  summarise( mean.coal = mean(coal_elec_per_capita),mean.gas=mean(gas_elec_per_capita),
             mean.wind=mean(wind_elec_per_capita),mean.oil=mean(oil_elec_per_capita),
             mean.hydro=mean(hydro_elec_per_capita),mean.sun=mean(solar_elec_per_capita) )
t.lf <- pivot_longer(t,cols=c("mean.coal","mean.gas","mean.wind","mean.oil","mean.hydro","mean.sun"),names_to="electricity source",values_to ="consumption per capita (kilowatt-hours)")
t.lf <- t.lf %>% mutate_all(funs(replace_na(.,0)))
t.lf$`electricity source` <- as.factor(t.lf$`electricity source`)
ggplot(data = t.lf,aes(y=year,x=t.lf$`electricity source`))+
  facet_wrap(.~continent,scales = "free")+geom_violin(stat = density(t.lf$`consumption per capita (kilowatt-hours)`))
View(t.lf)

ggplot(data = t.lf,aes(x=year,y=t.lf$`electricity source`,size=t.lf$`consumption per capita (kilowatt-hours)`))+geom_ribbon()
t.lf$year

df.energysource2015 <- data %>% filter(year==2015) %>% select("coal_share_energy","gas_share_energy","nuclear_share_energy",
                                                              "hydro_share_energy","renewables_share_energy","oil_share_energy","country","continent") 



df.lf <- pivot_longer(data=df.energysource2015,cols = c("coal_share_energy","gas_share_energy","nuclear_share_energy","hydro_share_energy","renewables_share_energy","oil_share_energy"),values_to="consumption",names_to="share energy")
df.lf %>% group_by(continent)%>% mutate(grandsum=sum(consumption,na.rm = TRUE)) %>% ungroup()

df.lf %>% filter(!(country %in% c("Africa","Europe","Oceania","South America","North America","Asia"))) %>% group_by(continent,`share energy`) %>% 
  summarise(sum.energysource=sum(consumption,na.rm = T)) %>% ungroup() %>% group_by(continent) %>% mutate(grand.sum = sum(sum.energysource),proportion=)
View(prop_source_by_continent_2015)
prop_source_by_continent_2015

View(prop_source_by_continent_2015)
View(df.lf)
View(prop_source_by_continent_2015)


prop_source_by_continent_2015 <- df.lf %>% filter()



write.csv(df.lf,file = "df.energysource2015.csv")
write.csv(prop_source_by_continent_2015,file="prop_source_by_continent_2015")


