#Initializing environment
cat("\f")
rm(list = ls())
library(tidyverse)
library(janitor)
library(here)

#Cleaning load and clean data set
area <- read_csv(here("rawdata", "land-area-km.csv")) |> clean_names()
area <- area|>filter(year == 2020)
area <- area |> rename(country = entity)

#Clean ufo dataset 
ufo <- read_csv(here("rawdata", "ufo-sightings-transformed.csv")) |> clean_names()
ufo<-count(ufo, country, sort = TRUE) |> na.omit(ufo)

#Clean gdp per capita dataset
development <- read_csv(here("rawdata", "gdp-per-capita-worldbank.csv")) |> clean_names()
development<- development |> filter(year == "2021") |> rename(country = entity)


#Merge to make linear model
ufogdp<- merge(x = ufo, y = development, by = "country", all.y = TRUE) |>
  rename(sightings = n, gdpcapita = gdp_per_capita_ppp_constant_2017_international)
ufogdp[is.na(ufogdp)] <- 0

#Linear model on sightings by GDP
sightingsModel<- lm(sightings ~ gdpcapita, data = ufogdp)
summary(sightingsModel)
ufoplot<- ggplot(ufogdp, aes(gdpcapita, sightings)) +geom_point() +
  geom_smooth(method = 'lm') + labs(title="UFO Sighitings by GDP Per Capita",
                                  x ="GDP Per Capita ($)", y = "UFO Sightings")

#This is messy, US is a huge outlier so I will remove that data point
ufonoUS<- ufogdp |> filter(country != "United States")
sightingsModelNoUS<- lm(sightings ~ gdpcapita, data = ufonoUS)
summary(sightingsModelNoUS)
noUSPlot<- ggplot(ufonoUS, aes(gdpcapita, sightings)) +geom_point() +
geom_smooth(method = 'lm')+ labs(title="UFO Sighitings by GDP Per Capita (No U.S.)",
                                  x ="GDP Per Capita ($)", y = "UFO Sightings")

#Look at both models at the same time:
require(gridExtra)
grid.arrange(ufoplot, noUSPlot)

#Linear model on sightings by area and GDP (not our topic but we can look at it)
areagdp <- merge(x = area, y = ufogdp, by = "country", all.y = TRUE)
areagdp[is.na(areagdp)] <- 0
areamodel <- lm(sightings ~ land_area_sq_km, data = areagdp)
summary(areamodel)

#Remove values with sightings greater than 500
ufogdp_less500=ufogdp[ufogdp$sightings<500, ]

#Linear model on sightings by GDP
sightingsModel_less500<- lm(sightings ~ gdpcapita, data = ufogdp_less500)
summary(sightingsModel_less500)

#Plot for countries with less than 500 sightings
ufo_less500plot<- ggplot(ufogdp_less500, aes(gdpcapita, sightings)) +geom_point() +geom_smooth(method = 'lm')+
  labs(title="UFO Sighitings by GDP Per Capita (Sightings <500)",
         x ="GDP Per Capita ($)", y = "UFO Sightings")
grid.arrange(ufo_less500plot)


#Remove values with sightings greater than 150
ufogdp_less150=ufogdp[ufogdp$sightings<150, ]

#Linear model on sightings by GDP
sightingsModel_less150<- lm(sightings ~ gdpcapita, data = ufogdp_less150)
summary(sightingsModel_less150)
ufo_less150plot<- ggplot(ufogdp_less150, aes(gdpcapita, sightings)) +geom_point() +geom_smooth(method = 'lm')+
  labs(title="UFO Sighitings by GDP Per Capita (Sightings <150)",
       x ="GDP Per Capita ($)", y = "UFO Sightings")
grid.arrange(ufo_less150plot)

#Look at both models at the same time:
require(gridExtra)
grid.arrange(ufo_less500plot, ufo_less150plot)

#GGplot histogram sightings less than 500
hist_less500 = ggplot(ufogdp_less500, aes(sightings))+geom_histogram(color="black",fill="white") +
  geom_vline(aes(xintercept=mean(sightings)), color="blue", linetype="dashed", size=1)+
  labs(title="Amount of UFO Sightings Histogram (Sightings <500)",
       x ="UFO Sightings", y = "Number of Countries")

#GGplot histogram sightings less than 150
hist_less150 = ggplot(ufogdp_less150, aes(sightings))+geom_histogram(color="black",fill="white") +
  geom_vline(aes(xintercept=mean(sightings)), color="blue", linetype="dashed", size=1)+
  labs(title="Amount of UFO Sightings Histogram (Sightings <150)",
       x ="UFO Sightings", y = "Number of Countries")

#All plots
ufoplot
noUSPlot
ufo_less500plot
ufo_less150plot
hist_less500
hist_less150


