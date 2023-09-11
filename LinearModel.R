library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(UsingR)
library(broom)
library(ggfortify)

getwd()
setwd("D:/")
setwd("/CourseWorks/DataScienceAssessment/CourseWorkFiles/CleanedData")



# House prices vs Broadband download speed
Town  = read_csv("Towns.csv") %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  dplyr::select(shortPostcode, Town, District, County)

broadband = read_csv('Broadband.csv', show_col_types = FALSE) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() %>% 
  group_by(District) %>% 
  summarise(AverageDownloadSpeed = mean(`Average download speed (Mbit/s)`))
  


HousePrice = read_csv('HousePrices.csv', show_col_types = FALSE) %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price))


PriceAndBroadbandspeed = HousePrice %>% 
  left_join(broadband, by = "District")


  
mod = lm(AveragePrice ~ AverageDownloadSpeed, data=PriceAndBroadbandspeed)    # Additive model (no interaction between gestation and smoke)
summary(mod)


fitted = augment(mod)
fitted

autoplot(mod)


ggplot(mod, aes(x=AveragePrice,y=AverageDownloadSpeed)) +
  geom_point()+
  geom_smooth(method="lm", se=FALSE)

#--------------------------------------------------------------------------------------------------------------------------------------


# House prices vs Drug offence rate
Town  = read_csv("Towns.csv") %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  dplyr::select(shortPostcode, Town, District, County)

crimeData = read_csv("Crime.csv") %>% 
  dplyr::select(ID,Year,shortPostcode,CrimeType, CrimeCount) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()

HousePrice = read_csv('HousePrices.csv', show_col_types = FALSE) %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price))

DrugsData <- crimeData %>% 
  filter(CrimeType=="Drugs") %>%
  group_by(District) %>%
  mutate(DrugCount = mean(CrimeCount)) %>% 
  distinct(District, DrugCount) %>% 
  dplyr::select(District, DrugCount)

HousepriceAndDrugs = HousePrice %>% 
  left_join(DrugsData, by="District")


mod = lm(AveragePrice ~ DrugCount, data=HousepriceAndDrugs)    # Additive model (no interaction between gestation and smoke)
summary(mod)
ggplot(mod, aes(x=AveragePrice, y=DrugCount))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)


#--------------------------------------------------------------------------------------------------------------------------------------


options(scipen = 99999999)
# Attainment8Score vs House prices
HousePrice = read_csv('HousePrices.csv', show_col_types = FALSE)

SchoolData = read_csv("School.csv") %>% 
  dplyr::select(shortPostcode=shortPostCode,SchoolName,Attainment8Score)


HousePriceandATT8SCR= HousePrice %>% 
  left_join(SchoolData, by = "shortPostcode") %>% 
  na.omit() %>% 
  group_by(Price) %>% 
  summarise(AverageAttainmentScore = mean(Attainment8Score))

mod = lm(AverageAttainmentScore ~ Price, data=HousePriceandATT8SCR)
summary(mod)

ggplot(mod, aes(x=AverageAttainmentScore, y=Price))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)
#--------------------------------------------------------------------------------------------------------------------------------------



# Average Download Speed vs Drug offence rate
Town  = read_csv("Towns.csv") %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  dplyr::select(shortPostcode, Town, District, County)

broadband = read_csv('Broadband.csv', show_col_types = FALSE) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() %>% 
  group_by(District) %>% 
  summarise(AverageDownloadSpeed = mean(`Average download speed (Mbit/s)`))

crimeData = read_csv("Crime.csv") %>% 
  dplyr::select(ID,Year,shortPostcode,CrimeType, CrimeCount) %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()

DrugsData <- crimeData %>% 
  filter(CrimeType=="Drugs") %>%
  group_by(District) %>%
  mutate(DrugCount = mean(CrimeCount)) %>% 
  distinct(District, DrugCount) %>% 
  dplyr::select(District, DrugCount)

downloadspeedAndDrugs = broadband %>% 
  left_join(DrugsData, by="District")

mod = lm(AverageDownloadSpeed ~ DrugCount, data=downloadspeedAndDrugs)    # Additive model (no interaction between gestation and smoke)
summary(mod)

ggplot(mod, aes(x=AverageDownloadSpeed, y=DrugCount))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)



#--------------------------------------------------------------------------------------------------------------------------------------


# Average download speed vs Attainment8score

broadband = read_csv('Broadband.csv', show_col_types = FALSE) 

SchoolData = read_csv("School.csv") %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  dplyr::select(shortPostcode,SchoolName,Attainment8Score)

SchoolData= SchoolData %>% 
  left_join(broadband, by = "shortPostcode") %>% 
  dplyr::select(SchoolName,Attainment8Score, avgDownloadSpeed = `Average download speed (Mbit/s)`) %>% 
  na.omit() %>% 
  group_by(Attainment8Score) %>% 
  summarise(avg = mean(avgDownloadSpeed)) %>% 
  arrange(-Attainment8Score)

mod = lm(avg ~ Attainment8Score, data=SchoolData)  
summary(mod)

ggplot(mod, aes(x=avg, y=Attainment8Score))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)


