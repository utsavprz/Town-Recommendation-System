library(tidyverse)
library(stringi)



getwd()
setwd("D:/")
setwd("/CourseWorks/DataScienceAssessment/CourseWorkFiles/CleanedData")




#--------------------------------------------------------------------------------------------------------------------------------------


# HOUSE PRICE RANKING

Towns  = read_csv("Towns.csv") %>% 
  dplyr::select(PostCode, shortPostcode, Town, District, County)

HousePrices = read_csv('HousePrices.csv', show_col_types = FALSE)

summary(HousePrices$Price)


HousePricesRanking = Towns %>% 
  left_join(HousePrices, by = "shortPostcode") %>% 
  dplyr::select(District, shortPostcode, Price) %>% 
  na.omit() %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  arrange(AveragePrice) %>% 
  mutate(PriceScore=10 - AveragePrice/247995) %>%
  dplyr::select(District, PriceScore)


HousePricesRanking

write.csv(HousePricesRanking, "../ranking/HousePricesRanking.csv")
#--------------------------------------------------------------------------------------------------------------------------------------


# BROADBAND SPEED RANKING

Towns  = read_csv("Towns.csv") %>% 
  dplyr::select(PostCode, shortPostcode, Town, District, County)

broadband = read_csv('Broadband.csv', show_col_types = FALSE)

summary(broadband)


BroadbandSpeedRanking = Towns %>% 
  mutate(shortPostcode = str_trim(str_sub(PostCode, -4,-1))) %>% 
  left_join(broadband, by = "shortPostcode") %>% 
  dplyr::select(District, shortPostcode, `Average download speed (Mbit/s)`) %>% 
  na.omit() %>% 
  group_by(District) %>% 
  summarise(AverageDownloadSpeed = mean(`Average download speed (Mbit/s)`)) %>% 
  mutate(DownloadScore=AverageDownloadSpeed/6) %>%
  dplyr::select(District, DownloadScore) %>% 
  arrange(-DownloadScore)

BroadbandSpeedRanking

write.csv(BroadbandSpeedRanking, "../ranking/BroadbandRanking.csv")
#--------------------------------------------------------------------------------------------------------------------------------------


# CRIME RANKING

Town  = read_csv("Towns.csv") %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  dplyr::select(shortPostcode, Town, District, County)

crimeData = read_csv("Crime.csv") %>% 
  dplyr::select(ID,Year,shortPostcode,CrimeType, CrimeCount)


summary(crimeData$CrimeCount)


crimeDataRanking = crimeData %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit() %>% 
  dplyr::select(District,CrimeCount) %>% 
  group_by(District) %>% 
  summarise(AverageCrimeCount=mean(CrimeCount)) %>% 
  arrange(AverageCrimeCount) %>% 
  mutate(CrimeCountScore=10 - AverageCrimeCount/391) %>% 
  dplyr::select(District,CrimeCountScore)

crimeDataRanking
write.csv(crimeDataRanking, "../ranking/crimeDataRanking.csv")
#--------------------------------------------------------------------------------------------------------------------------------------


# SCHOOL RANKING

Towns = read_csv("UncleanedHousePrices.csv")%>% 
  filter(County == 'GREATER MANCHESTER' | County == 'MERSEYSIDE') %>% 
  mutate(shortPostcode = str_trim(substring(PostCode, 1,4))) %>% 
  dplyr::select(shortPostcode, Town, District, County) %>% 
  na.omit()

Towns<-Towns[!(Towns$District == "SALFORD" & Towns$shortPostcode=="M14"),]
Towns<-Towns[!(Towns$District == "ROCHDALE" & Towns$shortPostcode=="M16"),]


SchoolData=read_csv("School.csv")

summary(SchoolData$Attainment8Score)

SchoolScoreData = SchoolData %>%
  rename(shortPostcode=shortPostCode) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit() %>% 
  group_by(District,SchoolName) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  mutate(score=score/8) %>% 
  dplyr::select(District,SchoolName,score) %>% 
  arrange(-score)

SchoolRanking = SchoolScoreData %>% 
  group_by(District) %>% 
  summarise(score=max(score)) %>% 
  left_join(SchoolScoreData, by="score") %>% 
  arrange(-score) %>% 
  filter(SchoolName!="Levenshulme High School" ,
         SchoolName!="Saint Paul's Catholic High School") %>% 
  dplyr::select(District = District.x,SchoolName,SchoolScore=score) %>% 
  distinct()

SchoolRanking<-SchoolRanking[!(SchoolRanking$District == "STOCKPORT" &
                                 SchoolRanking$SchoolName=="Parrs Wood High School"),]
de<-data.frame("STOCKPORT", "Levenshulme High School", 6.350000)
names(de)<-c("District","SchoolName","SchoolScore")

SchoolRanking <- rbind(SchoolRanking,de) %>% 
  arrange(-SchoolScore)
write.csv(SchoolRanking, "../ranking/SchoolRanking.csv")


#--------------------------------------------------------------------------------------------------------------------------------------


# OVERALL RANKING

# joining all individual ranking
RankingMerge = HousePricesRanking %>% 
  left_join(BroadbandSpeedRanking, by = "District") %>% 
  left_join(crimeDataRanking, by = "District") %>% 
  left_join(SchoolRanking, by = "District")

# Adding not available for district with no school data and 0 as attainment8score
RankingMerge$SchoolName[is.na(RankingMerge$SchoolName)] <- "Not available"
RankingMerge$SchoolScore[is.na(RankingMerge$SchoolScore)] <- 0

# Ranking based on mean of all score
overallRank = RankingMerge %>% 
  group_by(PriceScore, DownloadScore,CrimeCountScore,SchoolScore) %>%
  mutate(OverallScore = (PriceScore + DownloadScore + CrimeCountScore + SchoolScore)/4) %>% 
  arrange(-OverallScore) %>%
  dplyr::select( District, PriceScore, DownloadScore,CrimeCountScore,SchoolScore, OverallScore) %>% 
  head(5)

# Adding a Rank number
overallRank <- cbind(Rank = 1:nrow(overallRank), overallRank)  


write.csv(overallRank, "../ranking/overallRank.csv")