library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(ggrepel)
library(reshape2)
library(hrbrthemes)
library(stringi)

getwd()
setwd("D:/")
setwd("/CourseWorks/DataScienceAssessment/CourseWorkFiles/CleanedData")


euro <- dollar_format(prefix = "\u20ac", big.mark = ",")

Towns = read_csv("Towns.csv") %>% 
  dplyr::select(shortPostcode, Town, District, County)

housePrices = read_csv('HousePrices.csv', show_col_types = FALSE) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

summary(housePrices)


# BARGRAPH houseprices by district (2019-2021)
housePrices %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill= District)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous(breaks = seq(0, 500000, 25000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = 0.4,hjust = 1.1, color="black") +
  labs(title = "2019-2021 Average house prices by district")+
  coord_flip()+
  theme_ipsum()



Price = housePrices %>% 
  filter(Year == 2021, Price <= 2000000)

summary(Price$Price)


# BARGRAPH houseprices by district (2021)
housePrices %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill=District)) +
  geom_bar(position = "stack",stat = "identity") +
  scale_y_continuous(breaks = seq(0, 500000, 25000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = 0.4,hjust = 1.1, color="black") +
  labs(title = "2021 Average house prices by district") +
  coord_flip()+
  theme_ipsum()


#LINEGRAPH Average house prices by year (2019-2021)
housePrices %>% 
  group_by(Year) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Year, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.85, color="black") +
  scale_y_continuous(breaks = seq(0, 350000, 5000), 
                     label = euro) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2021 Average house prices by year")+
  theme_ipsum()



# BOXPLOT Average house prices by district (2019-2021)
housePrices %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,100000), 
                     label = euro) +
  geom_boxplot(outlier.colour="#CA615A",
               outlier.fill="black",
               outlier.size=2,
               outlier.alpha = 0.3) +
  coord_flip() +
  labs(title="2019-2021 house prices by district")+
  theme_ipsum()



# BOXPLOT Average house prices by district (2021)
housePrices %>% 
  filter(Year == 2021, District =="STOCKPORT") %>% 
  summary(Price)

housePrices %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2500000,150000), 
                     label = euro) +
  geom_boxplot(outlier.colour="#CA615A",
               outlier.fill="black",
               outlier.size=2,
               outlier.alpha = 0.3) +
  coord_flip() +
  labs(title="2021 house prices by district")+
  theme_ipsum()


#--------------------------------------------------------------------------------------------------------------------------------------


# BROADBAND GRAPHS


Town = read_csv('Towns.csv', show_col_types = FALSE) %>% 
  select(PostCode,Town, District, County)
broadband = read_csv('Broadband.csv', show_col_types = FALSE)

BroadbandSpeed = Town %>% 
  mutate(shortPostcode = str_trim(str_sub(PostCode, -4,-1))) %>% 
  left_join(broadband, by = "shortPostcode") %>% 
  select(District, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Maximum download speed (Mbit/s)`,
         `Maximum upload speed (Mbit/s)`) %>% 
  na.omit()


# To find average download speeds of district
BroadbandSpeed %>% 
  group_by(District) %>% 
  summarise(AverageDownloadSpeed = mean(`Average download speed (Mbit/s)`)) %>% 
  arrange(AverageDownloadSpeed)

# Barplot for Average download speed (Mbit/s) by district
BroadbandSpeed %>% 
  group_by(District) %>% 
  summarise(AverageDownloadSpeed = round(mean(`Average download speed (Mbit/s)`))) %>% 
  ggplot(aes(x = District, y = AverageDownloadSpeed,fill = District )) +
  geom_bar(stat = "identity",position = "stack") +
  scale_y_continuous(breaks = seq(0, 200, 5)) +
  geom_text(aes(label = AverageDownloadSpeed), 
            vjust = 0.4,hjust = 1.1, color="black") +
  labs(title = "Average download speed (Mbit/s) by district", x = "District",
       y = "Average Download Speed (Mbit/s)")+
  coord_flip()+
  theme_ipsum()

# Boxplot for Average download speed (Mbit/s) by district
BroadbandSpeed %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = `Average download speed (Mbit/s)`, fill=District)) +
  scale_y_continuous(breaks = seq(0,200,10)) +
  geom_boxplot(outlier.colour="red",
               outlier.fill="black",
               outlier.size=2) +
  labs(title = "Average download speed (Mbit/s) by district", x = "District",
       y = "Average Download Speed (Mbit/s)")+
  coord_flip()+
  theme_ipsum()



#--------------------------------------------------------------------------------------------------------------------------------------


# SCHOOL GRAPHS


schoolData = read_csv('School.csv', show_col_types = FALSE)

liverpoolSchoolData = read_csv('liverpoolSchoolData.csv')
manchesterSchoolData = read_csv('manchesterSchoolData.csv')


# Linegraph Average Attainment8Score by year
schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color="#69b3a2", size=2, alpha=0.9, linetype=2) +
  geom_text(aes(label = sprintf(AverageAttainment, fmt = '%#.2f')), 
            vjust = -0.85, color="black") +
  scale_x_continuous(breaks = 2016:2019) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")+
  theme_ipsum() 


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30
schoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Attainment8Score of Schools")

manchesterSchoolData %>% 
  filter(Attainment8Score>30, SchoolName=="Manchester High School for Girls") %>% 
  summary()


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (LIVERPOOL SCHOOL ONLY)
liverpoolSchoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 100, 5))+
  geom_boxplot(fill="#3D9894", color="#467696", alpha=0.6, outlier.colour="red",
               outlier.fill="red",
               outlier.size=2) +
  coord_flip() +
  theme_ipsum() +
  labs(title="2016-2019 Average Attainment8Score of Liverpool Schools")



# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (MANCHESTER SCHOOL ONLY)
manchesterSchoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 100, 5))+
  geom_boxplot(fill="#3D9894", color="#467696", alpha=0.6, outlier.colour="red",
               outlier.fill="red",
               outlier.size=2) +
  coord_flip() +
  theme_ipsum() +
  labs(title="2016-2019 Average Attainment8Score of Manchester Schools")



#--------------------------------------------------------------------------------------------------------------------------------------


# CRIME GRAPHS


Town  = read_csv("Towns.csv") %>% 
  mutate(shortPostcode = str_trim(stri_sub(PostCode,-3))) %>% 
  dplyr::select(shortPostcode, Town, District, County)

crimeData = read_csv("Crime.csv") %>% 
  dplyr::select(ID,Year,shortPostcode,CrimeType, CrimeCount)


crimeData = crimeData %>% 
  left_join(Town, by = "shortPostcode") %>% 
  na.omit()


# Boxplot for 2019-2021 Drugs count by District
crimeData %>% 
  filter(CrimeType == "Drugs") %>% 
  ggplot(aes(x=District, y=CrimeCount, fill=District)) + 
  scale_y_continuous(breaks = seq(0, 2000, 100))+
  geom_boxplot(outlier.colour="red",
               outlier.fill="red",
               outlier.size=2) +
  labs(title=" 2019-2021 Drugs count by District")+
  coord_flip()+
  theme_ipsum()

crimeData %>% 
  filter(CrimeType == "Drugs") %>%
  summary()


# Piechart for 2021 Robbery Rate 
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021) %>%
  group_by(District) %>%
  mutate(sumCount = sum(CrimeCount)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(sumCount)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(District, sumCount, perc, labels) %>% 
  dplyr::select(District, sumCount, perc, labels)


RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = District)) +
  geom_bar(stat="identity", width=2, size = 1, color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title="2021 Robbery Rate")+
  theme_ft_rc()+
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")




# Piechart for 2021 Robbery Rate of MERSEYSIDE
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021, County == "MERSEYSIDE") %>%
  group_by(District) %>%
  mutate(sumCount = sum(CrimeCount)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(sumCount)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(District, sumCount, perc, labels) %>% 
  select(District, sumCount, perc, labels)

RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = District)) +
  geom_bar(stat="identity", width=2, size = 1, color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title="2021 Robbery Rate of MERSEYSIDE")+
  theme_ft_rc()+
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")


# Piechart for 2021 Robbery Rate of GREATER MANCHESTER
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021, County == "GREATER MANCHESTER") %>%
  group_by(District) %>%
  mutate(sumCount = sum(CrimeCount)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(sumCount)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(District, sumCount, perc, labels) %>% 
  select(District, sumCount, perc, labels)

RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = District)) +
  geom_bar(stat="identity", width=2, size = 1, color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  labs(title="2021 Robbery Rate of GREATER MANCHESTER")+
  theme_ft_rc()+
  scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "")




# Radar chart for vehicle crime rate
crimeData %>% 
  filter(CrimeType=="Vehicle crime") %>%
  dplyr::select(Year,District,CrimeCount) %>% 
  group_by(District) %>%
  summarise(min=min(CrimeCount), mean = mean(CrimeCount)) %>% 
  gather(min, mean, -District) %>% 
  spread(District, mean) %>% 
  radarchart(axistype = 1, caxislabels = seq(0, 1.2,0.30))




