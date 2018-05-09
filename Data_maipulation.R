library(tidyverse)
library(lubridate)
library(dplyr)



#read the csv files
travel_mode <- read_csv("travel.csv")   #reading the commute files
annual_wage <- read_csv("avgwage.csv")  #reading the wage file




# filtering out residential states other than California and New York
travel_mode <- travel_mode %>%
  filter(str_detect(`R State Name`, "California") | str_detect(`R State Name`, "New York"))

# filtering out work states other than California and New York
travel_mode <- travel_mode %>%
  filter(str_detect(`State Name`, "California") | str_detect(`State Name`, "New York"))

#removing the Car, truck or van text from Travel Mode to simplify to only the result of the vehicle

travel_mode$`Travel Mode` <- gsub(pattern ="Car, truck, or van:",travel_mode$`Travel Mode`, replacement="")

#deleting rows with any column value as NA
travel_mode <- na.omit(travel_mode)

#deleting the Margin of Error Column
travel_mode <- subset(travel_mode, select = -c(`Margin of Error`))

#reading in the avg annual wage column only for California and New york states
annual_wage <- subset(annual_wage, select = c(`Annual Average Pay`, `St Name`, `Area`, `Cnty`))

annual_wage <- annual_wage%>%
  filter(str_detect(`St Name`, "California") | str_detect(`St Name`, "New York")) 


annual_wage <- annual_wage %>%
  filter(`Cnty` != '000')


annual_wage <- na.omit(annual_wage)

#Filtering out counties outside of Bay Aread and New York City in travel dataset
travel_mode <- travel_mode%>%
  filter(str_detect(`County Name`, "Alameda") | str_detect(`County Name`, "Napa") | str_detect(`County Name`, "Santa Clara") 
         | str_detect(`County Name`, "Contra Costa")| str_detect(`County Name`, "San Francisco") 
         | str_detect(`County Name`, "Solana") | str_detect(`County Name`, "Marin") | str_detect(`County Name`, "Sant Mataeo") 
         | str_detect(`County Name`, "Sonoma") | str_detect(`County Name`, "Manhattan") | str_detect(`County Name`, "Bronx")
         | str_detect(`County Name`, "Brooklyn") | str_detect(`County Name`, "Queens") | str_detect(`County Name`, "Staten Island"))



#Filtering out counties outside of Bay Aread and New York City in wage dataset
annual_wage <- annual_wage%>%
  filter(str_detect(`Area`, "Alameda") | str_detect(`Area`, "Napa") | str_detect(`Area`, "Santa Clara") 
         | str_detect(`Area`, "Contra Costa")| str_detect(`Area`, "San Francisco") 
         | str_detect(`Area`, "Solana") | str_detect(`Area`, "Marin") | str_detect(`Area`, "Sant Mataeo") 
         | str_detect(`Area`, "Sonoma") | str_detect(`Area`, "Manhattan") | str_detect(`Area`, "Bronx")
         | str_detect(`Area`, "Brooklyn") | str_detect(`Area`, "Queens") | str_detect(`Area`, "Staten Island"))

#Ordering Columns in wage dataset
annual_wage <- annual_wage[,c(4,2,3,1)][order(annual_wage$`St Name`),]


#travel_mode <- cbind(travel_mode, annual_wage, by.x = "County FIPS Code", by.y ="Cnty", all=FALSE)

write.csv(travel_mode, file = "travelmodes.csv", row.names = F)
write.csv(annual_wage, file = "wages.csv", row.names = F)