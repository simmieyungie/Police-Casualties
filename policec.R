#Load in the library
library(tidyverse)

#Load in dataset
police <- read.csv("MPVDatasetDownload.csv")

#MPV dataset\
#Convert date column to date type
police$date <- ymd(police$date)

#Convert the victim age from factor to numeric data type
police$Victim_age <- as.numeric(as.character(police$Victim_age))

#Examine missing values
apply(police, 2, anyNA) 

# Make sure finalfit is up-to-date 
#install.packages("finalfit")      

library(finalfit)
#Plot missing variables
police %>% 
  select(1,3,4,5) %>% 
  missing_plot()

#Summary of all missing variables
police %>% summarise_all(~ sum(is.na(.))) %>% 
  as.data.frame()

#Percentage of missing values
sum(is.na(police$Victim_age))/length(police$Victim_age)

#Replace missing values with mean age
police_1 <- police %>% 
  mutate(Victim_age = replace_na(Victim_age, mean(Victim_age, na.rm = T)))

#Age range by gender
ggplot(police_1, aes(x ="",y = Victim_age)) +
  geom_boxplot(fill = "gold")  +
  ggtitle("Age Range") + xlab("range") + ylab("Age") +
  coord_flip()

#Density plot of victim age  by gender
ggplot(police_1, aes(x = Victim_age, group = Victim_gender, fill = Victim_gender,
                     alpha = 0.5)) +
  geom_density()

#Count casualties by gender
police %>% 
  count(Victim_gender) %>% 
  ggplot(., aes(Victim_gender, n, fill = Victim_gender)) +
  geom_bar(stat = "identity")


#Trend of deaths
daily_count <- police %>% 
  group_by(date) %>% 
  count() #%>% 



#Calender plot  
openair::calendarPlot(daily_count, pollutant = "n", year = 2019)

#Write daily count csv
write.csv(daily_count, "daily.csv")


#lubridate for Date wrangling
library(lubridate)

#Month we've had deaths from all years
police %>% 
  mutate(mth = month(date)) %>% 
  group_by(mth) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(mth = month.name) %>% 
  write.csv("month_death.csv") #Pipe into a csv file directly


#Yearly death trends by month  
police %>% 
  mutate(mth = month(date),
         yr = year(date)) %>% 
  group_by(mth, yr) %>% 
  count() %>% 
  write.csv("yr-by_mth.csv") #Pipe into a csv file directly




#Most victimized race
police %>% 
  mutate(Victim_race = gsub("Unknown Race", "Unknown race", Victim_race)) %>% 
  group_by(Victim_race) %>% 
  count() %>% 
  write.csv("death_by_race.csv") 

#Death trend by month and year (2013 - 2019)
police %>% 
  mutate(mth = month(date),
         yr = year(date)) %>% 
  group_by(mth, yr) %>% 
  count(Victim_race)


#Pervalent Cause of death
police %>% 
  group_by(Cause.of.death) %>% 
  count() %>% 
  write.csv("cause_of_death.csv")


#Mean yearly deaths
police %>% 
  mutate(yr = year(date)) %>% 
  group_by(yr) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(n = mean(n))


#Count occurence by date
pol <- police %>% 
  group_by(date) %>% 
  count()

#Generate a date sequence
pol_day <- seq(as.Date('2013-01-01'), as.Date('2019-12-31'), by = "1 day") %>% 
  as.data.frame()


#Change pol_day column name to date
names(pol_day) <- "date"


#Convert date to a date datatype
pol$date <- lubridate::ymd(pol$date)

#Join the pol_day dataframe and the pol dataframe, such that there will be dates
#Appearing for those days without any casualty
#This is so that we can have the number of casualties by day from 2013 - 2019
n <- pol_day %>% 
  left_join(pol, by = "date")


#Find the number of days with death reports for each year and no death reports
n %>% 
  mutate(yr = year(date)) %>% 
  mutate(dth = if_else(n <1, "n0_death", "death")) %>% 
  group_by(yr) %>% 
  count(dth)

