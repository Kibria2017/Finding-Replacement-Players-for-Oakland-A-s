library(dplyr)
install.packages("tinytex")
#loading batting table
batting<-read.csv("C:/Users/Mohsin Asif/Box Sync/MS IS/Moneyball Project/batting.csv")

#loading salary table
salary<-read.csv("C:/Users/Mohsin Asif/Box Sync/MS IS/Moneyball Project/salaries.csv")

#analyzing the structure and summarizing batting table
str(batting)
summary(batting)

#analyzing the structure and summarizing batting table
str(salary)
summary(batting)

#Adding a new variable BA for calculating Batting Average: 
batting$BA<-batting$H/batting$AB

#Adding a new variable OBP for calculating On Base Percentage: 
batting$OBP<-(batting$H+batting$BB+batting$HBP)/(batting$AB+batting$BB+batting$HBP+batting$SF)

#Adding a new variable SLG for calculating Slugging: 
#Creating a new variable 1xB to calculate Singles as it is not already in the batting table 
batting$x1B<-batting$H-(batting$X2B+batting$X3B+batting$HR)

#Creating new column for slugging using following formula
batting$SLG<-(batting$x1B+(2*batting$X2B)+(3*batting$X3B)+(4*batting$HR))/batting$AB

#Quick look at the final batting table:
head(batting, 25)

#Since our Salary data starts from year 1985, we will only select data after 1984 from batting table
batting<-subset(batting, batting$yearID>1984,)

#Checking if we applied subset function correctly and only have data after 1984
summary(batting$yearID)

#Merging batting and salary data for further analysis on selecting replacement players 
batsal<-merge(batting, salary, by=c('playerID','yearID'))

#Removing null values
batsal_clean<-na.omit(batsal)

#Subselecting data of players lost in 2001
lost_players<-subset(batsal_clean, yearID==2001 & 
                       batsal_clean$playerID %in% c('giambja01','damonjo01','saenzol01'), 
                                 select=c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB'))

#Statistics of Lost players in 2001
mean(lost_players$OBP)
sum(lost_players$AB)

#Excluding lost players from the analysis  
remaining_players<-subset(batsal_clean, !(batsal_clean$playerID %in% c('giambja01','damonjo01','saenzol01')))

#Selecting replacement players according to the defined criteria
replacement_players<-subset(remaining_players,AB>300 & yearID==2001 & OBP>0.37 & salary<=5000000)

#Arraning for highest OBP
replacement_players<-arrange(replacement_players,-OBP)

#Selecting top 3 players base on OBP
top3<-head(replacement_players, 3)
top3
#Statistics of replacement players
mean(top3$OBP)
sum(top3$AB)
sum(top3$salary)
