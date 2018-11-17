#1. WHO DATASET

WHO <- read.csv("WHO.csv")


# D. Country with the lowest literacy
WHO$Country[which.min(WHO$LiteracyRate)]


# E. Richest Country in Europe based on GNI
Europe <- subset(WHO, Region == "Europe")
Europe$Country[which.max(Europe$GNI)]


# F. Mean Life expectancy of countries in Africa
Africa <- subset(WHO, Region == "Africa")
mean(Africa$LifeExpectancy, na.rm=TRUE)


# G. Number of countries with population greater than 10,000
length(WHO$Country[WHO$Population>1000])

# H. Top 5 Countries in the Americas with the highest Child Mortality
Americas <- subset(WHO, Region == "Americas")
Sorted_Americas <- head(Americas[order(Americas$ChildMortality, decreasing = TRUE),], 5)
Sorted_Americas$Country


#NBA DATASET
NBA <- read.csv("Historical NBA Performance.csv")

# A. The year Bulls has the highest winning percentage
Bulls <- subset(NBA, Team == "Bulls")
Bulls$Year[which.max(Bulls$Winning.Percentage)]


# B. Teams with an even win-loss record in a year
Teams <- NBA$Team[NBA$Winning.Percentage == 0.5]
Win_Loss_Teams = data.frame(Teams)
na.omit(Win_Loss_Teams)

#3. Season_Stats

Seasons_Stats <- read.csv("Seasons_Stats.csv")

# A. Player with the highest 3-pt attempt rate in a season
Seasons_Stats$Player[which.max(Seasons_Stats$X3PAr)]


# B. Player with the highest free throw rate in a season
Seasons_Stats$Player[which.max(Seasons_Stats$FTr)]


# C. What year/season does Lebron James scored the highest?
Lebron <- subset(Seasons_Stats, Player == "LeBron James")
Lebron$Year[which.max(Lebron$PTS)]


# D. What year/season does Michael Jordan scored the highest?
MJ <- subset(Seasons_Stats, Player == "Michael Jordan*")
MJ$Year[which.max(MJ$PTS)]


# E. Player efficiency rating of Kobe Bryant in the year where his MP is the lowest?
Kobe <- subset(Seasons_Stats, Player == "Kobe Bryant")
Kobe$PER[which.min(Kobe$MP)]


#4. National Universities Rankings

NU_Rankings <- read.csv("National Universities Rankings.csv")


# A. University with the most number of undergrads
NU_Rankings$Undergrad.Enrollment <- as.numeric(gsub (",", "", NU_Rankings$Undergrad.Enrollment))
NU_Rankings$Name[which.max(NU_Rankings$Undergrad.Enrollment)]


# B. Average Tuition in the Top 10 University
Top_10_University <- head(NU_Rankings,10)
Tuition1 <- gsub(",","",Top_10_University$Tuition.and.fees)
Tuition2 <- gsub("\\$","",Tuition1)
Final_Tuition <- as.numeric(Tuition2)
mean(Final_Tuition)