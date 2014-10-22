source('C:\\Users\\mb24244\\Documents\\football anaylsis\\conferenceDataWithFCS.r')
records <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\recordsWithFCS.csv', sep=',', header=T)
schedules <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\games.csv', sep=',', header=T)

teams <- conferenceRecords[conferenceRecords$Conference != "FCS",] 


Adjusted.SoS <- c(1:length(conferenceRecords$TEAM))

First.Pass.SoS <- c(1:length(conferenceRecords$TEAM))

conferenceRecords <- cbind(conferenceRecords, First.Pass.SoS)
conferenceRecords <- cbind(conferenceRecords, Adjusted.SoS)


conferenceRecords[conferenceRecords$Conference == "FCS",]$First.Pass.SoS <- 0.385
conferenceRecords[conferenceRecords$Conference == "FCS",]$Adjusted.SoS <- 0.385 

for(i in 1:length(teams$TEAM))
{  
  homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
  awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
  
  homeOppRecords <- conferenceRecords[match(homeGames$Visitor, conferenceRecords$TEAM),] 
  awayOppRecords <- conferenceRecords[match(awayGames$Home.Team, conferenceRecords$TEAM),] 
  
  homeOppRecords <- subset(homeOppRecords, Conference != as.character(conferenceRecords[i,]$Conference)) 
  awayOppRecords <- subset(awayOppRecords, Conference != as.character(conferenceRecords[i,]$Conference))
  
  
  homeScores <- 0
  awayScores <- 0
  
  if(length(homeOppRecords) != 0)
  {
    homeScores <- 0.95*(homeOppRecords$ConferenceOffset)*(homeOppRecords$OVERALL.W / (homeOppRecords$OVERALL.W+homeOppRecords$OVERALL.L))
  }
  if(length(awayOppRecords) != 0)
  {
    awayScores <- 1.05*(awayOppRecords$ConferenceOffset)*(awayOppRecords$OVERALL.W / (awayOppRecords$OVERALL.W+awayOppRecords$OVERALL.L))
  }
  
  homeScores <- homeScores[!is.na(homeScores)]
  awayScores <- awayScores[!is.na(awayScores)]
  
  conferenceRecords[(conferenceRecords$TEAM == as.character(teams[i,]$TEAM)),]$First.Pass.SoS <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
}


for(i in 1:length(teams$TEAM))
{  
  homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
  awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
  
  homeOppRecords <- conferenceRecords[match(homeGames$Visitor, conferenceRecords$TEAM),] 
  awayOppRecords <- conferenceRecords[match(awayGames$Home.Team, conferenceRecords$TEAM),] 
  
  homeOppRecords <- subset(homeOppRecords, Conference != as.character(conferenceRecords[i,]$Conference))
  awayOppRecords <- subset(awayOppRecords, Conference != as.character(conferenceRecords[i,]$Conference))
  
  homeScores <- 0
  awayScores <- 0
  
  if(length(homeOppRecords) != 0)
  {
    homeScores <- 0.95*(homeOppRecords$ConferenceOffset)*homeOppRecords$First.Pass.SoS
  }
  if(length(awayOppRecords) != 0)
  {
    awayScores <- 1.05*(awayOppRecords$ConferenceOffset)*awayOppRecords$First.Pass.SoS
  }
  
  homeScores <- homeScores[!is.na(homeScores)]
  awayScores <- awayScores[!is.na(awayScores)]
  
  secondPass <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
  
  conferenceRecords[(conferenceRecords$TEAM == as.character(teams[i,]$TEAM)),]$Adjusted.SoS<- ((2*conferenceRecords[(conferenceRecords$TEAM == as.character(teams[i,]$TEAM)),]$First.Pass.SoS)+secondPass)/3
}


maxSoS <- max(conferenceRecords$Adjusted.SoS)
minSoS <- min(conferenceRecords[conferenceRecords$Conference != "FCS",]$Adjusted.SoS)

SoS.Score <- (conferenceRecords$Adjusted.SoS - minSoS) / (maxSoS-minSoS)

conferenceRecords <- cbind(conferenceRecords, SoS.Score)

SoS.Ranking <- length(teams$TEAM) - ((length(teams$TEAM)-1)*conferenceRecords$SoS.Score)

conferenceRecords <- cbind(conferenceRecords, SoS.Ranking)
FBS.Records <- conferenceRecords[conferenceRecords$Conference != "FCS",] 
sumTable <- aggregate(cbind(Adjusted.SoS, SoS.Score, SoS.Ranking) ~ Conference, data = conferenceRecords, sum)
countTable <- aggregate(cbind(Adjusted.SoS, SoS.Score, SoS.Ranking) ~ Conference, data = conferenceRecords, length)
Average.OOC.SoS <- sumTable/countTable

Average.OOC.SoS$Conference <- sumTable$Conference 

