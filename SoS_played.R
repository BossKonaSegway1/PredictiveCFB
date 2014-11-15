All.SoS <- function(){
  source('conferenceData.r')
  records <- read.csv(file = 'data\\records.csv', sep=',', header=T)
  schedules <- read.csv(file = 'data\\gamesPlayed.csv', sep=',', header=T)
  
  conferenceRecords <- Conference.Records()
  
  teams <- conferenceRecords[conferenceRecords$Conference != "FCS",] 
  
  records <- conferenceRecords
  
  Adjusted.SoS <- c(1:length(records$TEAM))
  
  First.Pass.SoS <- c(1:length(records$TEAM))
  
  records <- cbind(records, First.Pass.SoS)
  records <- cbind(records, Adjusted.SoS)
  
  
  records[records$Conference == "FCS",]$First.Pass.SoS <- 0.5
  records[records$Conference == "FCS",]$Adjusted.SoS <- 0.385 
  
  homeOffset <- 1 #0.95
  awayOffset <- 1 #1.05
  
  for(i in 1:length(teams$TEAM))
  {  
    homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
    awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
    homeOppRecords <- records[match(homeGames$Visitor, records$TEAM),] 
    awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
    
    homeScores <- homeOffset*(conferenceRecords[match(homeOppRecords$TEAM,conferenceRecords$TEAM),]$ConferenceOffset)*(homeOppRecords$OVERALL.W / (homeOppRecords$OVERALL.W+homeOppRecords$OVERALL.L))
    awayScores <- awayOffset* conferenceRecords[match(awayOppRecords$TEAM,conferenceRecords$TEAM),]$ConferenceOffset*(awayOppRecords$OVERALL.W / (awayOppRecords$OVERALL.W+awayOppRecords$OVERALL.L))
    
    homeScores <- homeScores[!is.na(homeScores)]
    awayScores <- awayScores[!is.na(awayScores)]
    
    
    records[records$TEAM == as.character(teams[i,]$TEAM),]$First.Pass.SoS <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
  }
  
  for(i in 1:length(teams$TEAM))
  {  
    homeGames <- schedules[schedules$Home.Team == as.character(teams[i,]$TEAM),]
    awayGames <- schedules[schedules$Visitor == as.character(teams[i,]$TEAM),]
    homeOppRecords <- records[match(homeGames$Visitor, records$TEAM),] 
    awayOppRecords <- records[match(awayGames$Home.Team, records$TEAM),] 
    
    homeScores <- homeOffset*(conferenceRecords[match(homeOppRecords$TEAM,conferenceRecords$TEAM),]$ConferenceOffset)*homeOppRecords$First.Pass.SoS
    awayScores <- awayOffset*conferenceRecords[match(awayOppRecords$TEAM,conferenceRecords$TEAM),]$ConferenceOffset*awayOppRecords$First.Pass.SoS
    
    homeScores <- homeScores[!is.na(homeScores)]
    awayScores <- awayScores[!is.na(awayScores)]
    
    secondPass <- (sum(homeScores) + sum(awayScores))/(length(homeScores) + length(awayScores))
    
    records[records$TEAM == as.character(teams[i,]$TEAM),]$Adjusted.SoS<- ((2*records[records$TEAM == as.character(teams[i,]$TEAM),]$First.Pass.SoS)+secondPass)/3
  }
  
  
  maxSoS <- max(records$Adjusted.SoS)
  minSoS <- min(records[records$Adjusted.SoS!=0.385,]$Adjusted.SoS)
  
  SoS.Score <- (records$Adjusted.SoS - minSoS) / (maxSoS-minSoS)
  
  records <- cbind(records, SoS.Score)
  
  SoS.Ranking <- length(teams$TEAM) - ((length(teams$TEAM)-1)*records$SoS.Score)
  
  return(records <- cbind(records, SoS.Ranking))
}
FBS.SoS <- function(){
 
  records <- All.SoS()
return(FBS.Records <- records[records$Conference != "FCS",])

}