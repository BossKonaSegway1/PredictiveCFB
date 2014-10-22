source('C:\\Users\\mb24244\\Documents\\football anaylsis\\SoSplayed.r')
source('C:\\Users\\mb24244\\Documents\\football anaylsis\\complex_predictor_function.r')
box.data <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\box_data.csv', sep=',', header=T)

FBS.Record.List <- FBS.Records[FBS.Records$TEAM %in% as.character(box.data$TEAM),]

TEAM <- FBS.Record.List$TEAM
WIN.PERC <- FBS.Record.List$OVERALL.W/(FBS.Record.List$OVERALL.W+FBS.Record.List$OVERALL.L)
SoS.MULT <- 0.75 + (FBS.Record.List$SoS.Score/4)
SIM.WINS <-  rep(0, length(FBS.Record.List$TEAM))
SIM.LOSSES <- rep(0, length(FBS.Record.List$TEAM))
SIM.PERC <-  rep(0, length(FBS.Record.List$TEAM))
RANK.SCORE <-  rep(0, length(FBS.Record.List$TEAM))
RANK <-  rep(0, length(FBS.Record.List$TEAM))

rankings <- data.frame(TEAM, WIN.PERC, SoS.MULT, SIM.WINS, SIM.LOSSES, SIM.PERC, RANK.SCORE)

rankings <- rankings[!is.na(rankings$TEAM),]

count <- 20

for(l in 1:(length(rankings$TEAM)-1))
{
  for(m in (l+1):length(rankings$TEAM))
  {
      hName <- as.character(rankings[l,]$TEAM)
      aName <- as.character(rankings[m,]$TEAM)
      
      ranking.temp <- neutral.complex.predict(hName, aName, count)
      
      rankings[l,]$SIM.WINS <- rankings[l,]$SIM.WINS + ranking.temp[1,]$WINS
      rankings[l,]$SIM.LOSSES <- rankings[l,]$SIM.LOSSES + ranking.temp[2,]$WINS
      
      rankings[m,]$SIM.WINS <- rankings[m,]$SIM.WINS + ranking.temp[2,]$WINS                                
      rankings[m,]$SIM.LOSSES <- rankings[m,]$SIM.LOSSES + ranking.temp[1,]$WINS
  }
}

rankings$SIM.PERC <- rankings$SIM.WINS / (rankings$SIM.WINS + rankings$SIM.LOSSES)
rankings$RANK.SCORE <- rankings$SIM.PERC*rankings$WIN.PERC*rankings$SoS.MULT

rankings$RANK <- (length(rankings$TEAM)+1) - rank(rankings$RANK.SCORE)
