source('C:\\Users\\mb24244\\Documents\\football anaylsis\\SoS.r')
SoS <- records
offense <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\offense.csv', sep=',', header=T)
defense <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\defense.csv', sep=',', header=T)
records <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\records.csv', sep=',', header=T)
box.scores <- read.csv(file = 'C:\\Users\\mb24244\\Documents\\football anaylsis\\box_data.csv', sep=',', header=T)

averageSoS <- sum(SoS$Adjusted.SoS)/length(SoS$Adjusted.SoS)

combine <- merge(offense, defense, by="TEAM")
combine <- merge(combine, records, by="TEAM")
average.YDS.G <- sum(offense$YDS.G)/length(offense$YDS.G)
deviation.YDS.G <- sd(offense$YDS.G)
average.D.YDS.G <- sum(defense$D.YDS.G)/length(defense$D.YDS.G)
deviation.D.YDS.G <- sd(defense$D.YDS.G)

home <- combine[combine$TEAM == "Florida State",]
away <- combine[combine$TEAM == "Notre Dame",]

homeOpps <- box.scores[box.scores$TEAM == "Florida State",]
awayOpps <- box.scores[box.scores$TEAM == "Notre Dame",]


homePassOadj <- 0
homePassDadj <- 0
homeRunOadj <- 0
homeRunDadj <- 0
homePFadj <- 0
homePAadj <- 0
homeTurnoverForAve <- sum(homeOpps$TURNOVERS.FOR)/length(homeOpps$TURNOVERS.FOR)                      
homeTurnoverAgainstAve <- sum(homeOpps$TURNOVERS.AGAINST)/length(homeOpps$TURNOVERS.AGAINST)
homePenaltyAgainstAve <- sum(homeOpps$PENALTY.YARDS.AGAINST)/length(homeOpps$PENALTY.YARDS.AGAINST)

awayPassOadj <- 0
awayPassDadj <- 0
awayRunOadj <- 0
awayRunDadj <- 0
awayPFadj <- 0
awayPAadj <- 0

awayTurnoverForAve <- sum(awayOpps$TURNOVERS.FOR)/length(awayOpps$TURNOVERS.FOR)
awayTurnoverAgainstAve <-  sum(awayOpps$TURNOVERS.AGAINST)/length(awayOpps$TURNOVERS.AGAINST)
awayPenaltyAgainstAve <-  sum(awayOpps$PENALTY.YARDS.AGAINST)/length(awayOpps$PENALTY.YARDS.AGAINST)

homePassOoff <- c(1:length(homeOpps$OPP))
homePassDoff <- c(1:length(homeOpps$OPP))
homeRunOoff <- c(1:length(homeOpps$OPP))
homeRunDoff <- c(1:length(homeOpps$OPP))
homePFoff <- c(1:length(homeOpps$OPP))
homePAoff <- c(1:length(homeOpps$OPP))

awayPassOoff <- c(1:length(awayOpps$OPP))
awayPassDoff <- c(1:length(awayOpps$OPP))
awayRunOoff <- c(1:length(awayOpps$OPP))
awayRunDoff <- c(1:length(awayOpps$OPP))
awayPFoff <- c(1:length(awayOpps$OPP))
awayPAoff <- c(1:length(awayOpps$OPP))

for(i in 1:length(homeOpps$OPP))
{

  oppName <- as.character(homeOpps[i,]$OPP)
  SoSoff <- 1 + (averageSoS - SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
  opp <- combine[combine$TEAM == oppName,]
  homePassOoff[i] <- SoSoff*(homeOpps[i,]$P.YARDS.FOR)/opp$D.P.YDS.G
  homePassDoff[i] <- SoSoff*(homeOpps[i,]$P.YARDS.AGAINST)/opp$P.YDS.G
  homeRunOoff[i] <- SoSoff*(homeOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
  homeRunDoff[i] <- SoSoff*(homeOpps[i,]$R.YARDS.AGAINST)/opp$R.YDS.G
  homePFoff[i] <- SoSoff*(homeOpps[i,]$POINTS.FOR)/opp$D.PTS.G
  homePAoff[i] <- SoSoff*(homeOpps[i,]$POINTS.AGAINST)/opp$PTS.G
}


for(i in 1:length(awayOpps$OPP))
{
  oppName <- as.character(awayOpps[i,]$OPP)
  opp <- combine[combine$TEAM == oppName,]
  SoSoff <- 1 + (averageSoS - SoS[SoS$TEAM == oppName,]$Adjusted.SoS)
  awayPassOoff[i] <-  SoSoff*(awayOpps[i,]$P.YARDS.FOR)/opp$D.P.YDS.G
  awayPassDoff[i] <-  SoSoff*(awayOpps[i,]$P.YARDS.AGAINST)/opp$P.YDS.G
  awayRunOoff[i] <-  SoSoff*(awayOpps[i,]$R.YARDS.F)/opp$D.R.YDS.G
  awayRunDoff[i] <-  SoSoff*(awayOpps[i,]$R.YARDS.AGAINST)/opp$R.YDS.G
  awayPFoff[i] <-  SoSoff*(awayOpps[i,]$POINTS.FOR)/opp$D.PTS.G
  awayPAoff[i] <-  SoSoff*(awayOpps[i,]$POINTS.AGAINST)/opp$PTS.G
}

homePassOadj <- sum(homePassOoff)/length(homePassOoff)
homePassDadj <-  sum(homePassDoff)/length(homePassDoff)
homeRunOadj <-  sum(homeRunOoff)/length(homeRunOoff)
homeRunDadj <-  sum(homeRunDoff)/length(homeRunDoff)
homePFadj <-  sum(homePFoff)/length(homePFoff)
homePAadj <-  sum(homePAoff)/length(homePAoff)

awayPassOadj <- sum(awayPassOoff)/length(awayPassOoff)
awayPassDadj <-  sum(awayPassDoff)/length(awayPassDoff)
awayRunOadj <-  sum(awayRunOoff)/length(awayRunOoff)
awayRunDadj <-  sum(awayRunDoff)/length(awayRunDoff)
awayPFadj <-  sum(awayPFoff)/length(awayPFoff)
awayPAadj <-  sum(awayPAoff)/length(awayPAoff)

homeSoSoffset <- 1 + (SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS - SoS[SoS$TEAM == as.character(away$TEAM),]$Adjusted.SoS)
awaySoSoffset <- 1 + (SoS[SoS$TEAM == as.character(away$TEAM),].SoS - SoS[SoS$TEAM == as.character(home$TEAM),]$Adjusted.SoS)
homeExpectedPass <- c(homeSoSoffset*homePassOadj*away$D.P.YDS.G, awaySoSoffset*awayPassDadj*home$P.YDS.G)
homeExpectedRun <- c(homeSoSoffset*homeRunOadj*away$D.R.YDS.G, awaySoSoffset*awayRunDadj*home$R.YDS.G)

awayExpectedPass <- c(homeSoSoffset*homePassDadj*away$P.YDS.G, awaySoSoffset*awayPassOadj*home$D.P.YDS.G)
awayExpectedRun <- c(homeSoSoffset*homeRunDadj*away$R.YDS.G, awaySoSoffset*awayRunOadj*home$D.R.YDS.G)

homePointsFor <- c(homeSoSoffset*homePFadj*away$D.PTS.G, awaySoSoffset*awayPAadj*home$PTS.G)
awayPointsFor <- c(homeSoSoffset*homePAadj*away$PTS.G, awaySoSoffset*awayPFadj*home$D.PTS.G)

homeExpectedPass[3] <- ((1.1*homeExpectedPass[1])+(0.9*homeExpectedPass[2]))/2 
homeExpectedRun[3] <- ((1.1*homeExpectedRun[1])+(0.9*homeExpectedRun[2]))/2 

awayExpectedPass[3] <- ((1.1*awayExpectedPass[1])+(0.9*awayExpectedPass[2]))/2 
awayExpectedRun[3] <- ((1.1*awayExpectedRun[1])+(0.9*awayExpectedRun[2]))/2 

homePointsFor[3] <- ((1.1*homePointsFor[1])+(0.9*homePointsFor[2]))/2 
awayPointsFor[3] <- ((1.1*awayPointsFor[1])+(0.9*awayPointsFor[2]))/2 

homeTurnovers <- (0.65*awayTurnoverForAve+1.35*homeTurnoverAgainstAve)/2
awayTurnovers <- (0.7*homeTurnoverForAve+1.3*awayTurnoverAgainstAve)/2

turnoverMargin <- homeTurnovers - awayTurnovers

penaltyMargin <- 1.05*awayPenaltyAgainstAve - .95*homePenaltyAgainstAve

yardageMargin <- (homeExpectedRun[3]+homeExpectedPass[3])-(awayExpectedPass[3]+awayExpectedRun[3])

homePointsFor[4] <- 3+(1.5*(penaltyMargin/100.0))+(1.75*(yardageMargin/100.0))+(-2.75*turnoverMargin)+homePointsFor[3]
awayPointsFor[4] <- (-1.5*(penaltyMargin/100.0))+(-1.75*(yardageMargin/100.0))+(2.75*turnoverMargin)+awayPointsFor[3]
