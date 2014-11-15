elo.ranking <-function()
{
  schedules <- read.csv(file = 'data\\gamesPlayed.csv', sep=',', header=T)
  box.scores <- read.csv(file = 'data\\box_data.csv', sep=',', header=T)
  
  FBS.Records <- read.csv(file = 'data\\records.csv', sep=',', header=T)
  FBS.Record.List <- FBS.Records[FBS.Records$TEAM %in% as.character(box.scores$TEAM),]
  TEAM <- FBS.Record.List$TEAM
  ELO <- rep(1400,  length(FBS.Record.List$TEAM))
  GAMES <- rep(0,  length(FBS.Record.List$TEAM))
  RANK <-  rep(0, length(FBS.Record.List$TEAM))
  
  rankings <- data.frame(TEAM, ELO, GAMES, RANK)
  
  for(i in 1:(length(schedules$Home.Team)))
  {
    box <- box.scores[box.scores$TEAM == as.character(schedules[i,]$Home.Team),]
    
    box <- box[box$OPP == as.character(schedules[i,]$Visitor),]
    
    if(!(nrow(box) == 0))
    {
      home <- rankings[rankings$TEAM == as.character(box$TEAM),]
      away <- rankings[rankings$TEAM == as.character(box$OPP),]
      Rh <- 10^(home$ELO/400)
      Ra <- 10^(away$ELO/400)
      Eh <- Rh/(Rh+Ra)
      Ea <- Ra/(Rh+Ra)
      Sh <- 1
      Sa <- 0
      if(box$POINTS.FOR < box$POINTS.AGAINST)
      {
        Sh <- 0
        Sa <- 1
      }
      else if(box$POINTS.FOR == box$POINTS.AGAINST)
      {      
        Sh <- 0.5
        Sa <- 0.5
      }
      home$ELO <- home$ELO + ((800/(20+home$GAMES))*(Sh-Eh)) 
      away$ELO <- away$ELO + ((800/(20+away$GAMES))*(Sa-Ea)) 
      home$GAMES <- home$GAMES + 1
      away$GAMES <- away$GAMES + 1
      rankings[rankings$TEAM == as.character(box$TEAM),] <- home
      rankings[rankings$TEAM == as.character(box$OPP),] <- away
    }
  }
  
   second.pass.rankings <- rankings
#   second.pass.rankings$GAMES <- 0
#   for(i in (length(schedules$Home.Team)):1)
#   {
#     box <- box.scores[box.scores$TEAM == as.character(schedules[i,]$Home.Team),]
#     
#     box <- box[box$OPP == as.character(schedules[i,]$Visitor),]
#     
#     if(!(nrow(box) == 0))
#     {
#       home <- second.pass.rankings[second.pass.rankings$TEAM == as.character(box$TEAM),]
#       away <- second.pass.rankings[second.pass.rankings$TEAM == as.character(box$OPP),]
#       Rh <- 10^(home$ELO/400)
#       Ra <- 10^(away$ELO/400)
#       Eh <- Rh/(Rh+Ra)
#       Ea <- Ra/(Rh+Ra)
#       Sh <- 1
#       Sa <- 0
#       if(box$POINTS.FOR < box$POINTS.AGAINST)
#       {
#         Sh <- 0
#         Sa <- 1
#       }
#       else if(box$POINTS.FOR == box$POINTS.AGAINST)
#       {      
#         Sh <- 0.5
#         Sa <- 0.5
#       }
#       home$ELO <- home$ELO + ((800/(20+home$GAMES))*(Sh-Eh)) 
#       away$ELO <- away$ELO + ((800/(20+away$GAMES))*(Sa-Ea)) 
#       home$GAMES <- home$GAMES + 1
#       away$GAMES <- away$GAMES + 1
#       second.pass.rankings[second.pass.rankings$TEAM == as.character(box$TEAM),] <- home
#       second.pass.rankings[second.pass.rankings$TEAM == as.character(box$OPP),] <- away
#     }
#   }
#   
  rankings$RANK <- 1 + length(rankings$ELO) - rank(rankings$ELO)
  second.pass.rankings$RANK <- 1 + length(second.pass.rankings$ELO) - rank(second.pass.rankings$ELO)
  return(second.pass.rankings)
}

elo.predict<-function(teamA, teamB)
{
  elos <- elo.ranking()
  eloA <- elos[elos$TEAM == teamA,]$ELO
  eloB <- elos[elos$TEAM == teamB,]$ELO
  return (1/(1+10^((eloB-eloA)/400))) 
}