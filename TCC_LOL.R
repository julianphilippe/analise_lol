
#rm(list = "players")


#install.packages('caret')

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tibble)
library(caret)

base <- read.csv("F://LOL/statistcs_games2.csv", sep = ";")


colnames(base)


info_times <- base[1:21]

id <- base[1:1]

players <- base[22:591]

info_players <- bind_cols(id,players) 


info_playes_Sum <- info_players %>%
  mutate(W_kills = W_P1_kills + W_P2_kills + W_P3_kills + W_P4_kills + W_P5_kills,
         W_deaths = W_P1_deaths + W_P2_deaths + W_P3_deaths + W_P4_deaths + W_P5_deaths,
         W_assists = W_P1_assists + W_P2_assists + W_P3_assists + W_P4_assists + W_P5_assists,
         W_minionsKilled = W_P1_minionsKilled + W_P2_minionsKilled + W_P3_minionsKilled + W_P4_minionsKilled + W_P5_minionsKilled,
         W_neutralMinionsKilled = W_P1_neutralMinionsKilled + W_P2_neutralMinionsKilled + W_P3_neutralMinionsKilled + W_P4_neutralMinionsKilled + W_P5_neutralMinionsKilled,
         W_neutralMinionsKilledTeamJungle = W_P1_neutralMinionsKilledTeamJungle + W_P2_neutralMinionsKilledTeamJungle + W_P3_neutralMinionsKilledTeamJungle + W_P4_neutralMinionsKilledTeamJungle + W_P5_neutralMinionsKilledTeamJungle,
         W_neutralMinionsKilledEnemyJungle = W_P1_neutralMinionsKilledEnemyJungle + W_P2_neutralMinionsKilledEnemyJungle + W_P3_neutralMinionsKilledEnemyJungle + W_P4_neutralMinionsKilledEnemyJungle + W_P5_neutralMinionsKilledEnemyJungle,
         W_goldEarned = W_P1_goldEarned + W_P2_goldEarned + W_P3_goldEarned + W_P4_goldEarned + W_P5_goldEarned,
         W_goldSpent = W_P1_goldSpent + W_P2_goldSpent + W_P3_goldSpent + W_P4_goldSpent + W_P5_goldSpent,
         W_wardsPlaced = W_P1_wardsPlaced + W_P2_wardsPlaced + W_P3_wardsPlaced + W_P4_wardsPlaced + W_P5_wardsPlaced,
         W_sightWardsBoughtInGame = W_P1_sightWardsBoughtInGame + W_P2_sightWardsBoughtInGame + W_P3_sightWardsBoughtInGame + W_P4_sightWardsBoughtInGame + W_P5_sightWardsBoughtInGame,
         W_visionWardsBoughtInGame = W_P1_visionWardsBoughtInGame + W_P2_visionWardsBoughtInGame + W_P3_visionWardsBoughtInGame + W_P4_visionWardsBoughtInGame + W_P5_visionWardsBoughtInGame,
         W_wardsKilled = W_P1_wardsKilled + W_P2_wardsKilled + W_P3_wardsKilled + W_P4_wardsKilled + W_P5_wardsKilled,
         W_doubleKills = W_P1_doubleKills + W_P2_doubleKills + W_P3_doubleKills + W_P4_doubleKills + W_P5_doubleKills,
         W_tripleKills = W_P1_tripleKills + W_P2_tripleKills + W_P3_tripleKills + W_P4_tripleKills + W_P5_tripleKills,
         W_quadraKills = W_P1_quadraKills + W_P2_quadraKills + W_P3_quadraKills + W_P4_quadraKills + W_P5_quadraKills,
         W_pentaKills = W_P1_pentaKills + W_P2_pentaKills + W_P3_pentaKills + W_P4_pentaKills + W_P5_pentaKills,
         W_killingSprees = W_P1_killingSprees + W_P2_killingSprees + W_P3_killingSprees + W_P4_killingSprees + W_P5_killingSprees,
         
         L_kills = L_P1_kills + L_P2_kills + L_P3_kills + L_P4_kills + L_P5_kills,
         L_deaths = L_P1_deaths + L_P2_deaths + L_P3_deaths + L_P4_deaths + L_P5_deaths,
         L_assists = L_P1_assists + L_P2_assists + L_P3_assists + L_P4_assists + L_P5_assists,
         L_minionsKilled = L_P1_minionsKilled + L_P2_minionsKilled + L_P3_minionsKilled + L_P4_minionsKilled + L_P5_minionsKilled,
         L_neutralMinionsKilled = L_P1_neutralMinionsKilled + L_P2_neutralMinionsKilled + L_P3_neutralMinionsKilled + L_P4_neutralMinionsKilled + L_P5_neutralMinionsKilled,
         L_neutralMinionsKilledTeamJungle = L_P1_neutralMinionsKilledTeamJungle + L_P2_neutralMinionsKilledTeamJungle + L_P3_neutralMinionsKilledTeamJungle + L_P4_neutralMinionsKilledTeamJungle + L_P5_neutralMinionsKilledTeamJungle,
         L_neutralMinionsKilledEnemyJungle = L_P1_neutralMinionsKilledEnemyJungle + L_P2_neutralMinionsKilledEnemyJungle + L_P3_neutralMinionsKilledEnemyJungle + L_P4_neutralMinionsKilledEnemyJungle + L_P5_neutralMinionsKilledEnemyJungle,
         L_goldEarned = L_P1_goldEarned + L_P2_goldEarned + L_P3_goldEarned + L_P4_goldEarned + L_P5_goldEarned,
         L_goldSpent = L_P1_goldSpent + L_P2_goldSpent + L_P3_goldSpent + L_P4_goldSpent + L_P5_goldSpent,
         L_wardsPlaced = L_P1_wardsPlaced + L_P2_wardsPlaced + L_P3_wardsPlaced + L_P4_wardsPlaced + L_P5_wardsPlaced,
         L_sightWardsBoughtInGame = L_P1_sightWardsBoughtInGame + L_P2_sightWardsBoughtInGame + L_P3_sightWardsBoughtInGame + L_P4_sightWardsBoughtInGame + L_P5_sightWardsBoughtInGame,
         L_visionWardsBoughtInGame = L_P1_visionWardsBoughtInGame + L_P2_visionWardsBoughtInGame + L_P3_visionWardsBoughtInGame + L_P4_visionWardsBoughtInGame + L_P5_visionWardsBoughtInGame,
         L_wardsKilled = L_P1_wardsKilled + L_P2_wardsKilled + L_P3_wardsKilled + L_P4_wardsKilled + L_P5_wardsKilled,
         L_doubleKills = L_P1_doubleKills + L_P2_doubleKills + L_P3_doubleKills + L_P4_doubleKills + L_P5_doubleKills,
         L_tripleKills = L_P1_tripleKills + L_P2_tripleKills + L_P3_tripleKills + L_P4_tripleKills + L_P5_tripleKills,
         L_quadraKills = L_P1_quadraKills + L_P2_quadraKills + L_P3_quadraKills + L_P4_quadraKills + L_P5_quadraKills,
         L_pentaKills = L_P1_pentaKills + L_P2_pentaKills + L_P3_pentaKills + L_P4_pentaKills + L_P5_pentaKills,
         L_killingSprees = L_P1_killingSprees + L_P2_killingSprees + L_P3_killingSprees + L_P4_killingSprees + L_P5_killingSprees) %>%
  
  select(matchId, W_kills, W_deaths, W_assists, W_minionsKilled, W_neutralMinionsKilled, W_neutralMinionsKilledTeamJungle, W_neutralMinionsKilledEnemyJungle, W_goldEarned, W_goldSpent, W_wardsPlaced,
         W_sightWardsBoughtInGame, W_visionWardsBoughtInGame, W_wardsKilled, W_doubleKills, W_tripleKills, W_quadraKills, W_pentaKills, W_killingSprees,
         
         L_kills, L_deaths, L_assists, L_minionsKilled, L_neutralMinionsKilled, L_neutralMinionsKilledTeamJungle, L_neutralMinionsKilledEnemyJungle, L_goldEarned, L_goldSpent, L_wardsPlaced,
         L_sightWardsBoughtInGame, L_visionWardsBoughtInGame, L_wardsKilled, L_doubleKills, L_tripleKills, L_quadraKills, L_pentaKills, L_killingSprees)



baseFinal <- inner_join(info_times, info_playes_Sum)

baseFinal <- mutate(baseFinal, durationMin = minute(strptime(baseFinal$matchDuration,  format = '%T')))

baseWinner <- baseFinal %>%
  select(matchId, matchDuration, durationMin, W_firstBlood, W_firstTower, W_firstDragon, W_firstInhibitor, W_firstBaron, W_dragonKills, W_inhibitorKills, W_towerKills, W_baronKills, W_kills, W_deaths, W_assists,
         W_minionsKilled, W_neutralMinionsKilled, W_neutralMinionsKilledTeamJungle, W_neutralMinionsKilledEnemyJungle, W_goldEarned, W_goldSpent, W_wardsPlaced, W_sightWardsBoughtInGame, W_visionWardsBoughtInGame,
         W_wardsKilled, W_doubleKills, W_tripleKills, W_quadraKills, W_pentaKills, W_killingSprees) %>%
  rename(firstBlood = W_firstBlood, firstTower = W_firstTower, firstDragon = W_firstDragon, firstInhibitor = W_firstInhibitor, firstBaron = W_firstBaron, dragonKills = W_dragonKills, 
         inhibitorKills = W_inhibitorKills, towerKills = W_towerKills, baronKills = W_baronKills, kills = W_kills, deaths = W_deaths, assists = W_assists, minionsKilled = W_minionsKilled, 
         neutralMinionsKilled = W_neutralMinionsKilled, neutralMinionsKilledTeamJungle = W_neutralMinionsKilledTeamJungle, neutralMinionsKilledEnemyJungle = W_neutralMinionsKilledEnemyJungle, 
         goldEarned = W_goldEarned, goldSpent = W_goldSpent, wardsPlaced = W_wardsPlaced, sightWardsBoughtInGame = W_sightWardsBoughtInGame, visionWardsBoughtInGame = W_visionWardsBoughtInGame,
         wardsKilled = W_wardsKilled, doubleKills = W_doubleKills, tripleKills = W_tripleKills, quadraKills = W_quadraKills, pentaKills = W_pentaKills, killingSprees = W_killingSprees) %>%
  mutate(teamWinner = 1, 
         teamSide = case_when(baseFinal$teamWinner == 100 ~ 'blue',
                              baseFinal$teamWinner == 200 ~ 'purple'),
         duration_0_10 = ifelse(baseFinal$durationMin <= 10, 1, 0), 
         duration_10_20 = ifelse(baseFinal$durationMin > 10 & baseFinal$durationMin <= 20, 1, 0),
         duration_20_30 = ifelse(baseFinal$durationMin > 20 & baseFinal$durationMin <= 30, 1, 0),
         duration_30_40 = ifelse(baseFinal$durationMin > 30 & baseFinal$durationMin <= 40, 1, 0),
         duration_40_more = ifelse(baseFinal$durationMin > 40, 1, 0),
         firstBlood = ifelse(firstBlood == 'True', 1, 0),
         firstTower = ifelse(firstTower == 'True', 1, 0),
         firstDragon = ifelse(firstDragon == 'True', 1, 0),
         firstInhibitor = ifelse(firstInhibitor == 'True', 1, 0),
         firstBaron = ifelse(firstBaron == 'True', 1, 0),
         teamBlue = ifelse(teamSide == 'blue', 1, 0),
         teamPurple = ifelse(teamSide == 'purple', 1, 0))


baseLoser <- baseFinal %>%
  select(matchId, matchDuration, durationMin, L_firstBlood, L_firstTower, L_firstDragon, L_firstInhibitor, L_firstBaron, L_dragonKills, L_inhibitorKills, L_towerKills, L_baronKills, L_kills, L_deaths, L_assists,
         L_minionsKilled, L_neutralMinionsKilled, L_neutralMinionsKilledTeamJungle, L_neutralMinionsKilledEnemyJungle, L_goldEarned, L_goldSpent, L_wardsPlaced, L_sightWardsBoughtInGame, L_visionWardsBoughtInGame,
         L_wardsKilled, L_doubleKills, L_tripleKills, L_quadraKills, L_pentaKills, L_killingSprees) %>%
  rename(firstBlood = L_firstBlood, firstTower = L_firstTower, firstDragon = L_firstDragon, firstInhibitor = L_firstInhibitor, firstBaron = L_firstBaron, dragonKills = L_dragonKills, 
         inhibitorKills = L_inhibitorKills, towerKills = L_towerKills, baronKills = L_baronKills, kills = L_kills, deaths = L_deaths, assists = L_assists, minionsKilled = L_minionsKilled, 
         neutralMinionsKilled = L_neutralMinionsKilled, neutralMinionsKilledTeamJungle = L_neutralMinionsKilledTeamJungle, neutralMinionsKilledEnemyJungle = L_neutralMinionsKilledEnemyJungle, 
         goldEarned = L_goldEarned, goldSpent = L_goldSpent, wardsPlaced = L_wardsPlaced, sightWardsBoughtInGame = L_sightWardsBoughtInGame, visionWardsBoughtInGame = L_visionWardsBoughtInGame,
         wardsKilled = L_wardsKilled, doubleKills = L_doubleKills, tripleKills = L_tripleKills, quadraKills = L_quadraKills, pentaKills = L_pentaKills, killingSprees = L_killingSprees) %>%
  mutate(teamWinner = 0, 
         teamSide = case_when(baseFinal$teamWinner == 200 ~ 'blue',
                              baseFinal$teamWinner == 100 ~ 'purple'),
         duration_0_10 = ifelse(baseFinal$durationMin <= 10, 1, 0), 
         duration_10_20 = ifelse(baseFinal$durationMin > 10 & baseFinal$durationMin <= 20, 1, 0),
         duration_20_30 = ifelse(baseFinal$durationMin > 20 & baseFinal$durationMin <= 30, 1, 0),
         duration_30_40 = ifelse(baseFinal$durationMin > 30 & baseFinal$durationMin <= 40, 1, 0),
         duration_40_more = ifelse(baseFinal$durationMin > 40, 1, 0),
         firstBlood = ifelse(firstBlood == 'True', 1, 0),
         firstTower = ifelse(firstTower == 'True', 1, 0),
         firstDragon = ifelse(firstDragon == 'True', 1, 0),
         firstInhibitor = ifelse(firstInhibitor == 'True', 1, 0),
         firstBaron = ifelse(firstBaron == 'True', 1, 0),
         teamBlue = ifelse(teamSide == 'blue', 1, 0),
         teamPurple = ifelse(teamSide == 'purple', 1, 0))
  


baseTotal <- rbind(baseWinner, baseLoser)


baseTCC <- baseTotal %>%
  select(matchId, matchDuration, durationMin, teamSide, teamWinner, duration_0_10, duration_10_20, duration_20_30, duration_30_40, duration_40_more, teamBlue, teamPurple, firstBlood, firstTower, firstInhibitor,
         firstDragon, firstBaron, towerKills, inhibitorKills, dragonKills, baronKills, kills, deaths, assists, minionsKilled, neutralMinionsKilled, neutralMinionsKilledTeamJungle, neutralMinionsKilledEnemyJungle,
         goldEarned, goldSpent, wardsPlaced, wardsKilled, visionWardsBoughtInGame, doubleKills, tripleKills, quadraKills, pentaKills, killingSprees)

##analise explorat?ria variaveis qualitativas
#frequencia absoluta e relativa

teamWinner <- data.frame(variaveis = 'teamWinner', freqAbsoluta = table(baseTCC$teamWinner), freqRelativa = prop.table(table(baseTCC$teamWinner)))
teamBlue <- data.frame(variaveis = 'teamBlue',  freqAbsoluta = table(baseTCC$teamBlue), freqRelativa = prop.table(table(baseTCC$teamBlue)))
teamPurple <- data.frame(variaveis = 'teamPurple',  freqAbsoluta = table(baseTCC$teamPurple), freqRelativa = prop.table(table(baseTCC$teamPurple)))
duration_0_10 <- data.frame(variaveis = 'duration_0_10', freqAbsoluta = table(baseTCC$duration_0_10), freqRelativa = prop.table(table(baseTCC$duration_0_10)))
duration_10_20 <- data.frame(variaveis = 'duration_10_20', freqAbsoluta = table(baseTCC$duration_10_20), freqRelativa = prop.table(table(baseTCC$duration_10_20)))
duration_20_30 <- data.frame(variaveis = 'duration_20_30', freqAbsoluta = table(baseTCC$duration_20_30), freqRelativa = prop.table(table(baseTCC$duration_20_30)))
duration_30_40 <- data.frame(variaveis = 'duration_30_40', freqAbsoluta = table(baseTCC$duration_30_40), freqRelativa = prop.table(table(baseTCC$duration_30_40)))
duration_40_more <- data.frame(variaveis = 'duration_40_more', freqAbsoluta = table(baseTCC$duration_40_more), freqRelativa = prop.table(table(baseTCC$duration_40_more)))
firstBlood <- data.frame(variaveis = 'firstBlood', freqAbsoluta = table(baseTCC$firstBlood), freqRelativa = prop.table(table(baseTCC$firstBlood)))
firstTower <- data.frame(variaveis = 'firstTower', freqAbsoluta = table(baseTCC$firstTower), freqRelativa = prop.table(table(baseTCC$firstTower)))
firstInhibitor <- data.frame(variaveis = 'firstInhibitor', freqAbsoluta = table(baseTCC$firstInhibitor), freqRelativa = prop.table(table(baseTCC$firstInhibitor)))
firstDragon <- data.frame(variaveis = 'firstDragon', freqAbsoluta = table(baseTCC$firstDragon), freqRelativa = prop.table(table(baseTCC$firstDragon)))
firstBaron <- data.frame(variaveis = 'firstBaron', freqAbsoluta = table(baseTCC$firstBaron), freqRelativa = prop.table(table(baseTCC$firstBaron)))

#tabela com todas frequencias

freqVarsQuali <- bind_rows(teamWinner, teamBlue, teamPurple , duration_0_10, duration_10_20, duration_20_30, duration_30_40, duration_40_more,
                           firstBlood, firstTower, firstInhibitor, firstDragon, firstBaron)


freqVarsQuali <- data.frame( freqVarsQuali$variaveis, freqVarsQuali$freqAbsoluta.Var1, freqVarsQuali$freqAbsoluta.Freq, freqVarsQuali$freqRelativa.Freq)

str(freqVarsQuali)

freqVarsQuali <- freqVarsQuali %>%
  mutate(freqRelativa = round(freqVarsQuali$freqVarsQuali.freqRelativa.Freq, digits = 2))


##analise explorat?ria variaveis quantitativas

install.packages('psych')

library(psych)

options(scipen=999)

rm(list = 'variaveisQualitativas')


variaveisQuantitativas <- data_frame(towerKills = baseTCC$towerKills, inhibitorKills = baseTCC$inhibitorKills, dragonKills = baseTCC$dragonKills,
                                    baronKills = baseTCC$baronKills, kills = baseTCC$kills, deaths = baseTCC$deaths, assists = baseTCC$assists,
                                    minionsKilled = baseTCC$minionsKilled, neutralMinionsKilled = baseTCC$neutralMinionsKilled, neutralMinionsKilledTeamJungle = baseTCC$neutralMinionsKilledTeamJungle,
                                    neutralMinionsKilledEnemyJungle = baseTCC$neutralMinionsKilledEnemyJungle, goldEarned = baseTCC$goldEarned, goldSpent = baseTCC$goldSpent,
                                    wardsPlaced = baseTCC$wardsPlaced, wardsKilled = baseTCC$wardsKilled, visionWardsBoughtInGame = baseTCC$visionWardsBoughtInGame,
                                    doubleKills = baseTCC$doubleKills, tripleKills = baseTCC$tripleKills, quadraKills = baseTCC$quadraKills, pentaKills = baseTCC$pentaKills,
                                    killingSprees = baseTCC$killingSprees)



analiseVarQuant <- round(describe(variaveisQuantitativas, quant = c(.25,.75)), digits = 2)




describe(variaveisQuantitativas, quant = c(.25,.75))


View(analiseVarQuant)


#------------------------------Analise das variáveis-----------------------------------#

#------Duracao------#

as.tibble(baseTCC)

duracao <- baseTCC %>%
  select(matchId, teamWinner, durationMin) %>%
  ggplot(aes(x = durationMin)) +
  geom_histogram(fill = 'blue') +
  theme_bw()

duracao

#------------outliers------------#
#provavel desconxçao do jogo#
jogosRapidos <- baseTCC %>%
  filter(durationMin < 15) %>%
  ggplot(aes(x = durationMin)) +
  geom_histogram(fill = 'blue') +
  theme_bw()

#media de tempo de jogo
mean(baseTCC$durationMin)


#retirados dados de jogos menosres que 15 min

baseTCC <- baseTCC %>%
  filter(durationMin > 5)

duracao <- baseTCC %>%
  select(matchId, teamWinner, durationMin) %>%
  ggplot(aes(x = durationMin)) +
  geom_histogram(fill = 'blue') +
  theme_bw()



#------firstBlood, firstTower, firstInhibitor, firstDragon, firstBaron------#

rm(list = c('tb_blood'))


blood <- ggplot(baseTCC, aes(firstBlood)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") 

tower <- ggplot(baseTCC, aes(firstTower)) + 
  geom_bar(aes(fill = as.factor(teamWinner)))+
  theme(legend.position = "none") 

inhibitor <- ggplot(baseTCC, aes(firstInhibitor)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") 

dragon <- ggplot(baseTCC, aes(firstDragon)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none")

baron <- ggplot(baseTCC, aes(firstBaron)) +
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme()



layout <- matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
multiplot(blood, tower, inhibitor, dragon, baron, layout=layout)


#------------teamBlue, teamPurpple----------------#


teamBlue <- ggplot(baseTCC, aes(teamBlue)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") 

teamPurple <- ggplot(baseTCC, aes(teamPurple)) + 
  geom_bar(aes(fill = as.factor(teamWinner)))+
  theme(legend.position = "none") 


layout <- matrix(c(1,2),2,2,byrow=TRUE)
multiplot(teamBlue, teamPurple, layout=layout)




#------------towerKills, inhibitorKills, dragonKills, baronKills----------------#


tower <- ggplot(baseTCC, aes(baseTCC$towerKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'towerKills')

inhibitor <- ggplot(baseTCC, aes(baseTCC$inhibitorKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'inhibitorKills')

dragon <- ggplot(baseTCC, aes(baseTCC$dragonKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'dragonKills')

baron <- ggplot(baseTCC, aes(baseTCC$baronKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  labs(x = 'baronKills')


layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(tower, inhibitor, dragon, baron, layout=layout)




#------------Kills, deaths, assists----------------#


kills <- ggplot(baseTCC, aes(baseTCC$kills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'kills')

deaths <- ggplot(baseTCC, aes(baseTCC$deaths)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'deaths')

assists <- ggplot(baseTCC, aes(baseTCC$assists)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  labs(x = 'assists')



layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(kills, deaths, assists, layout=layout)



#------------minionsKilled, neutralMinionsKilled------------------------# 



minionsKilled <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$minionsKilled)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'minionsKilled')

neutralMinionsKilled <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$neutralMinionsKilled)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'neutralMinionsKilled')

layout <- matrix(c(1,1,2,2),2,2,byrow=TRUE)
multiplot(minionsKilled, neutralMinionsKilled, layout=layout)



minionsOut <- baseTCC %>%
  filter(minionsKilled < 1100)

as.tibble(minionsOut)

plot1 <- ggplot(minionsOut, aes(as.factor(teamWinner), minionsOut$minionsKilled)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'minionsKilled')
  
  



#--------------neutralMinionsKilledTeamJungle, neutralMinionsKilledEnemyJungle----------------#

neutralMinionsKilledTeamJungle <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$neutralMinionsKilledTeamJungle)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'neutralMinionsKilledTeamJungle')

neutralMinionsKilledEnemyJungle <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$neutralMinionsKilledEnemyJungle)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'neutralMinionsKilledEnemyJungle')


layout <- matrix(c(1,1,2,2),2,2,byrow=TRUE)
multiplot(neutralMinionsKilledTeamJungle, neutralMinionsKilledEnemyJungle, layout=layout)


#--------------goldEarned, goldSpent----------------#



goldEarned <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$goldEarned)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'goldEarned')

goldSpent <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$goldSpent)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'goldSpent')


layout <- matrix(c(1,1,2,2),2,2,byrow=TRUE)
multiplot(goldEarned, goldSpent, layout=layout)



#--------------wardsPlaced, wardsKilled, visionWardsBoughtInGame----------------#


wardsPlaced <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$wardsPlaced)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'wardsPlaced')

wardsKilled <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$wardsKilled)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'wardsKilled')

visionWardsBoughtInGame <- ggplot(baseTCC, aes(as.factor(teamWinner), baseTCC$visionWardsBoughtInGame)) + 
  geom_boxplot(aes(fill = as.factor(teamWinner))) +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'teamWinner', y = 'visionWardsBoughtInGame')


layout <- matrix(c(1,1,2,2,3,3),3,2,byrow=TRUE)
multiplot(wardsPlaced, wardsKilled, visionWardsBoughtInGame, layout=layout)



#--------------doubleKills, tripleKills, quadraKills, pentaKills, killingSprees----------------#



doubleKills <- ggplot(baseTCC, aes(baseTCC$doubleKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'doubleKills')

tripleKills <- ggplot(baseTCC, aes(baseTCC$tripleKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'tripleKills')

quadraKills <- ggplot(baseTCC, aes(baseTCC$quadraKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'quadraKills')

pentaKills <- ggplot(baseTCC, aes(baseTCC$pentaKills)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  theme(legend.position = "none") +
  labs(x = 'pentaKills')

killingSprees <- ggplot(baseTCC, aes(baseTCC$killingSprees)) + 
  geom_bar(aes(fill = as.factor(teamWinner))) +
  labs(x = 'killingSprees')


layout <- matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE)
multiplot(doubleKills, tripleKills, quadraKills, pentaKills, killingSprees, layout=layout)


#------------------------------modelagem-----------------------------------#


#---------------------ajustando base---------------------#

as.tibble(baseTCC)

baseTCC$teamWinner <- as.factor(baseTCC$teamWinner)
baseTCC$duration_0_10 <- as.factor(baseTCC$duration_0_10)
baseTCC$duration_10_20 <- as.factor(baseTCC$duration_10_20)
baseTCC$duration_20_30 <- as.factor(baseTCC$duration_20_30)
baseTCC$duration_30_40 <- as.factor(baseTCC$duration_30_40)
baseTCC$duration_40_more <- as.factor(baseTCC$duration_40_more)
baseTCC$teamBlue <- as.factor(baseTCC$teamBlue)
baseTCC$teamPurple <- as.factor(baseTCC$teamPurple)
baseTCC$firstBlood <- as.factor(baseTCC$firstBlood)
baseTCC$firstTower <- as.factor(baseTCC$firstTower)
baseTCC$firstInhibitor <- as.factor(baseTCC$firstInhibitor)
baseTCC$firstDragon <- as.factor(baseTCC$firstDragon)
baseTCC$firstBaron <- as.factor(baseTCC$firstBaron)

str(baseTCC)

baseModelagem <- tibble()
baseTrain <- tibble()
baseTest <- tibble()

#--------------------------------padronizando vanriáveis---------------------------#

baseModelagem <- baseTCC %>%
  dplyr::select(-matchId, -matchDuration, -durationMin, -teamSide, -duration_0_10)
  

#--------------------------criando base treinamento e teste-----------------------------#

set.seed(998)
inTraining <- createDataPartition(baseModelagem$teamWinner, p = .75, list = FALSE)
baseTrain <- baseModelagem[ inTraining,]
baseTest  <- baseModelagem[-inTraining,]


table(baseTrain$teamWinner)
table(baseTest$teamWinner)






#---------------------define cross validation-----------------------#

train_control <- trainControl(method = "cv", number = 5, repeats = 3)


head(train_control)


# regress?o log?stica

str(baseTrain)

gmlmodel <- train(as.factor(teamWinner)~ ., data=baseModelagem, trControl=train_control, method=c("LogitBoost"))

gmlmodel$modelInfo

predictions<- predict(gmlmodel,baseModelagem)
gmlmodelbinded <- cbind(baseModelagem,predictions)


# summarize results
confusionMatrix<- confusionMatrix(gmlmodelbinded$predictions,gmlmodelbinded$teamWinner)
confusionMatrix


#------------------GLM---------------------#


str(baseModelagem)

glmteste <- glm(teamWinner ~ duration_10_20 + duration_20_30 + duration_30_40 + duration_40_more + teamBlue + teamPurple + firstBlood +
                  firstTower + firstInhibitor + firstDragon + firstBaron + towerKills + inhibitorKills + dragonKills + baronKills + kills + deaths + assists + minionsKilled +
                  neutralMinionsKilled + neutralMinionsKilledTeamJungle + neutralMinionsKilledEnemyJungle + goldEarned + goldSpent + wardsPlaced + wardsKilled + 
                  visionWardsBoughtInGame  + doubleKills + tripleKills + quadraKills + pentaKills + killingSprees, data = baseTrain, family = binomial)

summary(glmteste)


stepwise <- step(glmteste, direction = "both")


summary(stepwise)


glmTreinamento <- glm(formula = teamWinner ~ duration_10_20 + duration_20_30 + 
                       duration_30_40 + teamBlue + firstTower + firstBaron + towerKills + 
                       dragonKills + baronKills + kills + deaths + minionsKilled + 
                       goldEarned + goldSpent + wardsPlaced + wardsKilled + killingSprees, 
                       family = binomial, data = baseTrain)


summary(glmTreinamento)

baseTrain$pred <- predict(glmTreinamento, type = "response")
baseTrain$predFinal <- case_when(baseTrain$pred >0.5 ~ 1,
                                 baseTrain$pred <= 0.5 ~ 0)


tab <- table(baseTrain$predFinal, baseTrain$teamWinner)

matrizConf <- confusionMatrix(tab)

as.table(matrizConf)
as.matrix(matrizConf)
as.matrix(matrizConf, what = "overall")
as.matrix(matrizConf, what = "classes")


#-----------------base teste---------------#

baseTest$pred <- predict(treinamento, newdata = baseTest, type = "response")
baseTest$predFinal <- case_when(baseTest$pred >0.5 ~ 1,
                                baseTest$pred <= 0.5 ~ 0)


tab <- table(baseTest$predFinal, baseTest$teamWinner)

matrizConf <- confusionMatrix(tab)

as.table(matrizConf)
as.matrix(matrizConf)
as.matrix(matrizConf, what = "overall")
as.matrix(matrizConf, what = "classes")


#------------------Transformando todas as variáveis em variáveis qualitativas---------------------#


str(baseModelagemQuali)

summary(baseModelagem$killingSprees)

ggplot(baseModelagemQuali, aes(baseModelagemQuali$killingSprees)) +
  geom_bar(aes(fill = as.factor(teamWinner)))

baseModelagemQuali <- baseModelagem %>%
  mutate(towerKills = as.factor(case_when(baseModelagem$towerKills <= 4 ~ "Até 4 torres",
                                          (baseModelagem$towerKills > 4 & baseModelagem$towerKills <= 8) ~ "Entre 5 e 8 torres",
                                          baseModelagem$towerKills > 8 ~ "Mais de 8 torres")),
         inhibitorKills = as.factor(case_when(baseModelagem$inhibitorKills <= 4 ~ "Até 4 inibidores",
                                              (baseModelagem$inhibitorKills > 4 & baseModelagem$inhibitorKills <= 8) ~ "Entre 5 e 8 inibidores",
                                              baseModelagem$inhibitorKills > 8 ~ "Mais de 8 inibidores")),
         dragonKills = as.factor(case_when(baseModelagem$dragonKills <= 3 ~ "Até 3 dragões",
                                           baseModelagem$dragonKills > 3 ~ "Mais de 3 dragões")),
         baronKills = as.factor(case_when(baseModelagem$baronKills <= 1 ~ "Até 1 barão",
                                          baseModelagem$baronKills > 1 ~ "Mais de 1 barão")),
         kills = as.factor(case_when(baseModelagem$kills <= 20 ~ "Até 20 kills",
                                     (baseModelagem$kills > 20 & baseModelagem$kills <= 40) ~ "Entre 21 e 40 kills",
                                     baseModelagem$kills > 40 ~ "Mais de 40 kills")),
         deaths = as.factor(case_when(baseModelagem$deaths <= 20 ~ "Até 20 mortes",
                                      (baseModelagem$deaths > 20 & baseModelagem$deaths <= 40) ~ "Entre 21 e 40 mortes",
                                      baseModelagem$deaths > 40 ~ "Mais de 40 mortes")),
         assists = as.factor(case_when(baseModelagem$assists <= 40 ~ "Até 40 assistências",
                                       (baseModelagem$assists > 40 & baseModelagem$assists <= 80) ~ "Entre 41 e 80 assistências",
                                       baseModelagem$assists > 80 ~ "Mais de 80 assistências")),
         minionsKilled = as.factor(case_when(baseModelagem$minionsKilled <= 350 ~ "Até 350 minionsKilled",
                                             (baseModelagem$minionsKilled > 350 & baseModelagem$minionsKilled <= 700) ~ "Entre 351 e 700 minionsKilled",
                                             (baseModelagem$minionsKilled > 700 & baseModelagem$minionsKilled <= 1050) ~ "Entre 701 e 1050 minionsKilled",
                                             baseModelagem$minionsKilled > 1050 ~ "Mais de 1050 minionsKilled")),
         neutralMinionsKilled = as.factor(case_when(baseModelagem$neutralMinionsKilled <= 80 ~ "Até 80 neutralMinionsKilled",
                                                    (baseModelagem$neutralMinionsKilled > 80 & baseModelagem$neutralMinionsKilled <= 160) ~ "Entre 81 e 160 neutralMinionsKilled",
                                                    baseModelagem$neutralMinionsKilled > 160 ~ "Mais de 160 neutralMinionsKilled")),
         neutralMinionsKilledTeamJungle = as.factor(case_when(baseModelagem$neutralMinionsKilledTeamJungle <= 75 ~ "Até 75 neutralMinionsKilledTeamJungle",
                                                              baseModelagem$neutralMinionsKilledTeamJungle > 75 ~ "Mais de 75 neutralMinionsKilledTeamJungle")),
         neutralMinionsKilledEnemyJungle = as.factor(case_when(baseModelagem$neutralMinionsKilledEnemyJungle <= 50 ~ "Até 50 neutralMinionsKilledEnemyJungle",
                                                               baseModelagem$neutralMinionsKilledEnemyJungle > 50 ~ "Mais de 50 neutralMinionsKilledEnemyJungle")),
         goldEarned = as.factor(case_when(baseModelagem$goldEarned <= 30000 ~ "Até 30000 goldEarned",
                                          (baseModelagem$goldEarned > 30000 & baseModelagem$goldEarned <= 60000) ~ "Entre 30001 e 60000 goldEarned",
                                          baseModelagem$goldEarned > 60000 ~ "Mais de 60000 goldEarned")),
         goldSpent = as.factor(case_when(baseModelagem$goldSpent <= 30000 ~ "Até 30000 goldSpent",
                                         (baseModelagem$goldSpent > 30000 & baseModelagem$goldSpent <= 60000) ~ "Entre 30001 e 60000 goldSpent",
                                         baseModelagem$goldSpent > 60000 ~ "Mais de 60000 goldSpent")),
         wardsPlaced = as.factor(case_when(baseModelagem$wardsPlaced <= 50 ~ "Até 50 wardsPlaced",
                                           (baseModelagem$wardsPlaced > 50 & baseModelagem$wardsPlaced <= 100) ~ "Entre 51 e 100 wardsPlaced",
                                           baseModelagem$wardsPlaced > 100 ~ "Mais de 100 wardsPlaced")),
         wardsKilled = as.factor(case_when(baseModelagem$wardsKilled <= 20 ~ "Até 20 wardsKilled",
                                           baseModelagem$wardsKilled > 20 ~ "Mais de 20 wardsKilled")),
         visionWardsBoughtInGame = as.factor(case_when(baseModelagem$visionWardsBoughtInGame <= 10 ~ "Até 10 visionWardsBoughtInGame",
                                                       baseModelagem$visionWardsBoughtInGame > 10 ~ "Mais de 10 visionWardsBoughtInGame")),
         doubleKills = as.factor(case_when(baseModelagem$doubleKills <= 5 ~ "Até 5 doubleKills",
                                           baseModelagem$doubleKills > 5 ~ "Mais de 5 doubleKills")),
         tripleKills = as.factor(case_when(baseModelagem$tripleKills <= 2 ~ "Até 2 tripleKills",
                                           baseModelagem$tripleKills > 2 ~ "Mais de 2 tripleKills")),
         quadraKills = as.factor(case_when(baseModelagem$quadraKills <= 1 ~ "Até 1 quadraKills",
                                           baseModelagem$quadraKills > 1 ~ "Mais de 1 quadraKills")),
         pentaKills = as.factor(case_when(baseModelagem$pentaKills > 0 ~ "Houve pentaKills",
                                          baseModelagem$pentaKills < 1 ~ "não houve pentaKills")),
         killingSprees = as.factor(case_when(baseModelagem$killingSprees <= 5 ~ "Até 5 killingSprees",
                                             (baseModelagem$killingSprees > 5 & baseModelagem$killingSprees <= 10) ~ "Entre 6 e 10 killingSprees",
                                             baseModelagem$killingSprees > 10 ~ "Mais de 10 killingSprees")))


str(baseModelagemQuali)



#--------------------------criando base treinamento e teste Quali-----------------------------#

set.seed(998)
inTrainingQuali <- createDataPartition(baseModelagemQuali$teamWinner, p = .75, list = FALSE)
baseTrainQuali <- baseModelagemQuali[ inTraining,]
baseTestQuali  <- baseModelagemQuali[-inTraining,]


table(baseTrainQuali$teamWinner)
table(baseTestQuali$teamWinner)



#------------------GLM Quali---------------------#


str(baseModelagemQuali)

glmtesteQuali <- glm(teamWinner ~ duration_10_20 + duration_20_30 + duration_30_40 + duration_40_more + teamBlue + teamPurple + firstBlood +
                       firstTower + firstInhibitor + firstDragon + firstBaron + towerKills + inhibitorKills + dragonKills + baronKills + kills + deaths + assists + minionsKilled +
                       neutralMinionsKilled + neutralMinionsKilledTeamJungle + neutralMinionsKilledEnemyJungle + goldEarned + goldSpent + wardsPlaced + wardsKilled + 
                       visionWardsBoughtInGame  + doubleKills + tripleKills + quadraKills + pentaKills + killingSprees, data = baseTrainQuali, family = binomial)


summary(glmtesteQuali)


stepwise <- step(glmtesteQuali, direction = "both")


summary(stepwise)


glmTreinamentoQuali <- glm(formula = teamWinner ~  duration_20_30 + 
                            duration_30_40 + teamBlue + firstTower + towerKills + 
                            kills + deaths + assists + goldSpent + doubleKills + 
                            tripleKills, 
                          family = binomial, data = baseTrainQuali)


summary(glmTreinamentoQuali)



baseTrainQuali$pred <- predict(glmTreinamentoQuali, type = "response")
baseTrainQuali$predFinal <- case_when(baseTrainQuali$pred >0.5 ~ 1,
                                      baseTrainQuali$pred <= 0.5 ~ 0)


tab <- table(baseTrainQuali$predFinal, baseTrainQuali$teamWinner)

matrizConf <- confusionMatrix(tab)

as.table(matrizConf)
as.matrix(matrizConf)
as.matrix(matrizConf, what = "overall")
as.matrix(matrizConf, what = "classes")




#------------------------selecionando variaveis relevantes------------------#

baseTrain2 <- baseTrain %>%
  dplyr::select(teamWinner, deaths, kills, towerKills, killingSprees, teamBlue, firstTower, duration_40_more, goldEarned, goldSpent, minionsKilled, wardsPlaced, duration_30_40,
         wardsKilled, baronKills, firstBaron)


glmteste <- glm(teamWinner ~ 1, data = baseTrain2, family = binomial)






#------------------------arvore-------------------------#


library("rpart")
library("rpart.plot")
# train the model 
rpartmodel<- train(teamWinner~., data=baseModelagem, trControl=train_control, method="rpart")
# make predictions
predictions<- predict(rpartmodel,baseModelagem)
hr_model_tree<- cbind(baseModelagem,predictions)
# summarize results
confusionMatrix<- confusionMatrix(hr_model_tree$predictions,hr_model_tree$teamWinner)
confusionMatrix