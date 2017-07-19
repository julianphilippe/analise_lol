
library(tibble)
library(httr)
library(glue)
library(rvest)
library(rjson)
library(dplyr)
library(lubridate)
library(readr)


#dfPartidas <- readr::read_delim('dfPartidas.csv', delim = ';')
#geralPartidas <- readr::read_delim('geralPartidas.csv', delim = ';')
#participantes <- readr::read_delim('participantes.csv', delim = ';')


names(participantes)

adc <- participantes %>%
  filter(role == 'DUO_CARRY') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 


sup <- participantes %>%
  filter(role == 'DUO_SUPPORT') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

jungle <- participantes %>%
  filter(lane == 'JUNGLE') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

mid <- participantes %>%
  filter(lane == 'MIDDLE') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

top <- participantes %>%
  filter(lane == 'TOP') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

partidasFinal <- geralPartidas %>%
  inner_join(adc, by = c('gameId' = 'gameId', 'teamId' = 'teamId'))

