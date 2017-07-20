
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



#----------------------separa dados dos participantes por times---------------------#


#----ADC----#

adc <- participantes %>%
  filter(role == 'DUO_CARRY') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

#----SUP---#

sup <- participantes %>%
  filter(role == 'DUO_SUPPORT') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

#----JG----#

jungle <- participantes %>%
  filter(lane == 'JUNGLE') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 


#----MID----#

mid <- participantes %>%
  filter(lane == 'MIDDLE') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 


#----TOP----#

top <- participantes %>%
  filter(lane == 'TOP') %>%
  select(gameId, teamId, championId, spell1Id, spell2Id, highestAchievedSeasonTier, role, lane, kills, deaths, assists,
         killingSprees, inhibitorKills, turretKills, totalMinionsKilled, goldEarned, champLevel, totalDamageDealt, 
         magicDamageDealt, physicalDamageDealt, trueDamageDealt, longestTimeSpentLiving, totalTimeCrowdControlDealt, 
         visionScore) 

#--------------------------ajusta nomes---------------------------#


#----ADC----#

nomes <- names(adc[3:24])

nome_adc <- paste0('adc_',nomes)

names(adc) <- c('gameId', 'teamId', nome_adc)


#----SUP----#

nomes <- names(sup[3:24])

nome_sup <- paste0('sup_',nomes)

names(sup) <- c('gameId', 'teamId', nome_sup)


#----JG----#

nomes <- names(jungle[3:24])

nome_jg <- paste0('jg_',nomes)

names(jungle) <- c('gameId', 'teamId', nome_jg)


#----MID----#

nomes <- names(mid[3:24])

nome_mid <- paste0('mid_',nomes)

names(mid) <- c('gameId', 'teamId', nome_mid)


#----TOP----#

nomes <- names(top[3:24])

nome_top <- paste0('top_',nomes)

names(top) <- c('gameId', 'teamId', nome_top)


#-----Apaga listas de nomes------#

rm(list = c('nomes','nome_top','nome_adc','nome_mid','nome_jg','nome_sup'))



#--------------------Cria tabela final--------------------#


partidasFinal <- geralPartidas %>%
  left_join(adc, by = c('gameId' = 'gameId', 'teamId' = 'teamId'))











