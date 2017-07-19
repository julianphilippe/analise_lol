
#rm(list = ls())

library(tibble)
library(httr)
library(glue)
library(rvest)
library(rjson)
library(dplyr)
library(lubridate)
library(readr)


#fun??o lista de invocadores

invocadores <- function(invocador,user_key){
  
  invocador = paste0(invocador)
  
  key = '?api_key='
  
  urlInvoc = 'https://br.api.riotgames.com/api/lol/BR/v1.4/summoner/by-name/'
  
  #invoc = 'iceddeath,kuririncareca'
  
  listaInvoc <-rjson::fromJSON(paste0(readLines(paste0(urlInvoc,invocador,key,user_key))))
  
  #print(paste0(urlInvoc,invoc,key))
  
  dfInvoc <- tibble()
  for (i in seq_along(listaInvoc)){
    a <- tibble(nome = listaInvoc[[i]]$name, id = listaInvoc[[i]]$id)
    dfInvoc <- bind_rows(dfInvoc, a)
  }
  assign('dfInvoc', dfInvoc, envir=.GlobalEnv)
  
}


#invocadores('iceddeath,kuririncareca,pansadogueto,rodil,itsshowtime,xeternox,zEmerson,fullkira,fenixytb', 'RGAPI-bbc998cb-aca4-4366-bfb2-c21eaa91f781')


#------------------------------------------------------------------------------------#



#fun??o lista de partidas

partidas <- function(df,user_key){
  
  urlPartidas = 'https://br.api.riotgames.com/api/lol/BR/v2.2/matchlist/by-summoner/'
  
  key = '?seasons=PRESEASON2016&seasons=SEASON2016&seasons=PRESEASON2017&seasons=SEASON2017&api_key='
  
  dfPartidas <- tibble()
  
  for (i in seq_along(df)){
    
    id <- df[i]
    
    idInvoc = id
    
    listaPartidas <- rjson::fromJSON(paste0(readLines(paste0(urlPartidas,idInvoc,key,user_key))))
    
    print(id)
    
    for (i in seq_along(listaPartidas$matches)){
      a <- tibble(idJogador = id, id_partida = listaPartidas$matches[[i]]$matchId, campeao = listaPartidas$matches[[i]]$champion, lane = listaPartidas$matches[[i]]$lane, 
                  posicao = listaPartidas$matches[[i]]$role, temporada = listaPartidas$matches[[i]]$season)
      dfPartidas <- bind_rows(dfPartidas, a)
    }
    
  }
  assign('dfPartidas', dfPartidas, envir=.GlobalEnv)
}


#partidas(dfInvoc$id, 'RGAPI-bbc998cb-aca4-4366-bfb2-c21eaa91f781')

#------------------------------------------------------------------------------------#

#partidas das ?ltimas duas temporadas


dadosPartidas <- function(df,user_key){
  
  urlPartida = 'https://br1.api.riotgames.com/lol/match/v3/matches/'
  
  #idPartida = '1128639786'
  
  key = '?api_key='
  
  geralPartidas <- tibble()
  try(for (i in seq_along(df)){
    
    id = df[i]
    
    idPartida = id
    
    print(i)
    
    listaDadosPart <- rjson::fromJSON(paste0(readLines(paste0(urlPartida,idPartida,key,user_key))))
    
    Sys.sleep(2)
    
    
    #----------------------Status Partida-------------------------#
    
    geralPartidaTime_1 <- tibble(gameId = listaDadosPart$gameId,
                                 tempoPartidaMin = seconds_to_period(listaDadosPart$gameDuration)@minute,
                                 gameMode = listaDadosPart$gameMode,
                                 gameVersion = listaDadosPart$gameVersion,
                                 teamId = listaDadosPart$teams[[1]]$teamId,
                                 win = listaDadosPart$teams[[1]]$win,
                                 firstBlood = listaDadosPart$teams[[1]]$firstBlood,
                                 firstTower = listaDadosPart$teams[[1]]$firstTower,
                                 firstInhibitor =listaDadosPart$teams[[1]]$firstInhibitor,
                                 firstBaron = listaDadosPart$teams[[1]]$firstBaron,
                                 firstDragon = listaDadosPart$teams[[1]]$firstDragon,
                                 firstRiftHerald = listaDadosPart$teams[[1]]$firstRiftHerald,
                                 towerKills = listaDadosPart$teams[[1]]$towerKills,
                                 inhibitorKills = listaDadosPart$teams[[1]]$inhibitorKills,
                                 baronKills = listaDadosPart$teams[[1]]$baronKills,
                                 dragonKills = listaDadosPart$teams[[1]]$dragonKills,
                                 vilemawKills = listaDadosPart$teams[[1]]$vilemawKills,
                                 riftHeraldKills = listaDadosPart$teams[[1]]$riftHeraldKills)
    
    
    geralPartidaTime_2 <- tibble(gameId = listaDadosPart$gameId,
                                 tempoPartidaMin = seconds_to_period(listaDadosPart$gameDuration)@minute,
                                 gameMode = listaDadosPart$gameMode,
                                 gameVersion = listaDadosPart$gameVersion,
                                 teamId = listaDadosPart$teams[[2]]$teamId,
                                 win = listaDadosPart$teams[[2]]$win,
                                 firstBlood = listaDadosPart$teams[[2]]$firstBlood,
                                 firstTower = listaDadosPart$teams[[2]]$firstTower,
                                 firstInhibitor =listaDadosPart$teams[[2]]$firstInhibitor,
                                 firstBaron = listaDadosPart$teams[[2]]$firstBaron,
                                 firstDragon = listaDadosPart$teams[[2]]$firstDragon,
                                 firstRiftHerald = listaDadosPart$teams[[2]]$firstRiftHerald,
                                 towerKills = listaDadosPart$teams[[2]]$towerKills,
                                 inhibitorKills = listaDadosPart$teams[[2]]$inhibitorKills,
                                 baronKills = listaDadosPart$teams[[2]]$baronKills,
                                 dragonKills = listaDadosPart$teams[[2]]$dragonKills,
                                 vilemawKills = listaDadosPart$teams[[2]]$vilemawKills,
                                 riftHeraldKills = listaDadosPart$teams[[2]]$riftHeraldKills)
    
    a <- bind_rows(geralPartidaTime_1, geralPartidaTime_2)
    
    geralPartidas <- bind_rows(geralPartidas, a)
    
    participantes <- tibble()
    
    for (i in seq_along(listaDadosPart$participants)){
      a <- tibble(gameId = listaDadosPart$gameId,
                  teamId = listaDadosPart$participants[[i]]$teamId,
                  win = listaDadosPart$participants[[i]]$stats$win,
                  participantId = listaDadosPart$participants[[i]]$participantId,
                  championId = listaDadosPart$participants[[i]]$championId,
                  spell1Id = listaDadosPart$participants[[i]]$spell1Id,
                  spell2Id = listaDadosPart$participants[[i]]$spell2Id,
                  highestAchievedSeasonTier = listaDadosPart$participants[[i]]$highestAchievedSeasonTier,
                  role = listaDadosPart$participants[[i]]$timeline$role,
                  lane = listaDadosPart$participants[[i]]$timeline$lane,
                  kills = listaDadosPart$participants[[i]]$stats$kills,
                  deaths = listaDadosPart$participants[[i]]$stats$deaths,
                  assists = listaDadosPart$participants[[i]]$stats$assists,
                  killingSprees = listaDadosPart$participants[[i]]$stats$killingSprees,
                  inhibitorKills = listaDadosPart$participants[[i]]$stats$inhibitorKills,
                  turretKills = listaDadosPart$participants[[i]]$stats$turretKills,
                  totalMinionsKilled = listaDadosPart$participants[[i]]$stats$totalMinionsKilled,
                  goldEarned = listaDadosPart$participants[[i]]$stats$goldEarned,
                  champLevel = listaDadosPart$participants[[i]]$stats$champLevel,
                  totalDamageDealt = listaDadosPart$participants[[i]]$stats$totalDamageDealt,
                  magicDamageDealt = listaDadosPart$participants[[i]]$stats$magicDamageDealt,
                  physicalDamageDealt = listaDadosPart$participants[[i]]$stats$physicalDamageDealt,
                  trueDamageDealt = listaDadosPart$participants[[i]]$stats$trueDamageDealt,
                  longestTimeSpentLiving = listaDadosPart$participants[[i]]$stats$longestTimeSpentLiving,
                  totalTimeCrowdControlDealt = listaDadosPart$participants[[i]]$stats$totalTimeCrowdControlDealt,
                  visionScore = listaDadosPart$participants[[i]]$stats$visionScore)
      
      participantes <- bind_rows(participantes, a)
      
    }
    
  }
  
  )
  
  assign('geralPartidas', geralPartidas, envir=.GlobalEnv)
  assign('participantes', participantes, envir=.GlobalEnv)
  
}

#dadosPartidas(dfPartidas$id_partida, 'RGAPI-bbc998cb-aca4-4366-bfb2-c21eaa91f781')

#------------------------cria arquivos-----------------------------#


#readr::write_delim(dfInvoc, path ='F:/R Projects/analise_lol/dfInvoc.csv', delim = ';')

#readr::write_delim(dfPartidas, path ='F:/R Projects/analise_lol/dfPartidas.csv', delim = ';')

#readr::write_delim(geralPartidas, path ='F:/R Projects/analise_lol/geralPartidas.csv', delim = ';')

#readr::write_delim(participantes, path ='F:/R Projects/analise_lol/participantes.csv', delim = ';')




#adc <- participantes %>%
#  filter(role == 'DUO_CARRY')

#sup <- participantes %>%
#  filter(role == 'DUO_SUPPORT')

#jungle <- participantes %>%
#  filter(lane == 'JUNGLE')

#mid <- participantes %>%
#  filter(lane == 'MIDDLE')

#top <- participantes %>%
#  filter(lane == 'TOP')


