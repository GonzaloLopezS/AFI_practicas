########################
##    AFI - MEDS      ##
##    Practica R      ##
##    Ejercicio 3     ##
########################

library(tidyr)
library(reshape2)
library(forcats)
library(dplyr)
library(data.table)

file_b <- "snooker_r\\matches_r.csv"

dataset <- read.csv(file_b)

str(dataset)

#Tras inspeccionar el dataset, se van a eliminar campos con url y fecha

# subset que tendra: id torneo, ronda (stage), best of ("al mejor de"),
# nombre jugadores, resultado del partido, resultado en frames

subset_snooker <- dataset[c("tournament_id","stage","best_of","player1_name",
                            "player2_name","score1","score2","frames_scores")]

#quedan descartados los partidos que hayan quedado en empate:
subset_snooker <- subset_snooker[subset_snooker$score1 > subset_snooker$score2,]

typeof(subset_snooker)

# El campo "frames_scores" es susceptible de ser limpiado con tidyr

# Insight 1: Jugador con mas finales perdidas:

subset_snooker %>% 
  group_by(player2_name) %>% 
  filter(stage %in% "Final") %>% 
  summarise(lost_finals = n()) %>% 
  arrange(desc(lost_finals)) %>% 
  head(1)
  
# Inisght 2: Rivalidades historicas: Top enfrentamientos historicos + enfrentamientos en finales con mas partidos disputados 

################################################################################
# A copilot le pregunté:
# 1) sobre el dataset [...] y usando dplyr. ¿Cómo obtengo el top 50 de mayor número
# de veces que se repite la tupla (player1_name,player2_name) en total y en caso
# de que el valor de la columna stage valga "Final"?

# 2) corrección, en realidad quiero el resultado en 4 columnas:[...] otra (con filtro
# de stage == Final).
# Es decir, la tercera columna serían los counts sin el filtro stage == Final y 
# la última columna otro count con el filtro stage == Final
################################################################################

top50_rivalries <- subset_snooker %>%
  group_by(player1_name, player2_name) %>%
  summarize(total_games = n(),
            finals = sum(stage == "Final")) %>%
  arrange(desc(total_games),desc(finals)) %>%
  head(50)

top50_rivalries 

# Insight 3: Top 20 jugadores con mas partidas ganadas y % de victorias correspondientes
# a finales #data.table
DT_snooker <- data.table(subset_snooker)

total_abs_wins <- DT_snooker[,.N,player1_name][order(player1_name)]#[1:20,player1_name]
total_abs_wins <- total_abs_wins[28:nrow(total_abs_wins),]

total_abs_loses <- DT_snooker[,.N,player2_name][order(player2_name)]#[1:20,player1_name]
total_abs_loses <- total_abs_wins[45:nrow(total_abs_loses),]


# total_played = total_abs_wins

# top20_rel_wins <- DT_snooker[, ratio := player1_name/(player1_name + player2_name)]

# top20 <- DT_snooker[player1_name %in% top20_abs_wins,.(player1_name,ratio)]
# head(top20)
  
# Insight 4: Jugadores con mas partidas (no finales) ganadas en el desempate:
most_frequent_players <- DT_snooker[!(stage %in% "Final") & (score1 - score2 <= 1),.N, by = player1_name][order(-N)]
head(most_frequent_players)

#Insight 5: usar tidyr para generar nuevas columnas (wide dataset) con cada frame por separado (de un subset de 20 partidos)
snooker_20 = DT_snooker[1:20]
snooker_20$id_score <- 1:20

snooker_20 <- snooker_20 %>% 
  separate_rows(frames_scores, sep = ";")

snooker_20_wider <- pivot_wider(snooker_20, names_from = id_score, values_from = frames_scores, names_prefix = "score_")
