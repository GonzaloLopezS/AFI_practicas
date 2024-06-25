########################
##    AFI - MEDS      ##
##    Practica R      ##
##    Ejercicio 2     ##
########################

library(tidyr)
library(reshape2)
library(forcats)
library(dplyr)
library(data.table)

########################
##     PARTE 1        ##
##  tidyr y reshape2  ##
########################

file <- "ejercicio_1_df.csv"
sptx <- read.csv(file)

#1) Guardar el resultado en una nueva variable que contendra el dataset:

dataset_1 <- sptx %>% 
  group_by(topgenre,year_since_release) %>% 
  summarise(n())

colnames(dataset_1) <- c("top_genre","year_since_release","n_songs")
dataset_1

#2) Las lineas comentadas corresponden con otras funciones similiares, incluidas
# en las librerías tidyr y reshape2 Para comprobar otro resultado, descomentar y 
# ejecutar.

wide_dataset_1 <- pivot_wider(dataset_1, names_from = year_since_release, 
                              values_from = n_songs) #tidyr
# wide_dataset_2 <- spread(dataset_1,year_since_release,n_songs) #tidyr
# wide_dataset_3 <- dcast(dataset_1, top_genre ~ year_since_release, 
#                         value.var = "n_songs") #dcast
wide_dataset_1

#3) Las lineas comentadas corresponden con otras funciones similiares, incluidas
# en las librerías tidyr y reshape2 Para comprobar otro resultado, descomentar y 
# ejecutar.
long_dataset <- melt(wide_dataset_1, id.vars = c("top_genre"), variable.name = 
                       "year_since_release", value.name = "n_songs") #reshape2
long_dataset <- long_dataset[!is.na(long_dataset$n_songs), c("top_genre","year_since_release","n_songs")] %>% 
  arrange(top_genre, year_since_release)

# long_dataset_2 <- gather(wide_dataset_2, year_since_release, n_songs, "entre 10 y 20 años", "entre 20 y 30 años", 
#                          "Mas de 30 años de antiguedad", "maximo 10 años")
# long_dataset_2 <- long_dataset_2[!is.na(long_dataset_2$n_songs), c("top_genre","year_since_release","n_songs")] %>%
#   arrange(top_genre, year_since_release)

# long_dataset_3 <- pivot_longer(wide_dataset_3, cols = c("entre 10 y 20 años", "entre 20 y 30 años",
#                                                         "Mas de 30 años de antiguedad","maximo 10 años"),
#                                names_to = "year_since_release", values_to = "n_songs")
# long_dataset_3 <- long_dataset_3[!is.na(long_dataset_3$n_songs), c("top_genre","year_since_release","n_songs")] %>%
#   arrange(top_genre, year_since_release)

long_dataset

########################
##     PARTE 2        ##
##############
#La misma funcion que en el ejercicio 1, para convertir a numeric y quitar NAs
clean2numeric <- function(c){
  if (!is.numeric(c)) c <- as.numeric(c)
  c[!is.na(c)]
}


#1) Para cada genero: la duracion y el numero de canciones (titulos)
q1 <- sptx %>% 
  group_by(topgenre) %>% 
  select(lengthduration) %>% 
  summarise(n_songs = n(),
            duration = mean(clean2numeric(lengthduration)))

q1 #abrir tibble para ver decimales en duration

#2) Genero con mas canciones
q1 %>% 
  arrange(desc(n_songs)) %>% 
  filter(n_songs == max(n_songs)) %>% 
  select(topgenre,n_songs)

#usando data.table:
q1_datatable = data.table(q1)
q2b <- q1_datatable[,.(topgenre, n_songs), by = topgenre][order(-n_songs)]
q2b[1]

#3) Artista con mas canciones en los ultimos 10 años
q3 <- sptx %>% 
  group_by(artist) %>% 
  filter(year_since_release == "maximo 10 años") %>% 
  summarise(n_songs = n()) %>% 
  arrange(desc(n_songs)) %>% 
  head(1)

sptx_dt <- data.table(sptx)

#4) Canciones pop > 30 años, numero de canciones que duran mas de 100 segundos
ex4 <- sptx_dt[(topgenre == "pop" & year_since_release == "Mas de 30 años de antiguedad" & clean2numeric(lengthduration) > 100 ),.(.N)]
ex4 #16 canciones cumplen las condiciones para estrictamente "pop"

#5) ordenar artistas en funcion de lo bailable que es su cancion dentro del genero pop (desc)

#con dplyr:
q5 <- sptx %>% 
  group_by(artist) %>% 
  filter(topgenre == "pop") %>% 
  arrange(desc(danceability))
q5

#con data.table:
q5b <- sptx_dt[topgenre == "pop",.(mean_danceability = mean(danceability)), by = artist][order(-mean_danceability)]
q5b
