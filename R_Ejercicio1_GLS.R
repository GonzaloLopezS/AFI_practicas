########################
##    AFI - MEDS      ##
##    Practica R      ##
##    Ejercicio 1     ##
########################

file <- "spotify.csv"

#1)
spotify_doc <- read.csv(file) 

#2)List compuesta por un data frame
mode(spotify_doc)
typeof(spotify_doc)

is.list(spotify_doc)
is.data.frame(spotify_doc)

#3)
lapply(spotify_doc,mode) #tipo de cada columna
lapply(spotify_doc,str)  #muestra de ejemplos

#4)
head(spotify_doc,18)
#5)
tail(spotify_doc,25)

#6)
dim(spotify_doc)
nrows <- dim(spotify_doc)[1]
ncols <- dim(spotify_doc)[2]
paste("dimension = ", nrows,"x",ncols)

#7)
variables_dataset <- colnames(spotify_doc)
variables_dataset

#8)
var <- variables_dataset[1]
typeof(spotify_doc$Index)
spotify_doc <- spotify_doc[2:length(spotify_doc)]


#9) renombrar variables
cn <- colnames(spotify_doc)

cn <- tolower(cn)
cn <- gsub("\\.","",cn)
modif1 <- spotify_doc
colnames(modif1) <- c(cn)

sptx <- modif1 #datos maquetados a tratar de la tabla spotify original
rm(modif1)

#--- A partir de ahora se trabajara con el data frame "sptx"

#10)
sptx[is.na(sptx)] #devuelve character(0)
lapply(sptx,anyNA) #false para todas las variables

#11) Top Genre --> Revisar
topgenre_factor <- factor(sptx$topgenre)
topgenre_factor

#12)
ex12 <- sptx[1:30,1:(ncol(sptx)-3)]
ex12

#13)
ex13 <- sptx[-31:-nrow(sptx),-ncol(sptx):(-ncol(sptx)+2)]
ex13

#14)
artist_uniques <- length(unique(sptx$artist))
artist_uniques

#15)
ex15_ABBA <- subset(sptx, artist == "ABBA") 
ex15_ABBA <- ex15_ABBA$title
length(ex15_ABBA) #22 canciones (no se hallan colaboraciones, sino estrictamente de ABBA)

#16)
ex16 <- subset(sptx, topgenre == "pop") #estrictamente pop
length(ex16$title) #47 canciones que son estrictamente pop

#17)
library(lubridate)

calculate_year_since_release <- function(y){
  year <- y
  present <- year(today())
  
    if (present - year <= 10){
      return("1")
    }
    else if(present - year > 10 && present - year <= 20){
      return("2")
    }
    else if(present - year > 20 && present - year <= 30){
      return("3")
    }
    else{
      return("4") #casos > 30
    }
}

#Ejemplos de funcionamiento de la funcion anterior:
calculate_year_since_release(sptx$year[1])
calculate_year_since_release(sptx$year[10])
calculate_year_since_release(sptx$year[25])
calculate_year_since_release(sptx$year[37])
calculate_year_since_release(sptx$year[62])
calculate_year_since_release(sptx$year[237])
calculate_year_since_release(sptx$year[518])

#18)
year_since_release <- lapply(sptx$year,calculate_year_since_release)
sptx$year_since_release <- unlist(year_since_release)


#19)
ysr_label <- c("Mas de 30 años de antiguedad","entre 20 y 30 años",
               "entre 10 y 20 años","maximo 10 años")
ysr_level <- c("4","3","2","1")
ysr_factor <- factor(sptx$year_since_release, levels = ysr_level,labels = ysr_label)

sptx$year_since_release <- ysr_factor
sptx[c(4,15)] #para comprobar que el factor y sus etiquetas se corresponden con lo esperado de la columna 'Year'

#20)
ex20 <- nrow(subset(sptx, year_since_release == levels(sptx$year_since_release)[4])) #El level 4 equivale al tag "maximo 10 años"
ex20 #238 canciones

length(sptx$year[sptx$year >= (year(today()) - 10)]) #Otra forma de comprobar del resultado anterior

#21)
subset(sptx,topgenre == "latin") #estrictamente latin. Los titulos, disponibles en ventana de ejecucion.

#22)
hist(sptx$year)

#23) usar boxplot -> Tanto #23 como #24 necesitan cast a numeric
anyNA(sptx$lengthduration)
length_duration <- sptx$lengthduration

#Funcion que tomará un vector convertible a numérico y lo limpiará de NAs
clean2numeric <- function(c){
  if (!is.numeric(c)) c <- as.numeric(c)
  return(c[!is.na(c)])
}

length_duration <- clean2numeric(length_duration)
boxplot(length_duration)

#24)
vector <- function(x){
  x <- clean2numeric(x)
  return(c(min(x),mean(x),max(x)))
}

ex24 <- vector(length_duration)
ex24_labels <- c("duracion minima","duracion media","duracion maxima")
rbind(ex24_labels,ex24)

#25)
ex25 <- length(length_duration[length_duration > 100])
ex25

#26)
vn <- sapply(sptx, is.numeric)
vn[vn]

#27) Los años de publicación quedan fuera del cálculo de la media
str(sptx)
cn <- sptx[,unlist(lapply(sptx, is.numeric))]
cn[nrow(cn) + 1, ] = c("averages", lapply(cn[2:length(cn)],mean))

#Para la representacion, incluí una nueva fila de registros con valor "averages"
#a la columna que corresponde con Year y el resto de valores equivalen a la media
#de cada columna. Por ello, imprimo por pantalla la última linea (la 1995) del data frame.
#Existen otras formas de imprimir y representar el resultado
ex27 <- cn[nrow(cn),]
ex27 

#28)
s28 <- summary(sptx$year)
print(s28[2])
print(s28[3]) #Mediana = segundo cuartil
print(s28[5])

#29)
year_sorted <- sort(sptx$year)
l <- length(year_sorted)
dv <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
deciles <- c(unlist(year_sorted[sapply(dv*l,ceiling)]))
deciles

#30)
summary(sptx)

#31) ¿bailable = 'danceability? O es el producto de alguna combinación: dance + energy + bpm?

# A Copilot le pregunté: "en R, ¿cómo conseguir las medias de una variable numérica
# segmentada por grupos de etiquetas usando funciones de R base?"
# A partir de su respuesta para R base, la adapté a las condiciones del ejercicio/data frame

# danceability_avgs <- aggregate(sptx$danceability ~ topgenre_factor, FUN = mean, data = sptx)
# colnames(danceability_avgs) = c("Top Genre","Danceability")
# danceability_sorted <- danceability_avgs[order(danceability_avgs$Danceability, decreasing = TRUE), ]
# head(danceability_sorted)

# ... Pero luego releí el enunciado y vi que no servía de nada :-(

top_dancing <- sptx[head(order(sptx$danceability, decreasing = TRUE),1),]
top_dancing$topgenre

bottom_dancing <- sptx[tail(order(sptx$danceability, decreasing = TRUE),1),]
bottom_dancing$topgenre

#32)
sptx <- sptx[order(sptx$year, decreasing = TRUE),]
head(sptx) #tambien se puede abrir e inspeccionar el data frame clicando sobre la tabla de variables

# Tras realizar ultimas modificaciones, 
# Se guarda el data frame en un csv tal y como se pide:

#Revisar formato encodage
write.csv(sptx,"~\\AFI\\R\\MEDS_2024_Practica_Programacion_en_R\\ejercicio_1_df.csv") #respetar arborescencia
write.csv(sptx, "~\\nuevo_df2.csv") #este lo deja en Documentos (debajo de mi carpeta personal de OneDrive)
