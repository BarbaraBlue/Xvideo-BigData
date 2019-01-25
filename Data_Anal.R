### Instalar los packages necesarios ###
# install.packages("rvest")
# install.packages("data.table")
# install.packages("ggplot2") 

### Llamar los packages a utilizar ###
library('rvest')
library(data.table)
library(ggplot2)

#==================== usando Xvideos ====================#

# Se busca en la pagina Xvideo: ANAL con filtro de valoracion

# Inicializando la var de archivo con el nombre de la página a utilizar
paginaXVideos <- 'https://www.xvideos.com/?k=anal'

# Leyendo el html del archivo
webpageXVideos <- read_html(paginaXVideos)

# Extraccion del texto contenido en la clase thumb-under
contenidoWebXVideos <- html_nodes(webpageXVideos,'.thumb-under > p > a')
print (contenidoWebXVideos)

# Viendo el contenido de la posición 1 de la variable contenidoWebXVideos
print(contenidoWebXVideos[1])

# Extrayendo los links de los videos
linksVIDEOS <- html_attr(contenidoWebXVideos,"href")

# Arreglando los links de todos los videos

for(i in 1:27){
  LinksXvideo <- print(paste("http://www.xvideos.com",linksVIDEOS,sep = ""))
}

# Viendo que tiene la posicion 1 de la variable todosLosLinksXvideo
print(LinksXvideo[1])

# Viendo cuantas variables tiene LinksXvideo
length(LinksXvideo)

# Extrayendo el texto de contenidoWebXVideos
textoXVideos <- html_text(contenidoWebXVideos)

# Viendo que tiene la posicion 1 la variable textoXVideos
print(textoXVideos[1])

# Extraccion de duracion de cada video
DurationXVideos <- html_nodes(webpageXVideos,'.duration')

#Limpieza de los datos de duracion
DuracionXVideos <- html_text(DurationXVideos)

# Viendo que tiene la posición 1 de la variable DuracionXVideos
print(DuracionXVideos[1])

# Primer paso para extraer el numero de visitas de cada video
VistasXVideos <- html_nodes(webpageXVideos,'.thumb-under > p > span')

# Limpiando los datos para tener solo el texto 
texto_VistasXVideos <- html_text(VistasXVideos)

# Separando el texto obtenido con un guion para despues eliminar la duracion
split_VistasXVideos <- strsplit(texto_VistasXVideos,"-")

# Obteniendo el primer dato de views 
viewsXVideos <- list()
for(i in 1:length(split_VistasXVideos)){
  print(split_VistasXVideos[[i]][[2]])
  viewsXVideos[i] <- split_VistasXVideos[[i]][[2]]
}

# Limpiando los datos obtenidos de views
viewsXVideos <-  gsub("Views","",viewsXVideos)
viewsXVideos <- gsub(" ","",viewsXVideos)
viewsXVideos <- gsub("k","-k",viewsXVideos)  
viewsXVideos <- gsub("M","-M",viewsXVideos)  

# Separando los datos para luego reemplazar k y M numericamente
Visitas <- strsplit(viewsXVideos,"-")

# Crear funcion para reemplazar k y M numericamente #

# VisitasXVideo: string -> double
# VisitasXVideo: entrega la cantidad de visitas de cada video
# si aparece una k se multiplica el numero por mil 
# si aparece una M se multimplica por un millon
# Ejemplo: VisitasXVideo(4k)-> 4000

VisitasXVideo <- function (entrada){
  # para los elementos que no tienen ni k, ni M, se usa is.na
  if(is.na(entrada[2])){
    entrada[1] <- as.numeric(entrada[1])
  }else if(entrada[2]=="k"){
    entrada[1] <- as.numeric(entrada[1])*1000
  }else if(entrada[2]=="M"){
    entrada[1] <- as.numeric(entrada[1])*1000000
  }
  return(entrada[1])
}

# Recorriendo cada elemento aplicando la funcion VisitasXVideo 
for(i in 1:length(Visitas)){
  Visitas[i] <- VisitasXVideo(Visitas[[i]])
}

# Ver la posicion 1 de visitas
Visitas[1]

# Extrae los elementos de la lista y los pasa a una lista
unlistVisitas <- unlist(Visitas)

# Crear lista para agregar likes extraidos
Me_Gusta <- list()

# Crear lista para agregar dislikes extraidos 
No_Me_Gusta <- list()

### Extrayendo likes y dislikes por cada uno de los links sin for ###

Leer_link01 <-read_html(LinksXvideo[1])
Rating_link01 <- html_nodes(Leer_link01, '.rating-inbtn')
Texto_rating01 <- html_text(Rating_link01)
Me_Gusta <- c(Me_Gusta, Texto_rating01[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating01[2])

Leer_link02 <-read_html(LinksXvideo[2])
Rating_link02 <- html_nodes(Leer_link02, '.rating-inbtn')
Texto_rating02 <- html_text(Rating_link02)
Me_Gusta <- c(Me_Gusta, Texto_rating02[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating02[2])

Leer_link03 <-read_html(LinksXvideo[3])
Rating_link03 <- html_nodes(Leer_link03, '.rating-inbtn')
Texto_rating03 <- html_text(Rating_link03)
Me_Gusta <- c(Me_Gusta, Texto_rating03[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating03[2])

Leer_link04 <-read_html(LinksXvideo[4])
Rating_link04 <- html_nodes(Leer_link04, '.rating-inbtn')
Texto_rating04 <- html_text(Rating_link01)
Me_Gusta <- c(Me_Gusta, Texto_rating04[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating04[2])

Leer_link05 <-read_html(LinksXvideo[5])
Rating_link05 <- html_nodes(Leer_link05, '.rating-inbtn')
Texto_rating05 <- html_text(Rating_link05)
Me_Gusta <- c(Me_Gusta, Texto_rating05[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating05[2])

Leer_link06 <-read_html(LinksXvideo[6])
Rating_link06 <- html_nodes(Leer_link06, '.rating-inbtn')
Texto_rating06 <- html_text(Rating_link06)
Me_Gusta <- c(Me_Gusta, Texto_rating06[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating06[2])

Leer_link07 <-read_html(LinksXvideo[7])
Rating_link07 <- html_nodes(Leer_link07, '.rating-inbtn')
Texto_rating07 <- html_text(Rating_link07)
Me_Gusta <- c(Me_Gusta, Texto_rating07[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating07[2])

Leer_link08 <-read_html(LinksXvideo[8])
Rating_link08 <- html_nodes(Leer_link08, '.rating-inbtn')
Texto_rating08 <- html_text(Rating_link08)
Me_Gusta <- c(Me_Gusta, Texto_rating08[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating08[2])

Leer_link09 <-read_html(LinksXvideo[9])
Rating_link09 <- html_nodes(Leer_link09, '.rating-inbtn')
Texto_rating09 <- html_text(Rating_link09)
Me_Gusta <- c(Me_Gusta, Texto_rating09[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating09[2])

Leer_link10 <-read_html(LinksXvideo[10])
Rating_link10 <- html_nodes(Leer_link10, '.rating-inbtn')
Texto_rating10 <- html_text(Rating_link10)
Me_Gusta <- c(Me_Gusta, Texto_rating10[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating10[2])

Leer_link11 <-read_html(LinksXvideo[11])
Rating_link11 <- html_nodes(Leer_link11, '.rating-inbtn')
Texto_rating11 <- html_text(Rating_link11)
Me_Gusta <- c(Me_Gusta, Texto_rating11[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating11[2])

Leer_link12 <-read_html(LinksXvideo[12])
Rating_link12 <- html_nodes(Leer_link12, '.rating-inbtn')
Texto_rating12 <- html_text(Rating_link12)
Me_Gusta <- c(Me_Gusta, Texto_rating12[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating12[2])

Leer_link13 <-read_html(LinksXvideo[13])
Rating_link13 <- html_nodes(Leer_link13, '.rating-inbtn')
Texto_rating13 <- html_text(Rating_link13)
Me_Gusta <- c(Me_Gusta, Texto_rating13[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating13[2])

Leer_link14 <-read_html(LinksXvideo[14])
Rating_link14 <- html_nodes(Leer_link14, '.rating-inbtn')
Texto_rating14 <- html_text(Rating_link14)
Me_Gusta <- c(Me_Gusta, Texto_rating14[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating14[2])

Leer_link15 <-read_html(LinksXvideo[15])
Rating_link15 <- html_nodes(Leer_link15, '.rating-inbtn')
Texto_rating15 <- html_text(Rating_link15)
Me_Gusta <- c(Me_Gusta, Texto_rating15[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating15[2])

Leer_link16 <-read_html(LinksXvideo[16])
Rating_link16 <- html_nodes(Leer_link16, '.rating-inbtn')
Texto_rating16 <- html_text(Rating_link16)
Me_Gusta <- c(Me_Gusta, Texto_rating16[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating16[2])

Leer_link17 <-read_html(LinksXvideo[17])
Rating_link17 <- html_nodes(Leer_link17, '.rating-inbtn')
Texto_rating17 <- html_text(Rating_link17)
Me_Gusta <- c(Me_Gusta, Texto_rating17[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating17[2])

Leer_link18 <-read_html(LinksXvideo[18])
Rating_link18 <- html_nodes(Leer_link18, '.rating-inbtn')
Texto_rating18 <- html_text(Rating_link18)
Me_Gusta <- c(Me_Gusta, Texto_rating18[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating18[2])

Leer_link19 <-read_html(LinksXvideo[19])
Rating_link19 <- html_nodes(Leer_link19, '.rating-inbtn')
Texto_rating19 <- html_text(Rating_link19)
Me_Gusta <- c(Me_Gusta, Texto_rating19[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating19[2])

Leer_link20 <-read_html(LinksXvideo[20])
Rating_link20 <- html_nodes(Leer_link20, '.rating-inbtn')
Texto_rating20 <- html_text(Rating_link20)
Me_Gusta <- c(Me_Gusta, Texto_rating20[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating20[2])

Leer_link21 <-read_html(LinksXvideo[21])
Rating_link21 <- html_nodes(Leer_link21, '.rating-inbtn')
Texto_rating21 <- html_text(Rating_link21)
Me_Gusta <- c(Me_Gusta, Texto_rating21[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating21[2])

Leer_link22 <-read_html(LinksXvideo[22])
Rating_link22 <- html_nodes(Leer_link22, '.rating-inbtn')
Texto_rating22 <- html_text(Rating_link22)
Me_Gusta <- c(Me_Gusta, Texto_rating22[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating22[2])

Leer_link23 <-read_html(LinksXvideo[23])
Rating_link23 <- html_nodes(Leer_link23, '.rating-inbtn')
Texto_rating23 <- html_text(Rating_link23)
Me_Gusta <- c(Me_Gusta, Texto_rating23[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating23[2])

Leer_link24 <-read_html(LinksXvideo[24])
Rating_link24 <- html_nodes(Leer_link24, '.rating-inbtn')
Texto_rating24 <- html_text(Rating_link24)
Me_Gusta <- c(Me_Gusta, Texto_rating24[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating24[2])

Leer_link25 <-read_html(LinksXvideo[25])
Rating_link25 <- html_nodes(Leer_link25, '.rating-inbtn')
Texto_rating25 <- html_text(Rating_link25)
Me_Gusta <- c(Me_Gusta, Texto_rating25[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating25[2])

Leer_link26 <-read_html(LinksXvideo[26])
Rating_link26 <- html_nodes(Leer_link26, '.rating-inbtn')
Texto_rating26 <- html_text(Rating_link26)
Me_Gusta <- c(Me_Gusta, Texto_rating26[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating26[2])

Leer_link27 <-read_html(LinksXvideo[27])
Rating_link27 <- html_nodes(Leer_link27, '.rating-inbtn')
Texto_rating27 <- html_text(Rating_link27)
Me_Gusta <- c(Me_Gusta, Texto_rating27[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating27[2])

# Verificando que la lista posea 27 variables
length(Me_Gusta)
length(No_Me_Gusta)

# Arreglando los datos extraidos
Me_Gusta <- gsub("k","-k",Me_Gusta)  
Me_Gusta <- strsplit(Me_Gusta, "-")
No_Me_Gusta <- gsub("k","-k",No_Me_Gusta) 
No_Me_Gusta <- strsplit(No_Me_Gusta, "-")

# Recorriendo cada elemento de las listas aplicando la funcion VisitasXVideo

for(i in 1:length(Me_Gusta)){
  Me_Gusta[i] <- VisitasXVideo(Me_Gusta[[i]])
}

for(i in 1:length(No_Me_Gusta)){
  No_Me_Gusta[i] <- VisitasXVideo(No_Me_Gusta[[i]])
}

# Extrae los elementos de una lista y los pasa a una lista
unlistMe_Gusta <- unlist(Me_Gusta)
unlistNo_Me_Gusta <- unlist(No_Me_Gusta)

# Se genera una variable tipo, donde 5 es GAY
Tipo <- list("7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7","7")
unlistTipo <- unlist(Tipo)

# Se genera una tabla con los datos obtenidos para GAY
dfANAL <- data.frame(LINKS = LinksXvideo, TITULO= textoXVideos, TIPO= unlistTipo, VISITAS= unlistVisitas, ME_GUSTA= unlistMe_Gusta, NO_ME_GUSTA= unlistNo_Me_Gusta)

# Almacenando la informacion en CSV
write.csv(dfANAL, file="Tabla07.csv")