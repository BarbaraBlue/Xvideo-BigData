### Instalar los packages necesarios ###
# install.packages("rvest")
# install.packages("data.table")
# install.packages("ggplot2") 

### Llamar los packages a utilizar ###
library('rvest')
library(data.table)
library(ggplot2)

#==================== usando Xvideos ====================#

# # Se busca en la pagina Xvideo: PAG INICIO (Los mas visto recientemente)

# Inicializando la var de archivo con el nombre de la página a utilizar
paginaXVideos <- 'https://www.xvideos.com/'

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

Leer_link28 <-read_html(LinksXvideo[28])
Rating_link28 <- html_nodes(Leer_link28, '.rating-inbtn')
Texto_rating28 <- html_text(Rating_link28)
Me_Gusta <- c(Me_Gusta, Texto_rating28[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating28[2])

Leer_link29 <-read_html(LinksXvideo[29])
Rating_link29 <- html_nodes(Leer_link29, '.rating-inbtn')
Texto_rating29 <- html_text(Rating_link29)
Me_Gusta <- c(Me_Gusta, Texto_rating29[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating29[2])

Leer_link30 <-read_html(LinksXvideo[30])
Rating_link30 <- html_nodes(Leer_link30, '.rating-inbtn')
Texto_rating30 <- html_text(Rating_link30)
Me_Gusta <- c(Me_Gusta, Texto_rating30[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating30[2])

Leer_link31 <-read_html(LinksXvideo[31])
Rating_link31 <- html_nodes(Leer_link31, '.rating-inbtn')
Texto_rating31 <- html_text(Rating_link31)
Me_Gusta <- c(Me_Gusta, Texto_rating31[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating31[2])

Leer_link32 <-read_html(LinksXvideo[32])
Rating_link32 <- html_nodes(Leer_link32, '.rating-inbtn')
Texto_rating32 <- html_text(Rating_link32)
Me_Gusta <- c(Me_Gusta, Texto_rating32[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating32[2])

Leer_link33 <-read_html(LinksXvideo[33])
Rating_link33 <- html_nodes(Leer_link33, '.rating-inbtn')
Texto_rating33 <- html_text(Rating_link33)
Me_Gusta <- c(Me_Gusta, Texto_rating33[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating33[2])

Leer_link34 <-read_html(LinksXvideo[34])
Rating_link34 <- html_nodes(Leer_link34, '.rating-inbtn')
Texto_rating34 <- html_text(Rating_link34)
Me_Gusta <- c(Me_Gusta, Texto_rating34[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating34[2])

Leer_link35 <-read_html(LinksXvideo[35])
Rating_link35 <- html_nodes(Leer_link35, '.rating-inbtn')
Texto_rating35 <- html_text(Rating_link35)
Me_Gusta <- c(Me_Gusta, Texto_rating35[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating35[2])

Leer_link36 <-read_html(LinksXvideo[36])
Rating_link36 <- html_nodes(Leer_link36, '.rating-inbtn')
Texto_rating36 <- html_text(Rating_link36)
Me_Gusta <- c(Me_Gusta, Texto_rating36[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating36[2])

Leer_link37 <-read_html(LinksXvideo[37])
Rating_link37 <- html_nodes(Leer_link37, '.rating-inbtn')
Texto_rating37 <- html_text(Rating_link37)
Me_Gusta <- c(Me_Gusta, Texto_rating37[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating37[2])

Leer_link38 <-read_html(LinksXvideo[38])
Rating_link38 <- html_nodes(Leer_link38, '.rating-inbtn')
Texto_rating38 <- html_text(Rating_link38)
Me_Gusta <- c(Me_Gusta, Texto_rating38[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating38[2])

Leer_link39 <-read_html(LinksXvideo[39])
Rating_link39 <- html_nodes(Leer_link39, '.rating-inbtn')
Texto_rating39 <- html_text(Rating_link39)
Me_Gusta <- c(Me_Gusta, Texto_rating39[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating39[2])

Leer_link40 <-read_html(LinksXvideo[40])
Rating_link40 <- html_nodes(Leer_link40, '.rating-inbtn')
Texto_rating40 <- html_text(Rating_link40)
Me_Gusta <- c(Me_Gusta, Texto_rating40[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating40[2])

Leer_link41 <-read_html(LinksXvideo[41])
Rating_link41 <- html_nodes(Leer_link41, '.rating-inbtn')
Texto_rating41 <- html_text(Rating_link41)
Me_Gusta <- c(Me_Gusta, Texto_rating41[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating41[2])

Leer_link42 <-read_html(LinksXvideo[42])
Rating_link42 <- html_nodes(Leer_link42, '.rating-inbtn')
Texto_rating42 <- html_text(Rating_link42)
Me_Gusta <- c(Me_Gusta, Texto_rating42[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating42[2])

Leer_link43 <-read_html(LinksXvideo[43])
Rating_link43 <- html_nodes(Leer_link43, '.rating-inbtn')
Texto_rating43 <- html_text(Rating_link43)
Me_Gusta <- c(Me_Gusta, Texto_rating43[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating43[2])

Leer_link44 <-read_html(LinksXvideo[44])
Rating_link44 <- html_nodes(Leer_link44, '.rating-inbtn')
Texto_rating44 <- html_text(Rating_link44)
Me_Gusta <- c(Me_Gusta, Texto_rating44[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating44[2])

Leer_link45 <-read_html(LinksXvideo[45])
Rating_link45 <- html_nodes(Leer_link45, '.rating-inbtn')
Texto_rating45 <- html_text(Rating_link45)
Me_Gusta <- c(Me_Gusta, Texto_rating45[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating45[2])

Leer_link46 <-read_html(LinksXvideo[46])
Rating_link46 <- html_nodes(Leer_link46, '.rating-inbtn')
Texto_rating46 <- html_text(Rating_link46)
Me_Gusta <- c(Me_Gusta, Texto_rating46[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating46[2])

Leer_link47 <-read_html(LinksXvideo[47])
Rating_link47 <- html_nodes(Leer_link47, '.rating-inbtn')
Texto_rating47 <- html_text(Rating_link47)
Me_Gusta <- c(Me_Gusta, Texto_rating47[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating47[2])

Leer_link48 <-read_html(LinksXvideo[48])
Rating_link48 <- html_nodes(Leer_link48, '.rating-inbtn')
Texto_rating48 <- html_text(Rating_link48)
Me_Gusta <- c(Me_Gusta, Texto_rating48[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating48[2])

Leer_link49 <-read_html(LinksXvideo[49])
Rating_link49 <- html_nodes(Leer_link49, '.rating-inbtn')
Texto_rating49 <- html_text(Rating_link49)
Me_Gusta <- c(Me_Gusta, Texto_rating49[1])
No_Me_Gusta <- c(No_Me_Gusta, Texto_rating49[2])

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

# Se genera una variable tipo, donde 0 es la pag de inicio de Xvideo
Tipo <- list("0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0","0")
unlistTipo <- unlist(Tipo)

# Se genera una tabla con los datos obtenidos para Xvideo
df__XVIDEO <- data.frame(LINKS= LinksXvideo, TITULO= textoXVideos, TIPO= unlistTipo, VISITAS= unlistVisitas, ME_GUSTA= unlistMe_Gusta, NO_ME_GUSTA= unlistNo_Me_Gusta)

# Almacenando la informacion en CSV
write.csv(df__XVIDEO, file="Tabla00.csv")
