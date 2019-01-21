###Instalar los packages necesarios####
# install.packages("selectr")
# install.packages("RCulr")
# install.packages("XML")
# install.packages("data.table")

library("rvest")
library("RCulr")
library("selectr")
library("XML")
library(data.table)

#==================== usando Xvideos ====================#

# Inicializando la var de archivo con el nombre de la página a utilizar
paginaXVideos <- 'https://www.xvideos.com/new/1/'

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
  linksXvideo <- print(paste("http://www.xvideos.com",linksVIDEOS,sep = ""))
}

# Viendo que tiene la posicion 1 de la variable todosLosLinksXvideo
print(linksXvideo[1])

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

###INTENTO DE EXTRACCION DE INFO DE CADA UNO DE LOS LINKS CON FOR####
# Ojo, antes: Correr funcion Visitas (que esta mas abajo)
### Problema: la funcion for no esta recorriendo todos los links, solo el 1


for(i in 1:length(linksXvideo)){
  print(paste("Link ---->",linksXvideo[[i]],sep = ""))
  Leer_links <- read_html(linksXvideo[[i]])
    Views_links <- html_nodes(Leer_links,'.views-full')
    texto_views <- html_text(Views_links)
    texto_views <- gsub("\n","",texto_views)
    texto_views <- gsub("\t","",texto_views)
    texto_views <- gsub("views","",texto_views)
    texto_views <- gsub(",","",texto_views)
    Rating_links <- html_nodes(Leer_links,'.rating-inbtn')
    texto_rating <- html_text(Rating_links)
    Views_like <- texto_rating[1]
    Views_like <- gsub("k","-k",Views_like)
    Views_like <- gsub("M","-M",Views_like)
    Likes <- strsplit(Views_like,"-")
    for(j in 1:length(Likes)){
      Likes[j] <- VisitasXVideo(Likes[[j]])
    }
    Views_dislike <- texto_rating[2]
    Views_dislike <- gsub("k","-k",Views_dislike)
    Views_dislike <- gsub("M","-M",Views_dislike)
    Dislikes <- strsplit(Views_dislike,"-")
    for(j in 1:length(Dislikes)){
      Dislikes[j] <- VisitasXVideo(Dislikes[[j]])
    }  
}

## EJEMPLO EXTRACCION DATOS 
#### VIEWS, LIKES Y DISLIKES, PRUEBA LINK 1

prueba1 <- read_html(linksXvideo[[1]])
contenidoprueba1  <- html_nodes(prueba1,'.views-full')
textoprueba1 <- html_text(contenidoprueba1)

textoprueba1 <- gsub("\n","",textoprueba1)
textoprueba1 <- gsub("\t","",textoprueba1)  
textoprueba1 <- gsub("views","",textoprueba1)   
textoprueba1 <- gsub(",","",textoprueba1)   

contenidoprueba1_2  <- html_nodes(prueba1,'.rating-inbtn')
textoprueba1_2 <- html_text(contenidoprueba1_2)
Views_positivas <- textoprueba1_2[1]
Views_positivas <- gsub("k","-k",Views_positivas)
Split_positivas <- strsplit(Views_positivas,"-")
for(i in 1:length(Split_positivas)){
  Split_positivas[i] <- VisitasXVideo(Split_positivas[[i]])
}
Views_negativas <- textoprueba1_2[2]
Views_negativas <- gsub("k","-k",Views_negativas)
Split_negativas <- strsplit(Views_negativas,"-")
for(i in 1:length(Split_negativas)){
  Split_negativas[i] <- VisitasXVideo(Split_negativas[[i]])
}  


###EXTRACCION VISITAS POR VIDEO DESDE LINK PAG GENERAL#####
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


#==================== UNA GRAN TABLA ====================#

# Creando una tabla con mas de una columna
dfvideos <- data.frame(LINKS = todosLosLinksXvideo, TITULO = textoXVideos, DURACION = DuracionXVideos, VIEWS = transpose(Visitas))

# Se detecta error en el titulo de "VISITAS", no hay caso
# AYUDA AMARU :c 

##### Se guardan los datos porque hay que empezar a tener algo ya po :c
#alamacenando la informacion en CSV
write.csv(dfvideos, file="01TablaXVideos.csv")

#rbin recordar# 

# Tablas datos por separado

# Tabla de los titulos de la pág 1 de new 
tabla_titulos <- table(textoXVideos)

# Transformando a data framtabla
tituloXVideos <- as.data.frame(tabla_titulos)

# Unificando los títulos
todosLosTitulosXVideo <- ""
for(i in 1 : length(textoXVideos)){
  todosLosTitulosXVideo <- paste(todosLosTitulosXVideo," ",textoXVideos[[i]])
}

# Separando las palabras por espacio
todosLosTitulosXVideo <- strsplit(todosLosTitulosXVideo," ")[[1]]

# Pasando todas las palabras a minúsculas
todosLosTitulosXVideo <- tolower(todosLosTitulosXVideo)

# Contando palabras
unlistTitulosXVideos <- unlist(todosLosTitulosXVideo)
tablaXVideos <- table(unlistTitulosXVideos)

# Transformando a data framtabla
tituloXVideos <- as.data.frame(tablaXVideos)

#Duracion de videos en tabla
tabla_duracion <- table(DuracionXVideos)
Duracion_tabla <- as.data.frame(tabla_duracion)

# Tabla de numero de visitas
# El "transpose" es para que quede en vertical
df <- data.frame("vistas" = transpose(a))