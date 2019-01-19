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
todosLosLinksXvideo <- ""
for(i in 1:27){
  todosLosLinksXvideo <- print(paste("http://www.xvideos.com",linksVIDEOS,sep = ""))
}

# Viendo que tiene la posicion 1 de la variable todosLosLinksXvideo
print(todosLosLinksXvideo[1])

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
  if(entrada[2]=="k"){
    entrada[1] <- as.numeric(entrada[1])*1000
  }
  else if(entrada[2]=="M"){
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