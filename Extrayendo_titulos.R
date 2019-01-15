library("rvest")

###Instalando mil paquetes para ver si sirve el Xpath####
install.packages("selectr")
install.packages("RCulr")
install.packages("XML")

library("RCulr")
library("selectr")
library("XML")

#==================== usando xvideos ====================#

paginaXVideos <- 'https://www.xvideos.com/new/1/'

webpageXVideos <- read_html(paginaXVideos)

#Extraccion del texto contenido en la clase thumb-under
contenidoWebXVideos <- html_nodes(webpageXVideos,'.thumb-under > p > a')
print (contenidoWebXVideos)

# viendo el contenido de la posición 1
# de la variable contenidoWebXVideos
print(contenidoWebXVideos[1])

# Extrayendo el texto de contenidoWebXVideos
textoXVideos <- html_text(contenidoWebXVideos)
print(textoXVideos)

# Viendo que tiene la posición 1 la variable textoXVideos
print(textoXVideos[1])

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


#Prueba extraccion de duracion de cada video
DurationXVideos <- html_nodes(webpageXVideos,'.duration')
DuracionXVideos <- html_text(DurationXVideos)

#Duracion de videos en tabla
tabla_duracion <- table(DuracionXVideos)
Duracion_tabla <- as.data.frame(tabla_duracion)

####INTENTANDO EXTRAER VIEWS#####NADA SIRVE### AYUDA####

views <- getNodeSet(webpageXVideos,"//*[@id="video_43754597"]/div[2]/p[2]/span/span[2]","x")

views <- webpageXVideos(Xpath="//*[@id="video_43754597"]/div[2]/p[2]/span/span[2]")

vistaql <- webpageXVideos %>% html_nodes(xpath = ‘//*[@id="video_43754597"]/div[2]/p[2]/span/span[2]’) %>% html_text()
 
getNodeSet(webpageXVideos, "//*[@id="video_43754597"]/div[2]/p[2]/span/span[2]")

viewssss <- html_nodes(webpageXVideos, xpath="//*[@id="video_43754597"]/div[2]/p[2]/span/span[2]")

getNodeSet(//*[@id="video_43721271"]/div[2]/p[2]/span/span[2]

webpageXVideos
tratamientos <- html.definicion.temp %>% html_nodes(xpath = ‘//div[contains(@id,”main”)]’) %>% html_text()