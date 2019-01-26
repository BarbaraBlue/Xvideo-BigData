### Instalar los packages necesarios ###
# install.packages("rvest")
# install.packages("data.table")
# install.packages("ggplot2") 

### Llamar los packages a utilizar ###
library('rvest')
library(data.table)
library(ggplot2)

#==================== usando Xvideos ====================#

# Se medira el tiempo que se demora en ejecutar el codigo
# Desde el inicio a la creacion final de la data
t <- proc.time()

# Data frame encargado de almacenar la informacion
BIG_DDATA = data.frame()

for (i in 1:20){
  print(paste("https://www.xvideos.com/new/",i,"/",sep = ""))
  # Leyendo el contenido de las paginas
  web_XVideos <- read_html(paste("https://www.xvideos.com/new/",i,"/",sep = ""))
  
  # Extraccion del texto contenido en la clase thumb-under
  contenidoWebXVideos <- html_nodes(web_XVideos,'.thumb-under > p > a')
  
  # Extrayendo los links de los videos
  linksVIDEOS <- html_attr(contenidoWebXVideos,"href")
  
  # Arreglando los links de todos los videos utilizando for
  for(i in 1:length(linksVIDEOS)){
    LinksXvideo <- print(paste("http://www.xvideos.com",linksVIDEOS,sep = ""))
  }
  
  # Viendo que tiene la posicion 1 de la variable todosLosLinksXvideo
  print(LinksXvideo[1])
  
  # Viendo cuantas variables tiene LinksXvideo
  length(LinksXvideo)
  
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
  
  # Extraccion de duracion de cada video
  DurationXVideos <- html_nodes(webpageXVideos,'.duration')
  
  # Limpieza de los datos de duracion
  DuracionXVideos <- html_text(DurationXVideos)
  
  # Separando por espacio para luego reemplazar min y sec por valores numericos
  Split_DuracionXVideos <- strsplit(DuracionXVideos," ")
  
  # Se elimina el min y el sec para dejarlos en valores numericos creando una funcion
  # TiempoXVideo: string -> double
  # TiempoXVideo: entrega la cantidad de tiempo en minutos de cada video
  # si aparece min se multiplica el numero por uno para mantenerlo en minutos 
  # si aparece sec se divide el numero por sesenta para pasarlo a minutos
  # Ejemplo: TiempoXVideo(35 sec)-> "0,5833"
  TiempoXVideo <- function (entrada){
    if(entrada[2]=="min"){
      entrada[1] <- as.numeric(entrada[1])*1
    }else if(entrada[2]=="sec"){
      entrada[1] <- as.numeric(entrada[1])/60
    }
  }
  
  # Recorriendo la lista de duracion aplicando la funcion creada
  for(i in 1:length(Split_DuracionXVideos)){
    Split_DuracionXVideos[i] <- TiempoXVideo(Split_DuracionXVideos[[i]])
  }
  
  # Genero un data frame para almacenar los datos de likes y dislikes 
  Data_1<-data.frame()
  
  #Funcion for para extraer likes y dislikes
  for(i in 1:length(LinksXvideo)){
    # Print para ir viendo en que link va
    print(paste("Link ---->",LinksXvideo[[i]],sep = ""))
    # Se lee el contenido de cada link
    Leer_links <- read_html(LinksXvideo[[i]])
    # Se extrae el contenido de titulos para poder graficar titulos, likes, dislikes
    Titulos_links <- html_nodes(Leer_links, 'h2')
    # Se extrae el texto de Titulos_links
    title_links <- html_text(Titulos_links)
    # Se genera una lista con los titulos de los videos
    titles_links <- title_links[2]
    # Extrayendo el total de las reproducciones
    Visitas_links <- html_nodes(Leer_links,'.views-full')
    # Extrayendo el texto de Views_links
    Views_links <- html_text(Visitas_links)
    # Limpiando los datos de Views_links
    Views_links <- gsub("\n","",Views_links)
    Views_links <- gsub("\t","",Views_links)
    Views_links <- gsub("views","",Views_links)
    Views_links <- gsub(",","",Views_links)
    # Se extrae la informacion de valorizacion
    Rating_links <- html_nodes(Leer_links,'.rating-inbtn')
    # Se extrae el texto de Rating_links
    texto_rating <- html_text(Rating_links)
    # Se obtiene el primer dato de los likes
    Views_like <- texto_rating[1]
    # Limpieza de los likes
    Views_like <- gsub("k","-k",Views_like)
    Views_like <- gsub("M","-M",Views_like)
    # Se separa para luego aplicar la funcion VisitasXVideo
    Likes <- strsplit(Views_like,"-")
    # Se aplica la funcion para dejarlo en valores numericos
    for(j in 1:length(Likes)){
      Likes[j] <- VisitasXVideo(Likes[[j]])
    }
    # Se obtiene el primer dato de dislikes
    Views_dislike <- texto_rating[2]
    # Limpieza datos dislikes
    Views_dislike <- gsub("k","-k",Views_dislike)
    Views_dislike <- gsub("M","-M",Views_dislike)
    # Se separan los datos para luego aplicar funcion VisitasXVideo
    Dislikes <- strsplit(Views_dislike,"-")
    # Se aplica funcion VisitasXVideo para tener valores numericos
    for(j in 1:length(Dislikes)){
      Dislikes[j] <- VisitasXVideo(Dislikes[[j]])
    } 
    # Se extrae el contenido para extraer comentarios del video
    Contenido_comentarios <- html_nodes(Leer_links, '.nb-video-comments')
    # Se extrae el texto para llegar al valor numerico
    Comentarios <- html_text(Contenido_comentarios)
    # Se determina como numero el valor obtenido y se arregla la duplicacion de datos
    Comments <- as.numeric(Comentarios[1])
  # Se genera una data temporal para almacenar las variables
    temp <- data.frame(TITLE=titles_links, VIEWS=unlist(as.numeric(Views_links)) ,LIKES = as.numeric(unlist(Likes[j])), DISLIKES = as.numeric(unlist(Dislikes[j])), COMMENTS = Comments)
  # Se asigna la data temporal a la creada anteriormente
    Data_1 <- rbind(Data_1,temp)
  }
  
  # Almaceno los otros datos extraidos en una tabla
  data1 <- data.frame(LINKS= LinksXvideo, DURATION=unlist(as.numeric(Split_DuracionXVideos)))
  
  # Agrego la data extraida a la gran tabla 
  BIG_DDATA <- rbind(BIG_DDATA,Data_1)
  
}  
# Con esto se termina de medir el tiempo
proc.time() - t

# En ejecutar desde asignacion de pagina a creacion de tablas se demora casi un minuto

#############################
# user    system    elapsed #
#53.31     5.47     1149.10 #
#############################

# Generando un total de 27 obs con 6 variables

#################################################################################
# No pude combinar las dos tablas, se me duplicaban variables                   #
# Por eso decidi agregar la extraccion de titulos a cada uno de los links       #
# OJO: No es que se duplique la variable, fue a modo de solucion para graficar  #
# data_ <- rbind(data_,DATA_)                                                   #
#################################################################################

# Almacenando la informacion en CSV
write.csv(BIG_DDATA, file="BIG_DDATA.csv")



