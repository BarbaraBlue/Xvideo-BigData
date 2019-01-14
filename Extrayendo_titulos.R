#==================== usando xvideos ====================#

paginaXVideos <- 'https://www.xvideos.com/new/2'

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
