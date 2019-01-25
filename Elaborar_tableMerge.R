### Instalar los packages necesarios ###
# install.packages("rvest")
# install.packages("data.table")
# install.packages("ggplot2")
install.packages("ggthemes")

### Llamar los packages a utilizar ###
library('rvest')
library(data.table)
library(ggplot2)
library(ggthemes)

##### Utilizando datas extraidas de XVideo #####
tablaMerge <- rbind(df__XVIDEO,dfANAL,dfBIG,dfBORRACHA,dfFORCED,dfGAY,dfLESBIAN,dfLOVE,dfHENTAI)

# Almacenando la informacion en CSV
write.csv(tablaMerge, file="TablaMERGE.csv")

tablaMerge %>%
  ggplot() +
  aes(x = TITULO, y= VISITAS) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Blues") 


ggplot(datos, aes(nombres, valores)) +
  geom_col(fill = rgb(0.2, 0.2, 1, 0.3), color = "blue") +
  theme_minimal()