
install.packages("easypackages")  #para llamar a varias librerias al mismo tiempo
library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont")  #  ó todo el resto


# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")
loadfonts(device = "win")
datosParlamento <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")
View(datosParlamento)
write.csv(datos_uip,"datosParlamento.csv")
#datosParlamento <-readr::read_csv("datosParlamento.csv")
dim(datosParlamento)
head(datosParlamento)
tail(datosParlamento)

datosParlamento 
Argentina <-datosParlamento %>%filter(iso_)