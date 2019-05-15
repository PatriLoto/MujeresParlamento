install.packages("easypackages") #para llamar a varias librerias al mismo tiempo
install.packages("ggcorrplot")

library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont","plotly")  #  ó todo el resto

library(ggcorrplot)


# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")
# Se realiza por única vez
font_import()
fonts()
loadfonts(device = "win")
#------------------------------------------------------------------------------------------
#Lectura y limpieza
#------------------------------------------------------------------------------------------
datosParlamento <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")
View(datosParlamento)
write.csv(datos_uip,"datosParlamento.csv")
#datosParlamento <-readr::read_csv("datosParlamento.csv")
dim(datosParlamento)
head(datosParlamento)
tail(datosParlamento)

datosParlamento <- clean_names(datosParlamento)
#names(datosParlamento)[6] = "masJoven"
#names(datosParlamento)[7] = "nroIntegrantes"
names(datosParlamento)[8] = "codPais"
# valores mundiales sin año
mundoA<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="alta")%>%summarise(totalporcenM=(sum(porcentaje_mujeres,na.rm = TRUE)),totalMujeres=round((totalporcenM*10), 0), totalIntegrantes=(sum(numero_integrantes, na.rm = TRUE)),totalporcenH=(100- totalporcenM))%>%arrange(desc(totalporcenM))
View(mundoA)


#Para probar a futuro: gráfico correlación y violin
#------------------------------------------------------------------------------------------------------------------------
prueba <-camaraAltaLatam%>% select(totalPorcenCAlta, totalIntegrantesCAlta, codPais) 
corr <- round(cor(prueba), 1)
corr
ggcorrplot(camaraAltaLatam) +
  ggtitle("Correlograma del conjunto mtcars") +
  theme_minimal()


ggcorrplot(camaraAltaLatam, method = 'circle') +
  ggtitle("Correlograma del conjunto mtcars") +
  theme_minimal()

#------------------------------------------------------------------------------------------------------------------------
#Gráfico violin
ggplot(data = camaraAltaLatam, aes(x = pais, y = totalPorcenCAlta)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = pais), color = 'black', alpha = 0.8) +
  xlab('Género') + 
  ylab('Puntuación de la Audiencia') +
  ggtitle('Puntuación de la Audiencia según el Género') + 
  theme_minimal()