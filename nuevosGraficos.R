install.packages("easypackages") #para llamar a varias librerias al mismo tiempo
install.packages("ggcorrplot")
install.packages("treemapify")
library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont","plotly")  #  ó todo el resto

library(ggcorrplot)
library(treemapify)
)
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
names(datosParlamento)[6] = "masJoven"
names(datosParlamento)[7] = "nroIntegrantes"
names(datosParlamento)[8] = "codPais"
# valores mundiales sin año
mundo<- datosParlamento%>% group_by(codPais, pais)%>% summarise(totalporcenM=(sum(porcentaje_mujeres,na.rm = TRUE)),totalMujeres=round((totalporcenM*10), 0), totalIntegrantes=(sum(numero_integrantes, na.rm = TRUE)),totalporcenH=(100- totalporcenM))%>%arrange(totalporcenM)
View(mundo)

mundoConMujeres <-mundo%>% filter(totalporcenM!=0)
View(mundoConMujeres)

##--------------------------------------------------------------------------------------------------
#CÁMARA ALTA O SENADO
#--------------------------------------------------------------------------------------------------
#Agrupo por cámara alta o senado
camaraAlta<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="alta")%>%
  summarise(totalPorcenCAlta=(sum(porcentaje_mujeres,na.rm = TRUE)), totalIntegrantesCAlta=(sum(numero_integrantes, na.rm = TRUE)))%>%arrange(totalPorcenCAlta)
View(camaraAlta)

# selecciono sólo los que tienen representacion en cámara baja o única
camaraAltaSi <-camaraAlta %>%filter(totalPorcenCAlta>0)
View(camaraAltaSi)

camaraAltaLatam <- filter(camaraAltaSi, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
View(camaraAltaLatam)


#países sin representacion en camara alta: 
paisesSinMujeresCAlta <- camaraAlta%>% filter (totalPorcenCAlta ==0)
View(paisesSinMujeresCAlta)

#Paleta colores
pL<-lacroix_palette("PassionFruit", n = 19, type = "continuous")
p<-lacroix_palette("PassionFruit", n = 580, type = "continuous")
pM<-lacroix_palette("PassionFruit", n = 189, type = "continuous")
p1<-lacroix_palette("Pamplemousse",n=580, type = "continuous")
pL1<-lacroix_palette("Pamplemousse",n = 19, type = "continuous")
pM1<-lacroix_palette("Pamplemousse",n=189, type = "continuous")

p2<-lacroix_palette("PeachPear", n = 580, type = "continuous")
pL2<-lacroix_palette("PeachPear", n = 19, type = "continuous")
pL22<-lacroix_palette("PeachPear", n = 12, type = "continuous")
pM2<-lacroix_palette("PeachPear", n = 189, type = "continuous")

p3<-lacroix_palette("PeachPear", type = "paired")

#---------------------------------------------------------------------------------------------
treemap<- ggplot(camaraAltaLatam, 
       aes(area = totalPorcenCAlta, fill = pais, label = pais,
           '', totalPorcenCAlta )) +
  geom_treemap() +
  geom_treemap_text(colour = "white",
                    place = "centre",
                    grow = F,
                    reflow = T) +
  scale_fill_manual(values = pL2) +
  labs(title = "") +
  theme_bw() +
  theme(legend.position = "none")
  
treemap


treemap +transition_states(totalPorcenCBaja) +
  shadow_mark()

Latam + transition_time(importaXP) +
  ease_aes('linear')+
  
  
ggplotly(treemap)
#ggploty(mujeres)















prueba <-camaraAltaLatam%>% select(totalPorcenCAlta, totalIntegrantesCAlta, codPais) 
corr <- round(cor(prueba), 1)
corr
ggcorrplot(camaraAltaLatam) +
  ggtitle("Correlograma del conjunto mtcars") +
  theme_minimal()


ggcorrplot(camaraAltaLatam, method = 'circle') +
  ggtitle("Correlograma del conjunto mtcars") +
  theme_minimal()


ggplot(data = camaraAltaLatam, aes(x = pais, y = totalPorcenCAlta)) + 
  geom_jitter(size = 1, color = 'gray', alpha = 0.5) +
  geom_violin(aes(fill = pais), color = 'black', alpha = 0.8) +
  xlab('Género') + 
  ylab('Puntuación de la Audiencia') +
  ggtitle('Puntuación de la Audiencia según el Género') + 
  theme_minimal()