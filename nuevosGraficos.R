install.packages("easypackages") #para llamar a varias librerias al mismo tiempo
install.packages("ggcorrplot")
install.packages("treemapify")
library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont","plotly")  #  ó todo el resto

library(ggcorrplot)
library(treemapify)

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



camaraAltaLatam <- filter(camaraAltaSi, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
View(camaraAltaLatam)


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

camaraAltaEuropa <- filter(camaraAltaSi, pais %in% c("Albania","Alemania","Andorra","Armenia","Austria","Azerbaiyán","Bélgica","Bielorrusia","Bosnia y Herzegovina", "Bulgaria","Chipre", "Croacia", "Dinamarca", "Eslovaquia", "España", "Estonia", "Finlandia",  "Francia",  "Georgia", "Grecia", "Hungría","Irlanda", "Islandia", 
                                                     "Italia", "Kazajistán", "Letonia", "Liechtenstein", "Lituania", "Luxemburgo", "Macedonia","Malta", "Moldavia", "Mónaco","Montenegro", "Noruega", "Países Bajos", "Polonia", "Portugal", "Reino Unido", "República Checa", "Rumanía", "Rusia", "San Marino", "Serbia", "Suecia","Suiza", "Turquía", "Ucrania"))



View(camaraAltaEuropa)


#países sin representacion en camara alta: 
paisesSinMujeresCAlta <- camaraAlta%>% filter (totalPorcenCAlta ==0)
View(paisesSinMujeresCAlta)

#Paleta colores
pL<-lacroix_palette("PassionFruit", n = 19, type = "continuous")
p<-lacroix_palette("PassionFruit", n = 580, type = "continuous")
p5<-lacroix_palette("PassionFruit", n = 14, type = "continuous")
pM<-lacroix_palette("PassionFruit", n = 189, type = "continuous")

p1<-lacroix_palette("Pamplemousse",n=580, type = "continuous")
pL1<-lacroix_palette("Pamplemousse",n = 19, type = "continuous")
pM1<-lacroix_palette("Pamplemousse",n=189, type = "continuous")
pL5<-lacroix_palette("Pamplemousse",n=14, type = "continuous")

p2<-lacroix_palette("PeachPear", n = 580, type = "continuous")
pL2<-lacroix_palette("PeachPear", n = 19, type = "continuous")
pL22<-lacroix_palette("PeachPear", n = 12, type = "continuous")
pM2<-lacroix_palette("PeachPear", n = 189, type = "continuous")
pM5<-lacroix_palette("PeachPear", n = 14, type = "continuous")

p3<-lacroix_palette("PeachPear", type = "paired")

#---------------------------------------------------------------------------------------------
#Treemap Naranja - PeachPear
#-----------------------------------------------------------------------------------------
treemap<- ggplot(camaraAltaEuropa, 
       aes(area = totalPorcenCAlta, fill = pais, label = totalPorcenCAlta,
           '', totalPorcenCAlta )) +
  geom_treemap(colour = "black") +
  geom_treemap_text(family="Tahoma",
                    colour = "black",
                    place = "centre",
                    grow = F,
                    reflow = T) +
  scale_fill_manual(values = pM5) +
  labs(title = "Porcentaje de mujeres en parlamentos \n de países pertenecientes al continente Europeo", x="", y=" ",
       caption = "Fuente: #DatosDeMiercoles por Patricia Loto") +
  theme_grey() +
  theme(plot.title=element_text(family="Tahoma", hjust=0.5), plot.caption=element_text(family="Palatino", color = "darkblue"), axis.text.x =element_blank(), axis.text.y =element_blank(), axis.ticks.x = element_blank() , axis.ticks.y = element_blank())     
  
treemap
treemap +transition_states(pais) +
  shadow_mark()

#---------------------------------------------------------------------------------------------
#Treemap Pamplemousse
#-----------------------------------------------------------------------------------------


treemap2<- ggplot(camaraAltaEuropa, 
                 aes(area = totalPorcenCAlta, fill = pais, label = totalPorcenCAlta,
                     '', totalPorcenCAlta )) +
  geom_treemap(colour = "white") +
  geom_treemap_text(family="Tahoma",
                    colour = "white",
                    place = "centre",
                    grow = F,
                    reflow = T) +
  scale_fill_manual(values = pL5) +
  labs(title = "Porcentaje de mujeres en parlamentos \n de países pertenecientes al continente Europeo", x="", y=" ",
       caption = "Fuente: #DatosDeMiercoles  por Patricia Loto") +
  theme_grey() +
  theme(plot.title=element_text(family="Tahoma", hjust=0.5), axis.text.x =element_blank(), axis.text.y =element_blank(), axis.ticks.x = element_blank() , axis.ticks.y = element_blank())     

treemap2
treemap2 +transition_states(pais) +
  shadow_mark()

#---------------------------------------------------------------------------------------------
#Treemap Pamplemousse
#-----------------------------------------------------------------------------------------
treemap3<- ggplot(camaraAltaEuropa, 
                  aes(area = totalPorcenCAlta, fill = pais, label = totalPorcenCAlta,
                      '', totalPorcenCAlta )) +
  geom_treemap(colour = "black") +
  geom_treemap_text(family="Tahoma",
                    colour = "black",
                    place = "centre",
                    grow = F,
                    reflow = T) +
  scale_fill_manual(values = p5) +
  labs(title = "Porcentaje de mujeres en parlamentos \n de países pertenecientes al continente Europeo", x="", y=" ",
       caption = "Fuente: #DatosDeMiercoles por Patricia Loto") +
  theme_grey() +
  theme(plot.title=element_text(family="Tahoma", hjust=0.5),plot.caption=element_text(family="Palatino", color = "black", face="bold"), axis.text.x =element_blank(), axis.text.y =element_blank(), axis.ticks.x = element_blank() , axis.ticks.y = element_blank())     

treemap3
treemap3 +transition_states(pais) +
  shadow_mark()









treemap + transition_time(totalPorcenCBaja) +
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