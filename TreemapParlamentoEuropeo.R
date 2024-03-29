install.packages("easypackages") #para llamar a varias librerias al mismo tiempo
install.packages("treemapify")
library(easypackages)

libraries("tidyverse","here","janitor","gganimate","gifski","LaCroixColoR", "extrafont","treemapify")  #  � todo el resto
library(treemapify)

# Se realiza por �nica vez
font_import()
fonts()
loadfonts(device = "win")
#------------------------------------------------------------------------------------------
#Lectura y limpieza
#------------------------------------------------------------------------------------------
datosParlamento <- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-05-08/datos_uip.csv")
View(datosParlamento)

#verifico los datos
dim(datosParlamento)
head(datosParlamento)
tail(datosParlamento)

datosParlamento <- clean_names(datosParlamento)
names(datosParlamento)[8] = "codPais"

# valores mundiales 
mundoA<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="alta")%>%summarise(totalporcenM=(sum(porcentaje_mujeres,na.rm = TRUE)),totalMujeres=round((totalporcenM*10), 0), totalIntegrantes=(sum(numero_integrantes, na.rm = TRUE)),totalporcenH=(100- totalporcenM))%>%arrange(desc(totalporcenM))
View(mundoA)

##--------------------------------------------------------------------------------------------------
#C�MARA ALTA O SENADO
#--------------------------------------------------------------------------------------------------
#Agrupo por c�mara alta o senado
camaraAlta<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="alta")%>%
  summarise(totalPorcenCAlta=(sum(porcentaje_mujeres,na.rm = TRUE)), totalIntegrantesCAlta=(sum(numero_integrantes, na.rm = TRUE)))%>%arrange(totalPorcenCAlta)
View(camaraAlta)

#Selecciono s�lo los que tienen representacion en c�mara alta
camaraAltaSi <-camaraAlta %>%filter(totalPorcenCAlta>0)
View(camaraAltaSi)
#Selecciono los pa�ses pertenecientes al continente europeo
camaraAltaEuropa <- filter(camaraAltaSi, pais %in% c("Albania","Alemania","Andorra","Armenia","Austria","Azerbaiy�n","B�lgica","Bielorrusia","Bosnia y Herzegovina", "Bulgaria","Chipre", "Croacia", "Dinamarca", "Eslovaquia", "Espa�a", "Estonia", "Finlandia",  "Francia",  "Georgia", "Grecia", "Hungr�a","Irlanda", "Islandia", 
                                                     "Italia", "Kazajist�n", "Letonia", "Liechtenstein", "Lituania", "Luxemburgo", "Macedonia","Malta", "Moldavia", "M�naco","Montenegro", "Noruega", "Pa�ses Bajos", "Polonia", "Portugal", "Reino Unido", "Rep�blica Checa", "Ruman�a", "Rusia", "San Marino", "Serbia", "Suecia","Suiza", "Turqu�a", "Ucrania"))
View(camaraAltaEuropa)

#pa�ses sin representacion en camara alta: 
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



#---------------------------------------------------------------------------------------------
#Treemap PeachPear
#-----------------------------------------------------------------------------------------
pM5<-lacroix_palette("PeachPear", n = 14, type = "continuous")
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
  labs(fill ="", title = "Porcentaje de mujeres en parlamentos \n de pa�ses pertenecientes al continente Europeo", x="", y=" ",
       caption = "Fuente: #DatosDeMiercoles por Patricia Loto") +
  theme_grey() +
  theme(plot.title=element_text(family="Tahoma", hjust=0.5), plot.caption=element_text(family="Palatino", color = "darkblue"), axis.text.x =element_blank(), axis.text.y =element_blank(), axis.ticks.x = element_blank() , axis.ticks.y = element_blank())     

treemap
ggsave("TreemapParlamentoEuropeo.png",width = 10, height = 5, dpi = "retina")

#agregaAnimaci�n
treemap +transition_states(pais) +
  shadow_mark()

#---------------------------------------------------------------------------------------------
#Treemap Pamplemousse
#-----------------------------------------------------------------------------------------
pL5<-lacroix_palette("Pamplemousse",n=14, type = "continuous")
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
  labs(fill ="", title = "Porcentaje de mujeres en parlamentos \n de pa�ses pertenecientes al continente Europeo", x="", y=" ",
       caption = "Fuente: #DatosDeMiercoles  por Patricia Loto") +
  theme_grey() +
  theme(plot.title=element_text(family="Tahoma", hjust=0.5), plot.caption=element_text(family="Palatino", color = "pink", face="bold"), axis.text.x =element_blank(), axis.text.y =element_blank(), axis.ticks.x = element_blank() , axis.ticks.y = element_blank())     

treemap2
ggsave("TreemapParlamentoEuropeo2.png", width = 10, height = 5, dpi = "retina")

#agregaAnimaci�n
treemap2 +transition_states(pais) +
  shadow_mark()

#----------------------------------------------------------------------------------------
#Treemap Passion Fruit
#-----------------------------------------------------------------------------------------
p5<-lacroix_palette("PassionFruit", n = 14, type = "continuous")
treemap3<- ggplot(camaraAltaEuropa, aes(area = totalPorcenCAlta, fill = pais, label = totalPorcenCAlta)) +
           geom_treemap(colour = "black", facet="bold") +
           geom_treemap_text(family="Tahoma", colour = "black", place = "centre",grow = F, reflow = T) +
           scale_fill_manual(values = p5) +
           labs(fill ="", title = "Porcentaje de mujeres en parlamentos \n de pa�ses pertenecientes al continente Europeo",
                 x="", 
                 y=" ",
                 caption = "Fuente: #DatosDeMiercoles por Patricia Loto") +
           theme_grey() +
           theme(plot.title=element_text(family="Tahoma", hjust=0.5),
                 plot.caption=element_text(family="Palatino", 
                 color = "black", face="bold"), axis.text.x =element_blank(), axis.text.y =element_blank(),
                 axis.ticks.x = element_blank() , axis.ticks.y = element_blank())     

treemap3
#Guarda gr�fico
ggsave("TreemapParlamentoEuropeo3.png", width = 10, height = 5, dpi = "retina")
#agregaAnimaci�n
treemap3 +transition_states(pais) +
  shadow_mark()

