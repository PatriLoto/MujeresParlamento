
install.packages("easypackages")  #para llamar a varias librerias al mismo tiempo
library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont","plotly")  #  ó todo el resto

library(LaCroixColoR)
library(extrafont)
library(plotly)
library(gganimate)
# se debe instalar la fuente en wintendo
# font_import(paths = "R/2019/2019-04-17/")

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

#Agrupo por cámara baja o única sin año

camaraBaja<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="baja" | camara =="única")%>%
  summarise(totalPorcenCBaja=(sum(porcentaje_mujeres,na.rm = TRUE)), totalIntegrantesCBaja=(sum(numero_integrantes, na.rm = TRUE)))%>%arrange(totalPorcenCBaja)
View(camaraBaja)
# selecciono sólo los que tienen representacion en cámara baja o única
camaraBajaSi <-camaraBaja %>%filter(totalPorcenCBaja>0)
View(camaraBajaSi)

camaraBajaLatam <- filter(camaraBaja, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
View(camaraBajaLatam)
#países sin representacion en camara baja: Afghanistán, Benin, España,	Micronesia,Indonesia,India, Nigeria,Panamá, Papúa Nueva Guinea,Vanuatu
paisesSinMujeresCBaja <- camaraBaja%>% filter (totalPorcenCBaja ==0)
View(paisesSinMujeresCBaja)

#Agrupo por cámara alta o senado
camaraAlta<- datosParlamento%>% group_by(codPais, pais)%>% filter (camara =="alta")%>%
  summarise(totalPorcenCAlta=(sum(porcentaje_mujeres,na.rm = TRUE)), totalIntegrantesCAlta=(sum(numero_integrantes, na.rm = TRUE)))%>%arrange(totalPorcenCAlta)
View(camaraAlta)

# selecciono sólo los que tienen representacion en cámara baja o única
camaraAltaSi <-camaraBaja %>%filter(totalPorcenCBaja>0)
View(camaraAltaSi)


#países sin representacion en camara alta: 
paisesSinMujeresCAlta <- camaraAlta%>% filter (totalPorcenCAlta ==0)
View(paisesSinMujeresCAlta)

#Paleta colores
pL<-lacroix_palette("PassionFruit", n = 19, type = "continuous")
p<-lacroix_palette("PassionFruit", n = 580, type = "continuous")
pM<-lacroix_palette("PassionFruit", n = 189, type = "continuous")
p1<-lacroix_palette("Pamplemousse",n=580, type = "continuous")
pL1<-lacroix_palette("Pamplemousse")
pM1<-lacroix_palette("Pamplemousse",n=189, type = "continuous")

p2<-lacroix_palette("PeachPear", n = 580, type = "continuous")
pL2<-lacroix_palette("PeachPear", n = 19, type = "continuous")
pM2<-lacroix_palette("PeachPear", n = 189, type = "continuous")

p3<-lacroix_palette(type = "paired")

#------------------------------------------------------------------------------------------------------------------------------
# Gráficos Cámara baja
#------------------------------------------------------------------------------------------------------------------------------
# con PLOTLY con porcentaje de mujeres en camara Baja o única

mujeresCBajalat <- ggplot(data =camaraBajaLatam, aes(x=(pais), y=totalPorcenCBaja, colour=pais, fill=totalPorcenCBaja, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  geom_bar(size=2, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =pL) +  
 #scale_fill_manual(values =pL) +
  labs (x = "País", y = "Porcentaje", 
        title= ("Participación femenina en Cámara baja o única"),
  caption = " Fuente: #DatosdeMiercoles", legend=" ") + # agrego título al gráfico
  theme (axis.text.x =element_blank(),
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(#family="Comic Sans MS",
                                 # size=rel(1), 
                                   size = 14,
                                   vjust=1, 
                                   hjust=0.5,                        #Para separarlo del gráfico
                                   #position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="white", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "pink", color = "pink"),
         text = element_text(family = "Palatino", colour = "white", size = 14))
mujeresCBajalat
#plotly
ggplotly(mujeresCBajalat, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)



xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = '')
             # tickcolor = 'rgb(204, 204, 204)',
              #tickwidth = 2,
              #ticklen = 5,
              #tickfont = list(family = 'Arial',
               #               size = 12,
                #              color = 'rgb(82, 82, 82)'))

#------------------------------------------------------------------------------------------------------------------------------

# con PLOTLY con porcentaje de mujeres en camara Baja o única

mujeresCBajaLATAM <- ggplot(data =camaraBajaLatam, aes(x=(pais), y=totalPorcenCBaja, colour=pais, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  geom_bar(size=2, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =pL) +  
  scale_fill_manual(values =p) +
  labs (x = "País", y = "Porcentaje", 
        title= ("Participación femenina en Cámara baja o única"),
        caption = " Fuente: #DatosdeMiercoles", legend=" ") + # agrego título al gráfico
  theme (axis.text.x =element_blank(),
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(#family="Comic Sans MS",
           # size=rel(1), 
           size = 14,
           vjust=1, 
           hjust=0.5,                        #Para separarlo del gráfico
          # position_identity(center),   
           face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
           color="white", #Color del texto  color=maroon, lightblue
           lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "pink", color = "pink"),
         text = element_text(family = "Palatino", colour = "white", size = 14))
mujeresCBajaLATAM
#plotly
ggplotly(mujeresCBajaLATAM, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)


#-----------------------------------------------------------------------------------------
# camara baja LATAM
Latam <- ggplot(camaraBajaLatam, aes(reorder(pais, totalPorcenCBaja), totalPorcenCBaja, size =(totalPorcenCBaja), text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalPorcenCBaja, '%' ))) + 
  #geom_point(show.legend = F) + 
  geom_col(aes(fill=pais)) +
  scale_fill_manual(values =pL) +  #pL2
  coord_flip()+ 
  theme_classic()+   #theme_wsj()theme_classic() +theme_economist()
  labs(title = "Participación femenina en Cámara baja o única  \n",
       subtitle = "Período:2017",
       x = "País",
       y = "Porcentaje",
       caption="#DatosDeMiercoles por Patricia Loto", legend=" ") +
  theme(axis.text.x =element_text(color="black", size=9),  #
        axis.text.y =element_text(color="black", size=9),
        plot.caption = element_text(color = "brown", face ="bold", size = 10, vjust=1),  ##562457
        plot.title = element_text(size=10,
                                  family = "Palatino",
                                  #size=rel(0.4),
                                  vjust=2,
                                  hjust=0.5,
                                  #position_identity(center),   
                                  face="bold",       
                                  color="black",     
                                  lineheight=1.2), legend.position= " ", legend.text= element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 11))
Latam
ggplotly(Latam, hoverformat='2.F', tooltip = "text")


     #   text = element_text(family = "Tahoma", colour = "white", size = 14))
  
  #theme(aspect.ratio=1)
  transition_time(importaXP) +
  ease_aes('linear')+
  shadow_mark(alpha = 1, size = 2)
  
  #gganimate
  
  ggplot(camaraBajaLatam, aes(totalPorcenCBaja, totalIntegrantesCBaja, size = totalPorcenCBaja, colour = pais)) +
    geom_point(alpha = 1, show.legend = TRUE) +
    scale_colour_manual(values = pM1) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    
   # facet_wrap(~continent) +
    # Here comes the gganimate specific bits
    labs(title = 'Pordentaje: round({frame_time},2)', x = 'Porcentaje Participación', y = 'Total Integrantes') +
    transition_time(totalPorcenCBaja) +
    shadow_mark(alpha = 1, size = 2)
   # ease_aes('linear')
  
  
  mujeresCBajaLATAM
  
  p <- ggplot(camaraBajaLatam, aes(pais, totalPorcenCBaja, fill = totalPorcenCBaja)) +
    geom_col() +
   scale_fill_distiller( palette=  "maroon", direction = 1) +
  # scale_fill_manual(values =pL)+
    theme_minimal() +
    labs(title = "Participación femenina en Cámara baja o única  \n",
         subtitle = "Período:2017",
         x = "País",
         y = "Porcentaje",caption="#DatosDeMiercoles por Patricia Loto", legend=" ")+
    theme(axis.text.x =element_text(angle=45,color="black", size=9),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(color = "white"),
      panel.ontop = TRUE)
  p

  p + transition_states(totalPorcenCBaja, wrap = FALSE) +
    shadow_mark()+
    enter_grow() +
    enter_fade()
  
  #------------------------------------------------------------------------------------------------------------------------------
# Gráficos Cámara alta
#------------------------------------------------------------------------------------------------------------------------------



















#--------------------------------------------------------------------------------------
#GRAFICOS paises con porcentaje de mujeres 
#-----------------------------------------------------------------------------------------
#ggplot sencillo

mujeres <- ggplot(data =mundo, aes(x=(pais), y=totalMujeres, colour=pais, text = paste('Porcentaje:', totalporcenM, '\n País:', pais))) + 
  geom_bar(size=1.5, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =p) +  
  #scale_fill_manual(values =c("turquoise", "maroon")) +
  labs (x = "País", y = "Porcentaje (%)", 
        title= ("Mujeres en el Parlamento"))+
        #caption = " Fuente: #DatosdeMiercoles", legend=" ")+ # agrego título al gráfico
  theme (axis.text.x =element_blank(),
           #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1),                      #Tamaño relativo de la letra del título
                                   vjust=1, 
                                   hjust=0.5,                        #Para separarlo del gráfico
                                   position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="black", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "black", color = "black"),
         text = element_text(family = "Roboto Condensed", colour = "white", size = 12))+

#-----------------------------------------------------------------------------------------

# con PLOTLY con total de mujeres en todas las camaras

mujeres <- ggplot(data =mundo, aes(x=(pais), y=totalMujeres, colour=pais, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalMujeres, '%' ))) + 
  geom_bar(size=1.5, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =p) +  
  scale_fill_manual(values =p) +
  labs (x = "País", y = "Total", 
        title= (""))+
  #caption = " Fuente: #DatosdeMiercoles", legend=" ")+ # agrego título al gráfico
  theme (axis.text.x =element_blank(),axis.ticks = ()
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1),                      #Tamaño relativo de la letra del título
                                   vjust=1, 
                                   hjust=0.5,                        #Para separarlo del gráfico
                                   position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="white", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "pink", color = "pink"),
         text = element_text(family = "Tahoma", colour = "white", size = 14))
mujeres
ggplotly(mujeres, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)


xaxis <- list(title = "",
              showline = TRUE,
              showgrid = FALSE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

#-----------------------------------------------------------------------------------------
# con PLOTLY con porcentaje total en todas las camaras

mujeres <- ggplot(data =mundo, aes(x=(pais), y=totalporcenM, colour=pais, text = paste('<b>País:</b>', pais, '\n <b> Mujeres:</b>', totalporcenM, '%'))) + 
  geom_bar(size=1.5, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =p) +  
  #scale_fill_manual(values =c("turquoise", "maroon")) +
  labs (x = "País", y = "Porcentaje", 
        title= ("Mujeres en el Parlamento"))+
  #caption = " Fuente: #DatosdeMiercoles", legend=" ")+ # agrego título al gráfico
  theme (axis.text.x =element_blank(),
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1),                      #Tamaño relativo de la letra del título
                                   vjust=1, 
                                   hjust=0.5,                        #Para separarlo del gráfico
                                   position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="white", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "pink", color = "pink"),
         text = element_text(family = "Verdana", colour = "white", size = 12))

ggplotly(mujeres, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)

#-----------------------------------------------------------------------------------------
#con GGANIMATE
mujeres <- ggplot(data =mundo, aes(x=(pais), y=totalporcenM, colour=pais, text = paste('Porcentaje:', totalporcenM, '%', '\n País:', pais))) + 
  geom_bar(size=1.5, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =p) +  
  #scale_fill_manual(values =c("turquoise", "maroon")) +
  labs (x = "País", y = "Porcentaje (%)", 
        title= ("Mujeres en el Parlamento"))+
  #caption = " Fuente: #DatosdeMiercoles", legend=" ")+ # agrego título al gráfico
  theme (axis.text.x =element_blank(),
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1),                      #Tamaño relativo de la letra del título
                                   vjust=1, 
                                   hjust=0.5,                        #Para separarlo del gráfico
                                   position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="black", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "black", color = "black"),
         text = element_text(family = "Tahoma", colour = "white", size = 12))+
  
  # Here comes the gganimate specific bits
  transition_time(totalporcenM) +
  #shadow_mark(alpha = 0.7, size = 0.7)          #DEJA LAS MARCAS
  shadow_wake(wake_length = 0.1)


ggplotly(mujeres, hoverformat='2.F', tooltip = "text")
ggploty(mujeres)
#-----------------------------------------------------------------------------------------
#comparativo países de latinoamerica
#-----------------------------------------------------------------------------------------
latam <- filter(datosParlamento, pais %in% c("Argentina", "Bolivia" , "Brasil","Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "México", "Nicaragua", "Panamá", "Paraguay", "Puerto Rico", "Perú", "República Dominicana", "Uruguay", "Venezuela"))
View(latam)
latinoamerica<- latam%>% group_by(codPais, pais)%>% summarise(totalporcenM=(sum(porcentaje_mujeres,na.rm = TRUE)),totalMujeres=round((totalporcenM*10), 0), totalIntegrantes=(sum(nroIntegrantes, na.rm = TRUE)),totalporcenH=(100- totalporcenM))%>%filter(totalMujeres!=0)%>%arrange(desc(totalporcenM))
View(latinoamerica)


mujeres2 <- ggplot(data =latinoamerica, aes(x=(pais), y=totalMujeres, colour=pais, text = paste('<b>País:</b>', pais,'\n <b> Mujeres:</b>', totalMujeres ))) + 
  geom_bar(size=1.5, stat="identity", position=position_dodge()) +
  scale_colour_manual(values =p1) +  
  scale_fill_manual(values =c(values =p1)) +
  labs (x = "País", y = "Total", 
        title= ("Mujeres en el Parlamento"))+
  #caption = " Fuente: #DatosdeMiercoles", legend=" ")+ # agrego título al gráfico
  theme (axis.text.x =element_blank(),
         #element_text(angle=90, vjust = 1, hjust=1.1, color="black", size=7),
         plot.title = element_text(family="Comic Sans MS",
                                   size=rel(1),                      #Tamaño relativo de la letra del título
                                   vjust=1, 
                                   hjust=0.5,                        #Para separarlo del gráfico
                                   position_identity(center),   
                                   face="bold",      #Tipo: Letra negra, otras posibilidades son "plain", "italic", "bold" y "bold.itali
                                   color="white", #Color del texto  color=maroon, lightblue
                                   lineheight=1.0),legend.text= element_blank(),
         legend.position = "none",
         panel.border = element_blank(),
         panel.background = element_blank(),
         panel.grid = element_blank(),
         rect = element_rect(fill = "pink", color = "pink"),
         text = element_text(family = "Tahoma", colour = "white", size = 14))
mujeres2
ggplotly(mujeres2, hoverformat='2.F', tooltip = "text")
#ggploty(mujeres)













ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')