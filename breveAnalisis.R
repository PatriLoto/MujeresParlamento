
install.packages("easypackages")  #para llamar a varias librerias al mismo tiempo
library(easypackages)

libraries("tidyverse","here","janitor","lubridate","gganimate","gifski","png","LaCroixColoR", "extrafont")  #  ó todo el resto

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
#cálculos
mundo<- datosParlamento%>% group_by(codPais, pais)%>% summarise(totalporcenM=(sum(porcentaje_mujeres,na.rm = TRUE)),totalMujeres=round((totalporcenM*10), 0), totalIntegrantes=(sum(nroIntegrantes, na.rm = TRUE)),totalporcenH=(100- totalporcenM))%>%arrange(totalporcenM)
View(mundo)

#Paleta colores
p<-lacroix_palette("PassionFruit", n = 580, type = "continuous")
p1<-lacroix_palette("Pamplemousse",n=580, type = "continuous")
p2<-lacroix_palette("PeachPear", n = 580, type = "continuous")
p3<-lacroix_palette(type = "paired")
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