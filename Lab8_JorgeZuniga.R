# ============================================================================ #
# Titulo: Laboratorio 8 #
# Autor: Jorge Zuñiga #
# Fecha: 06/12/19 #
# Fecha de ultmia modificación: 07/12/19 #
# ============================================================================ #

# Paquetes ####
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggcorrplot)
library( rgl )
library(gganimate)

install.packages("ggcorrplot")
install.packages("rgl")

## **Actividad 1**
> 
  Leemos las bases de datos ques se encuentran alojadas en https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/201
9-05-21

> 
  Base de datos No. 1, BD: *coast_vs_waste*: 
  
  coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

coast_vs_waste %>% 
  sample_n(50, replace=T) %>% #Para tomar una muestra de la base de datos, observar que sale.
  kable() %>% # Dar formato a la tabla
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "blue", underline = TRUE) %>% # Cambiar color, subrayado, etc del nombre de columnas.
  scroll_box(height = "250px") # Para dezplazarse por el scroll (no sea larga)  

>
  Base de datos No. 2, BD: *mismanaged_vs_gdp*: 
  
  mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")

mismanaged_vs_gdp %>% 
  sample_n(50, replace=T) %>% # Seleccionar una muestra de la malla de datos
  kable() %>% # Dar formato a la tabla.
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "red", underline = TRUE) %>% # Cambiar color, subrayado, etc del nombre de columnas.
  scroll_box(height = "250px") # Para dezplazarse por el scroll (no sea larga)  

>
  Base de datos No. 3, BD: *waste_vs_gdp*:
  
  waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")

waste_vs_gdp %>% 
  sample_n(50, replace=T) %>% # Para una muestra de la malla. Observar datos.
  kable() %>% # Dar formato a la tabla.
  kable_styling(bootstrap_options = c("striped",
                                      "hover",
                                      "condensed"),
                full_width = F,
                fixed_thead = T) %>%
  row_spec(0, color = "black") %>% # Cambiar color, subrayado, etc del nombre de columnas.
  scroll_box(height = "250px") # Para dezplazarse por el scroll (no sea larga)  

## **Actividad 2**
>
  Indicaciones:
> 
  Generar 4 graficos de su elección a partir de las mallas de datos descargadas.
>
  Uno de los graficos debera ser animado, usando la libreria gganimate.
>
  De preferencia generar graficos no vistos en clase.

>
  +
  GRAFICO No. 1:
  
  Prueba1 <-
  mismanaged_vs_gdp %>% 
  filter(Entity %in% c("Dominica", "Albania", "Canada"),
         `Total population (Gapminder)`,
         `GDP per capita, PPP (constant 2011 international $) (Rate)`) %>% 
  select(Entity, 
         `Total population (Gapminder)`,
         `GDP per capita, PPP (constant 2011 international $) (Rate)`) %>% 
  print()
  
ggplot() +
  geom_point(mismanaged_vs_gdp, aes(GDP per capita, PPP (constant 2011 international $) (Rate),
                          Total population (Gapminder))) +
  theme_bw()

>
  + 
  GRAFICO No. 2:
  
  grafico1<-
  coast_vs_waste %>%
  filter(Entity %in% c("Austria","Mexico","Bolivia","Colombia","Chile"), Year=="2010") %>%
  select(-`Mismanaged plastic waste (tonnes)`) %>%
  mutate(`Coastal population`=-`Coastal population`) %>% 
  pivot_longer(-c(1:3), 
               names_to = "TipoPob",
               values_to = "CantidadPob") %>%
  #Se modifica "TipoPob" y se agrega niveles a "Entity", para datos ordenados.
  mutate(TipoPob = case_when(TipoPob =="Coastal population" ~ "Poblacion de Costa",
                             TipoPob =="Total population (Gapminder)" ~ "Poblacion Total"),
         Entity = factor(Entity, 
                         levels =c("Austria","Mexico","Bolivia","Colombia","Chile") )) 

#Procede a realizar la grafica de los datos anteriores (malla: grafico1) en ggplot. 
ggplot(data=grafico1,
       aes(x = Entity, 
           y = CantidadPob, 
           fill = TipoPob, 
           group = TipoPob)) +
  geom_bar(data = subset(grafico1,TipoPob == "Poblacion de Costa"),
           stat = "identity") +
  geom_bar(data = subset(grafico1, TipoPob == "Poblacion Total"),
           stat = "identity") +
  coord_flip() +
  labs(x = "pais", 
       y = "poblacion", 
       title = "Pob. Total y de la Costa",
       subtitle = "2010") +
  scale_y_continuous(breaks = seq(-80e+06, 
                                  200e+06, 
                                  by = 40e+06),
                     labels = c(rev(seq(0, 
                                        80e+06, 
                                        by = 40e+06)), 
                                seq(40e+06, 
                                    200e+06, 
                                    by = 40e+06)))+
  theme_bw()+
  theme(legend.position = "bottom")

>
  +
  GRAFICO No. 3:
  
  waste_vs_gdp %>%
  # Se filtra para el año 2010
  filter(Year=="2010") %>%
  select("Residuos Plasticos per capita" = `Per capita plastic waste (kilograms per person per day)`,
         "PIB per capita"=`GDP per capita, PPP (constant 2011 international $) (constant 2011 international $)`,
         "Poblacion Total"=`Total population (Gapminder)`) %>%
  cor(use = "complete.obs") %>% 
  #Obtenemos el correlograma entre las variables
  ggcorrplot(hc.order = TRUE, 
             type = "lower",
             outline.col = "blue",
             ggtheme = theme_classic()) +
  labs(title = "Residuos VS PIB per capita",
       subtitle = "Correlaciones del año 2010")+
  theme(legend.position="right")

>
  +
  GRAFICO No. 4:
  
  mismanaged_vs_gdp %>% 
    filter(Year == "2010",
         `GDP per capita, PPP (constant 2011 international $) (Rate)`<6.5e+04,
         `Per capita mismanaged plastic waste (kilograms per person per day)`< 0.3) %>% 
  # Se filtró la malla de datos con las siguientes caracteristicas: para el año 2010,
  # PIB per capita menor a 65000 USD dollars y un RPMA menor a 0.3
  select(Entity, 
         Year, 
         `Per capita mismanaged plastic waste (kilograms per person per day)`,
         `GDP per capita, PPP (constant 2011 international $) (Rate)`,
         `Total population (Gapminder)`) %>% 
  ggplot(aes(`GDP per capita, PPP (constant 2011 international $) (Rate)`,
             `Per capita mismanaged plastic waste (kilograms per person per day)`))+
  # Se colorea por pais y tamaño de pobalcion.
  geom_point(aes(col= Entity, 
                 size=`Total population (Gapminder)`)) +
  # Obtenemos la linea de tendencia
  geom_smooth(method="lm", 
              se=F) +
  xlim(c(0,7.0e+04)) + 
  ylim(c(0,0.3)) +
  # Con xlim y ylim se pueden modificar las escalas de los ejes.
  labs(x = "PIB per capita", 
       y = "Residuos plasticos mal administrados per capita (kg/dia)", 
       title = "Relacion de residuos plasticos con el PIB per capita",
       subtitle = "2010") +
  theme_bw()+
  theme(legend.position = "none")

## **Actividad 3** 
>
  Elegir el grafico que mejor le parezca y subirlo a twitter con la etiqueta (hashtag) #datosdemiercoles .
>
  + Subir el grafico con un breve comentario y la liga de Github de su codigo.
  + Incluir la captura de pantalla como comprobante.

include_graphics("C:/Users/Usuario/Desktop/Optativa R/Lab 8/Lab 8 JZ/Lab8_CapturaTwitter.jpeg")

## **Actividad 4**
>Incluir los siguientes datos de la cuenta creada en github:
  >
  + Nombre de usuario
  + La liga del repositorio
  + Una captura de pantalla del repositorio

Usuario: [JorgeZB](https://github.com/jorgezuba)
[Liga del repositorio](https://github.com/jorgezuba/Lab-8-Procesamiento-de-datos-y-gr-ficos-.git)



