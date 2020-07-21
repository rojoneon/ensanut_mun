######################################################################
#Mapas de diabetes, hipertensión y obesidad con datos de INEGI-ENSANUT a nivel municipal (estim. áreas peq.)
#Elaborado por: Máximo Ernesto Jaramillo-Molina 
#               Twitter: @rojo_neon
#               Github: @rojoneon

#Descarga de datos INEGI: https://www.inegi.org.mx/investigacion/pohd/2018/default.html#Tabulados
######################################################################

##############
#Configuración
rm(list = ls())
library(pacman)
# Abrimos las paqueterías con un sólo comando:
p_load(haven, readr, readxl, ggplot2, shiny, tidyverse, knitr,gmodels,foreign,expss,fishualize)

##############
#Paqueterías para mapear
library(sf)
#install.packages("ggspatial")
library("ggspatial")
theme_set(theme_bw())
library("colorspace")
#hcl_palettes(plot = TRUE)
p_load(rstudioapi, lintr, raster, viridis, cowplot, rmarkdown)
sessionInfo()

##############
#Directorio de trabajo
setwd ("~/Documents/Data Science/Repos/rojoneon/2020B/ensanut_mun")
##############

##############
#Pegado de datos de presidencias municipales
##############
#Shape Nacional
nac <- st_read("nacional.shp")

#Shape Municipal
mun_nac <- st_read("municipal.shp")

mun_nac <- mun_nac %>%
  dplyr::select(CVEGEO,NOM_ENT,NOM_MUN)
glimpse(mun_nac)
head(mun_nac)

#Datos de ensanut a nivel municipal
ensanut_ap <- read.csv(file = "ensanut_areas_peq.csv", 
                       sep=",", 
                       colClasses=c(rep('factor', 6), 'numeric','numeric','numeric')
                       )
glimpse(ensanut_ap)

ensanut_ap <- ensanut_ap %>%
  rename(CVEGEO = mun)

#Unir ambas bases
datitos<-merge(x=mun_nac,      y=ensanut_ap,by=c("CVEGEO"))

#write.csv(datitos, file = "datitos.csv")



##########################
####Mapas
##########################


#Función para determinar theme de las gráficas
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Verdana",
                          color = "#939486"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#F5F5F3", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "#F5F5F3",
                                     color = NA),
      panel.background = element_rect(fill = "#F5F5F3",
                                      color = NA),
      legend.background = element_rect(fill = "#F5F5F3",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9,hjust = 1,
                                 color = "#939486"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "#4B4C47"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "#939486",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}


#####################
##Mapas con cuantiles de diabates
#####################

# ¿Cuántas clases quiero?
no_classes <- 5
# Extraer cuantiles
cuantil <- datitos %>%
  pull(diabetes) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- imap_chr(cuantil, function(., idx){
  return(paste0(round(cuantil[idx] , 0),
                "%",
                " – ",
                round(cuantil[idx + 1] , 0),
                "%"))
})

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
datitos <- datitos %>%
  mutate(q_diabetes = cut(diabetes,
                         breaks = cuantil,
                         labels = labels,
                         include.lowest = T))



##Va el mapa de diabetes
#####################
ggplot(data = datitos) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q_diabetes
    ),
    color = "white",
    size = 0
  ) +

  # Viridis color scale
  scale_fill_viridis(
    option = "plasma",
    name = " ",
    alpha = 0.8, 
    begin = 0.6, 
    end = 1,
    discrete = T, # discrete classes
    direction = -1, # oscuro es el más alto, claro/amarillo el más bajo
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T # El valor más alto hasta arriba
    )) +

  # # Utilizar un borde más grueso para los municipios
  # geom_sf(
  #   data = datitos,
  #   fill = "transparent",
  #   color = "white",
  #   size = 0.5) +

  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Prevalencia de diabetes en México",
       subtitle = "% de población de 20 años y más con diagnóstico previo de diabetes",
       caption = "Fuente: Elaborado por @rojo_neon, con datos de INEGI/ENSANUT a nivel muncipal.",
       fill = "%") +
  # Hacer un pequeño zoom
  #coord_sf(xlim = c(-99.35, -98.9), ylim = c(19.17,19.6), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("Diabetes_Nac.png", width = 7)









#####################
##Mapas con cuantiles de hipertension
#####################

# ¿Cuántas clases quiero?
no_classes <- 5
# Extraer cuantiles
cuantil <- datitos %>%
  pull(hipertension) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- imap_chr(cuantil, function(., idx){
  return(paste0(round(cuantil[idx] , 0),
                "%",
                " – ",
                round(cuantil[idx + 1] , 0),
                "%"))
})

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
datitos <- datitos %>%
  mutate(q_hipertension = cut(hipertension,
                          breaks = cuantil,
                          labels = labels,
                          include.lowest = T))



##Va el mapa de hipertension
#####################
ggplot(data = datitos) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q_hipertension
    ),
    color = "white",
    size = 0
  ) +
  
  # Viridis color scale
  scale_fill_viridis(
    option = "plasma",
    name = " ",
    alpha = 0.8, 
    begin = 0.6, 
    end = 1,
    discrete = T, # discrete classes
    direction = -1, # oscuro es el más alto, claro/amarillo el más bajo
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T # El valor más alto hasta arriba
    )) +
  
  # # Utilizar un borde más grueso para los municipios
  # geom_sf(
  #   data = datitos,
  #   fill = "transparent",
  #   color = "white",
  #   size = 0.5) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Prevalencia de hipertensión en México",
       subtitle = "% de población de 20 años y más con diagnóstico previo de diabetes",
       caption = "Fuente: Elaborado por @rojo_neon, con datos de INEGI/ENSANUT a nivel muncipal.",
       fill = "%") +
  # Hacer un pequeño zoom
  #coord_sf(xlim = c(-99.35, -98.9), ylim = c(19.17,19.6), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("Hipertension_Nac.png", width = 7)






#####################
##Mapas con cuantiles de obesidad
#####################

# ¿Cuántas clases quiero?
no_classes <- 5
# Extraer cuantiles
cuantil <- datitos %>%
  pull(obesidad) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1)) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric
# Así se crean las etiquetas
labels <- imap_chr(cuantil, function(., idx){
  return(paste0(round(cuantil[idx] , 0),
                "%",
                " – ",
                round(cuantil[idx + 1] , 0),
                "%"))
})

# Se elimina la última etiqueta
# En caso contrario sería hasta NA
labels <- labels[1:length(labels) - 1]
labels
# Crear la variable
datitos <- datitos %>%
  mutate(q_obesidad = cut(obesidad,
                              breaks = cuantil,
                              labels = labels,
                              include.lowest = T))



##Va el mapa de obesidad
#####################
ggplot(data = datitos) +
  # Agrego la capa principal
  geom_sf(
    mapping = aes(
      fill = q_obesidad
    ),
    color = "white",
    size = 0
  ) +
  
  # Viridis color scale
  scale_fill_viridis(
    option = "plasma",
    name = " ",
    alpha = 0.8, 
    begin = 0.6, 
    end = 1,
    discrete = T, # discrete classes
    direction = -1, # oscuro es el más alto, claro/amarillo el más bajo
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T # El valor más alto hasta arriba
    )) +
  
  # # Utilizar un borde más grueso para los municipios
  # geom_sf(
  #   data = datitos,
  #   fill = "transparent",
  #   color = "white",
  #   size = 0.5) +
  
  # Agregar títulos
  labs(x = NULL,
       y = NULL,
       title = "Prevalencia de obesidad en México",
       subtitle = "% de población de 20 años y más con diagnóstico previo de diabetes",
       caption = "Fuente: Elaborado por @rojo_neon, con datos de INEGI/ENSANUT a nivel muncipal.",
       fill = "%") +
  # Hacer un pequeño zoom
  #coord_sf(xlim = c(-99.35, -98.9), ylim = c(19.17,19.6), expand = FALSE) +
  # Finalmente, agregar theme
  theme_map()

#Guardar el mapa
ggsave("Obesidad_Nac.png", width = 7)

