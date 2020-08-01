# setwd("~/Documents/GitHub/ensanut_mun/shiny_app")

#### Global R frame 

pacman::p_load(tidyverse,
               ggplot2,
               shiny,
               gmodels,
               tmap,
               leaflet,
               foreign,
               expss,
               fishualize,
               viridis,
               raster,
               cowplot,
               sf,
               ggspatial,
               colorspace)
theme_set(theme_bw())

#Shape Nacional
# nac <- st_read("data/nacional.shp")

#Shape Municipal
mun_nac <- st_read("data/municipal.shp")
mun_nac <- mun_nac %>%
    dplyr::select(CVEGEO,NOM_ENT,NOM_MUN)
glimpse(mun_nac)

# Datos de ensanut a nivel municipal
ensanut_ap <- read.csv(file = "data/ensanut_areas_peq.csv", 
                       sep=",", 
                       colClasses=c(rep('factor', 6), 'numeric','numeric','numeric')
)
ensanut_ap <- ensanut_ap %>%
    rename(CVEGEO = mun,Obesidad=obesidad,Hipertensión=hipertension,Diabetes=diabetes)

#Unir ambas bases
datitos<-merge(x=mun_nac,y=ensanut_ap,by=c("CVEGEO"))

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

#### End Global R frame 


### Frontpage
ui <- fluidPage( theme = "bootswatch-cerulean.css",
    titlePanel("Mapas Encuesta Nacional de Salud y Nutrición 2018"),
    h3("Principales enfermedades a nivel municipal."),
    sidebarLayout(
        sidebarPanel(
            helpText("Seleccione la variable y el número de clases para mostrar"),
            
            selectInput("var", 
                        label = "Enfermedad",
                        choices = c("Diabetes", 
                                    "Hipertensión",
                                    "Obesidad"),
                        selected = "Hipertensión"),
            
            sliderInput(inputId = "class", 
                        label = "Número de clases", 
                        value = 5, min = 2, max = 10),
            helpText("Fuente: Elaborado por @rojo_neon, con datos de INEGI/ENSANUT a nivel muncipal."),
            p("Para reproducir, visite",
              a(href="https://github.com/rojoneon/ensanut_mun", "github.com/rojoneon/ensanut_mun")
        )
        ),
        
        mainPanel(
            h3(textOutput("selected_var")),
            h4(textOutput("selected_range")),
            plotOutput("map")
        )
    ) #End Sidebar Layout
    
) #End FluidPAge
### End Frontpage


### Backend
server <- function(input, output){
    output$selected_var <- renderText({ 
        paste("Prevalencia de ", input$var, "en México")
    })
    output$selected_range <- renderText({ 
        paste("Rangos calculados para ", input$class , " clases")
    })
    output$map <- renderPlot({
        

        # ¿Cuántas clases quiero?
        no_classes <- input$class
        variable <- input$var
        
        # Extraer cuantiles
        cuantil <- datitos %>%
            pull(variable) %>%
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
        
        # Crear la variable en datitos
        
        q_var = datitos %>%
            pull(variable)
        
        datitos <- datitos %>%
            mutate(q_variable = cut(q_var,
                                    breaks = cuantil,
                                    labels = labels,
                                    include.lowest = T))
        # Creamos el mapa
        
        ggplot(data = datitos) +
            # Agrego la capa principal
            geom_sf(
                mapping = aes(
                    fill = q_variable
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
                 title = "Prevalencia en México",
                 subtitle = "% de población de 20 años y más con diagnóstico previo",
                 caption = "Fuente: Elaborado por @rojo_neon, con datos de INEGI/ENSANUT a nivel muncipal.",
                 fill = "%") +
            # Hacer un pequeño zoom
            #coord_sf(xlim = c(-99.35, -98.9), ylim = c(19.17,19.6), expand = FALSE) +
            # Finalmente, agregar theme
            theme_map()
        
        # Guardar el mapa
        # ggsave("Hipertensión_Nac.png", width = 7)
        
        
        
    }) # End render plot map
    
} ### End Server function

### End Backend

# runApp("~/Desktop/Code/R Pro/Shiny/test_maps/census-app",display.mode = "showcase")

shinyApp(ui = ui, server = server)




