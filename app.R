library(shiny)
library(shinydashboard)
library(dygraphs)
library(tidyverse)
library(readr)
library(rvest)
library(leaflet)
library(RCurl)
library(stringr)

#### data 

source("global.R", local=TRUE)
new <- dat_update()
record <- new$records_share
venta_mundial <- new$venta_mundial
venta_pais <- new$ventas_pais
venta_medios <- new$venta_medios
billboard <- new$billboard

## ui.r
header <- dashboardHeader(title = tags$a(href="https://campusvirtual.uninorte.edu.py/login/index.php",
                          tags$img(src="uninorte.png", width="50%")))

side <- dashboardSidebar(
  br(),
  div(img(src="musicos.png", height="80%", width="80%"), align = "center"),
  h3("Industria Musical", align = "center"), br(),

  sliderInput("año", "Elija el año", 1991, 2018, 1,sep = ""),
  br(), br(),
  div(img(src="los_del_fondo.png", height="70%", width="70%"), align = "center"),
  br(),br(),br(), br(), br(),br(), br(), br(),br(),
  div(img(src="foto_gus.png", height="30%", width="30%"), align = "center"),br(),
  div(a(href= "https://www.linkedin.com/in/gustavo-mayeregger-050b64186/",target="_blank",icon("linkedin-square", 
  "fa-2x"), "Gustavo Mayeregger"), align = "center")
)
body <- dashboardBody(
  includeCSS("custom.css"),
  fluidRow(
    column(6,
      box(width = 500, title = "Billbord # 1",
        tableOutput("tab")
      )    

#      img(src="notasmusicales.png", width="30%")
    ),
    column(6,
      infoBoxOutput("info1", width = 500)
    )
  ),

  box(width = 12,

    background = "aqua",
    fluidRow(
      column(width = 6,
          plotOutput("plot_global", height = 280),
      ),
      column(width = 6,
        leafletOutput("map", height = 280)
      )
    ),
    fluidRow(
      column(width = 6,
             plotOutput("plot_record", height = 280),
      ),
      column(width = 6,
             plotOutput("plot_pais", height = 280)
      )
    )
  )
)
ui <- dashboardPage(header, side, body) 

## server.r
server <- function(input, output){

  output$tab <- renderTable(spacing = "xs", colnames = FALSE,  {

    bill <- billboard %>% filter(Año == input$año)
    
    tab <- data.frame(Categoría = c("TEMA POP", "ALBUM POP", "TEMA R&B", "ALBUM R&B"),
                      Premiado = c(bill$Pop_single, bill$Pop_album, bill$RandB_single, bill$RandB_album))

    tab

  })
  
  
  
  
  output$info1 <- renderInfoBox({
    infoBox(
      title = h3(input$año),
      icon =
        if(input$año <= 1994){icon("compact-disc")} 
        else if(input$año > 1994 & input$año <= 1999){icon("compact-disc")}
        else if(input$año > 1999 & input$año <= 2002){icon("napster")}
        else if(input$año > 2002 & input$año <= 2010){icon("itunes")}
        else if(input$año > 2010 & input$año <= 2018){icon("spotify")},
      subtitle =
        if(input$año == 1991){"La Sony cambia su nombre a Sony Music."}      
        else if(input$año == 1992){"Las 8 mayores cadenas de Estados Unidos representaban el 57% de las ventas. Era tanta la cantidad de productos ofrecidos que a las tiendas minoristas ya les faltaba espacio."} 
        else if(input$año == 1993){"BMG producía en sus instalaciones de Argentina, Brasil, Alemania Hong Kong, Irlanda, Méjico, Sudáfrica, España y Estados Unidos unos 2,5 millones de CDs al día"}      
        else if(input$año == 1994){"La Billboard considera este año como el mejor año para la música de todos los tiempos por la gran cantidad de buenos materiales en todos los géneros y el nacimiento de nuevos géneros influenciados por el éxito de Nirvana entre otros."}      
        else if(input$año == 1995){"Universal es adquirida por Seagram | El formato mp3 toma fuerza facilitando compartir músicas en un formato mucho más comprimido sacrificando algo de su calidad"}      
        else if(input$año == 1996){"Este año queda en la historia por ser el año de mayor ventas a nivel mundial, llegando a los casi 40 billones de dólares"}
        else if(input$año == 1997){"Estados Unidos representaba el 34% de todas las ventas mundiales frente a los 33% de toda la Europa. | El Rock representaba el 33% de toda las ventas"}
        else if(input$año == 1998){"Seagram (Universal) compra a Polygram aumentando su liderazgo | Se crea la SDMI para fijar normas en la distribución digital"}
        else if(input$año == 1999){"CDNow ostentaba una colección de 390.000 CDs. | Forrester Research estimó unas 3 millones de descargas diarias en mp3 | MP3.com poseía 180.000 canciones de 31.000 artistas | EMusic.com compra derechos de 140 discográficas independientes"}
        else if(input$año == 2000){"Napster adquiere popularidad con 20 millones de usuarios que descargaban los mp3 a sus ordenadores sin efectuarse ningún pago por derechos de autor | Otro fuerte impacto a la industria musical es la creciente popularidad de los grabadores de CD-R presentes en todos los ordenadores."}
        else if(input$año == 2001){"Apple lanza el innovador ipod. | Ante la presión por parte de la industria por las descargas ilegales se toman medidas judiciales y el sitio Napster es clausurado llegando a tener 26.4 millones de suscriptores."}
        else if(input$año == 2002){"Universal demanda a MP3.com por violaciones de derechos y debe pagar una multa de 200 millones de dólares quedando así fuera del negocio luego de tener 25 millones de suscriptores."}
        else if(input$año == 2003){"Apple abre la tienda iTunes, donde se podía fácilmente descargar temas a 0.99$ cada uno. | Algunas compañías registraban por primera vez que las ventas digitales sobrepasaban a las físicas."}
        else if(input$año == 2004){"Sony adquiere BMG | Apple lanza el innovador ipod que junto a iTunes popularizan la descarga musical pagada. | La MTV compra al mayor canal musical de alemania convirtiéndose en la mayor compañia musical en europa."}
        else if(input$año == 2005){"La Federación Internacional de la Industria Fonográfica present su informe donde recalca que 1 de cada 3 discos vendidos es pirata, facturando ese año la industria pirata unos 4600 millones de dólares y afirmó que la piratería financia al crimen organizado."}
        else if(input$año == 2006){"Se Funda WIN (Worldwide Independent Network) que es una coalición de los sellos independientes de todo el mundo."}
        else if(input$año == 2007){"Ante la caída de las ventas se vuelven más comunes los contratos 360 entre las discográficas y los artistas. Mediante éstos contratos las discográficas adquieren el derecho sobre todas las actividades del artista (conciertos, publicidades, etc) y no solo por la venta de sus discos."}
        else if(input$año == 2008){"Spotify es lanzado en Europa | La encuesta británica sobre los derechos musicales arroja que el 80% de los encuestados querían un servicio legal de descargas pero solo el 50% consideraba que se tenía que pagar derechos autorales"}
        else if(input$año == 2009){"A medida que la gente deja de interesarse por los discos su interés por asistir a los conciertos va en aumento y ya se espera que en el 2010 lo recaudado por los shows recaude más que las ventas por discos."}
        else if(input$año == 2010){"Deezer, Pandora, Spotify y Apple's iTunes Radio comienzan a cobrar suscripciones a servicios de streaming donde el usuario paga una tarifa anual por escuchar la música sin descargarla a diferencia de los sitios de descarga donde el usuario pagaba por cada tema descargado"}
        else if(input$año == 2011){"Universal compra todos los derechos musicales de EMI mientras que Sony adquiere los servicios publicitarios de EMI"}
        else if(input$año == 2012){"Pese a la caida del 12% de los medios físicos las ventas de los vinilos registraban un aumento del 20%"}
        else if(input$año == 2013){"Aparece la Crisis del 2013 en la industria musical debido principalmente al aumento de los servivios de streaming (menor margen para las compañías) lo que ocacionó una disminución en los otros medios de distribución (con mayores márgenes para las compañias)"}
        else if(input$año == 2014){"Spotify y demás medios de streaming son criticados por los artistas por injustas compensaciones. Spotify paga por derechos unos 70% de sus ingresos a las compañías y éstas a su vez a los artistas."}
        else if(input$año == 2015){"Debuta el Apple Music, el servicio de streaming de Apple"}
        else if(input$año == 2016){"Los ingresos por Streaming aumentan 60% en el primer semestre totalizando 1.6 billones de dólares, muy lejos de los 14.6 billones generado por la venta de CDs en 1999"}
        else if(input$año == 2017){"Spotify comienza a cotizar en bolsa | Unos 176 millones de usuarios pagaron suscripciones por algún servicio de streaming generando suficiente ingresos para dar oxígeno a la industria musical. | Unos 20$ es el promedio pagado por una suscripción anual."}
        else if(input$año == 2018){"América Latina es la región con mayor crecimiento en los servicios de streaming, aumentando un 16.8% con relación al 2017."},
      color = "black",
      fill = TRUE
    )
  })
  
  output$plot_global <- renderPlot({
    puntos <- data.frame(x = rep(input$año, 5), y = venta_mundial$Ventas[which(venta_mundial$Año==input$año)])
    venta_mundial %>% ggplot() + geom_line(aes(Año, Ventas, color=Medio), size = 1) + 
    geom_vline(xintercept = input$año, color = "red", linetype = "dashed", size = 1) +
#    geom_point(data = puntos, aes(x, y), size = 3) +
    labs(x = "", y = "billones de dólares", title = "Ventas por medio de reproducción")
  })
      
  output$plot_pais <- renderPlot({  
    venta_pais %>% group_by(Pais) %>% filter(sum(n())>=5) %>% 
      ggplot(aes(Año, Ventas, color=Pais)) + geom_smooth(se=FALSE) +
      geom_vline(xintercept = ifelse(input$año<2005, 2005, input$año), 
                 color = "red", linetype = "dashed", size = 1) +
      labs(x = "", y = "millones de dólares", title = "Ventas por países ")
  })
  
  output$plot_record <- renderPlot({  
      record %>% ggplot(aes(Año, Mercado, color=Compañía)) + geom_smooth(se=FALSE) +
        geom_vline(xintercept = input$año, color = "red", linetype = "dashed", size = 1) +
        labs(x = "", y = "porcentaje", title = "Cuota de mercado por compañías")
  })
  
#2005 2010 2014 2018
  
  output$map <- renderLeaflet({

    año_new <- case_when(input$año <= 2005 ~ 2005,
                        input$año > 2005 & input$año <= 2010 ~ 2010,
                        input$año > 2010 & input$año <= 2014 ~ 2014,
                        input$año > 2014 ~ 2018)
    dat <- venta_pais %>% filter(Año == año_new)
    leaflet() %>%
      addTiles() %>%  
      addCircleMarkers(dat$longitude, dat$latitude, radius = dat$Ventas/300, 
                       label = paste(dat$Pais, " - ", dat$Ventas, "M", sep=""), weight = 1)
  })

  
  
  
}

shinyApp(ui, server)