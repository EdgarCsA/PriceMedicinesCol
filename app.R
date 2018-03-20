library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)
library(RSocrata)
library(googleVis)

token<-  "##############################"   #Ingrese el token otorgado por DatosaAbiertos.gov.co
df<-read.socrata("https://www.datos.gov.co/resource/agrt-fatq.json", app_token=token)

ui <- miniPage(
  
  miniTitleBar(title=NULL, 
               left= selectizeInput('in6', label = NULL, choices=df$nombre_comercial, options = list(
          placeholder = '¿Nombre comercial del medicamento?',
          onInitialize = I('function() { this.setValue(""); }')))),
  tags$head(tags$style(HTML(".selectize-input {margin-top: 6px; margin-left: 7px;}"))),
  
  miniTabstripPanel(
        miniTabPanel("Lista", icon = icon("table"),
                 miniContentPanel(
                      tableOutput("table"),
                      htmlOutput("text1")

                 )
    ),
        miniTabPanel("Gráfico", icon = icon("area-chart"),
                 miniContentPanel(
                   p('El gráfico muestra la asignación de cada medicamento a un rango de precios "Alto", "Medio" y "Bajo"'),
                   htmlOutput("view")
                 )
    ),
    selected = "Lista"
  )
)

server <- function(input, output, session) {



output$table <- renderTable({ 
   datoss<- input$in6

    if(datoss==""){
        return(NULL)

      } else{       
        dfss<-subset(df, nombre_comercial==input$in6 | principio_activo==input$in6)
        filtro<- unique(dfss$principio_activo, incomparables = FALSE)
        dfss2<-subset(df, principio_activo %in% filtro)
        NomFabr=c(paste(dfss2$nombre_comercial,"_",dfss2$fabricante))
        precio=c(as.numeric(dfss2$precio_por_tableta))
        Tabl= data.frame("Nombre_Laboratorio"=NomFabr, Precio=precio) 
      }
  })

 output$text1 <- renderUI({
   datoss<- input$in6

    if(datoss==""){
        str1 <- "Esta aplicación toma los datos cargados por MinSalud en el sitio de datos abiertos www.datos.gov.co"
        str2 <- "Desarrollado por IngText SAS"
        str3 <- h3('Escribe el nombre del medicamento para mostrarte alternativas de este y la relación de los precios.')
        HTML(paste("<div style='text-align: center'>",str3,"<br>",str1,"<br>",str2,"</div>"))


      } else{  
        str1 <- ""
        str2 <- "Desarrollado por IngText SAS"
        str3 <- ""
        HTML(paste("<div style='text-align: center'>",str3,"<br>",str1,"<br>",str2,"</div>"))
      }
  })

output$view <- renderGvis({
    datoss<- input$in6

    if(datoss==""){
        return(NULL)

      } else{  
    dfss<-subset(df, nombre_comercial==input$in6 | principio_activo==input$in6)
    filtro<- unique(dfss$principio_activo, incomparables = FALSE)
    dfss2<-subset(df, principio_activo %in% filtro)
    NomFabr=c(paste(dfss2$nombre_comercial,"_",dfss2$fabricante))
    precio=c(as.numeric(dfss2$precio_por_tableta))
    too=c(dfss2$factoresprecio)
    tm=nrow(dfss2)*30
    datSK <- data.frame(From=NomFabr,To=too,Weight=precio)
    gvisSankey(datSK, from="From", to="To", weight="Weight", options=list(width='100%', height=tm))
  }
  })


  observeEvent(input$done, {
    stopApp(TRUE)
  })
}

shinyApp(ui, server)


#Filtrar la BD
#dfss<-subset(df, nombre_comercial=="Acetaminofen")
#dfss<-subset(df, nombre_comercial=="Acetaminofen"| principio_activo=="Acetaminofen")


#Eliminar datos dusplicados en "principio Activo"
#unique(dfss$principio_activo, incomparables = FALSE) 