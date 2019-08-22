library(RSQLite)
library(repmis)
library(lubridate)
library(dplyr)
library(magrittr)
library(biscale)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(shadowtext)
library(leaflet)
library(htmltools)
library(sf)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(tricolore)
library(ggtern)
library(shinyjs)

db <- dbConnect(SQLite(), dbname='demo_phenom_app.sqlite')
load("spatial_data_all.RData")


shinyApp(
  ui = dashboardPage(
    
    dashboardHeader(disable = T),
    dashboardSidebar(disable = T),
    dashboardBody(tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }",
                             ".shiny-output-error:after { visibility: hidden; }"),
                  
                  tags$style(".btn {background-color: #FE1F62; border-color: #FE1F6200;}",
                             ".btn:hover {background-color: #FE1F62; border-color: #FE1F6200;}"),
                  
                  
                  fluidRow(
                    column(
                      width = 12,
                      align = "justify",
                      
                      ## MAPS -----
                      
                      widgetUserBox(
                        title = "Demographic phenomena in Spain: multivariate mapping",
                        subtitle = "Combine up to 3 variables at provintial and autonomous level in one map",
                        width = 12,
                        color = "maroon",
                        src = "https://github.com/ordanovich/images/blob/master/logo_ine.png?raw=true",
                        closable = FALSE,
                        collapsible=FALSE,
                        
                        
                        fluidRow(column(
                          width = 12,
                          align = "center",
                          radioButtons('layout', '', 
                                       choices=c('Choropleth map', 'Bivariate map', "Ternary composition"),
                                       inline=T))),
                        
                        
                        
                        fluidRow(
                          column(width = 8,
                                 
                                 uiOutput("plots")
                                 
                          ),
                          
                          column(width = 4,
                                 flipBox(
                                   id = 1,
                                   main_img = "https://image.flaticon.com/icons/svg/1046/1046458.svg",
                                   header_img = "https://ak4.picdn.net/shutterstock/videos/5453114/thumb/11.jpg",
                                   front_btn_text = "More on method",
                                   back_btn_text = "Back to legend",
                                   front_title = "",
                                   back_title = "",
                                   uiOutput("legends"),
                                   
                                   back_content = tagList(
                                     column(
                                       width = 12,
                                       align = "justify",
                                       uiOutput("methods"),
                                       br(),
                                       br(),
                                       br(),
                                       uiOutput("github"),
                                       uiOutput("github_titles", align = "center")
                                     )
                                   )
                                   
                                 )
                          )
                          
                        ),
                        fluidRow(column(
                          width = 12,
                          align = "center",
                          
                          radioButtons('level', '', 
                                       choices=c("Provincia", "Comunidad"),
                                       inline=T))
                        )
                        
                        ,fluidRow(column
                                  (width = 12,
                                    sliderInput(
                                      
                                      inputId = "all_years",
                                      label = "",
                                      min = 1975,
                                      max = 2018,
                                      value = 2000,
                                      sep = "",
                                      step = 1
                                      
                                    )))
                      )
                    )
                    
                    ## ------
                    
                  ),
                  
                  ## VARIABLE SELECTION -----
                  
                  
                  
                  fluidRow(
                    
                    ## Variable 1 ------
                    
                    
                    
                    
                    column(width = 4,align = "justify",
                           widgetUserBox(
                             title = "Variable 1", 
                             subtitle = "Select to create a univariate map",
                             closable = F, 
                             collapsed = F,
                             width = 12,
                             
                             src = "https://image.flaticon.com/icons/svg/1046/1046479.svg",
                             color = "teal",
                             solidHeader = FALSE, 
                             collapsible = TRUE,
                             
                             htmlOutput("ind_prov"),
                             htmlOutput("tabla_prov"),
                             htmlOutput("unidad_prov"),
                             htmlOutput("sexo_prov"),
                             htmlOutput("edad_prov"),
                             htmlOutput("nacionalidad_prov"),
                             htmlOutput("durmat_prov"),
                             htmlOutput("nacord_prov")
                           )
                    ),
                    
                    ## Variable 2 -----
                    
                    column(width = 4, align = "justify",
                           widgetUserBox(
                             title = "Variable 2", 
                             subtitle = "Combine with Variable 1 for bivariate mapping",
                             closable = FALSE, 
                             collapsed = TRUE,
                             width = 12,
                             
                             src = "https://image.flaticon.com/icons/svg/1046/1046456.svg",
                             color = "teal",
                             solidHeader = FALSE, 
                             collapsible = TRUE,
                             
                             htmlOutput("ind_prov2"),
                             htmlOutput("tabla_prov2"),
                             htmlOutput("unidad_prov2"),
                             htmlOutput("sexo_prov2"),
                             htmlOutput("edad_prov2"),
                             htmlOutput("nacionalidad_prov2"),
                             htmlOutput("durmat_prov2"),
                             htmlOutput("nacord_prov2")
                           )),
                    
                    
                    ## Variable 3 -----
                    
                    
                    column(width = 4, align = "justify",
                           widgetUserBox(
                             title = "Variable 3", 
                             subtitle = "Use with Variable 1 and Variable 2 to create a ternary composition map",
                             closable = FALSE, 
                             collapsed = TRUE,
                             width = 12,
                             
                             src = "https://image.flaticon.com/icons/svg/1046/1046459.svg",
                             color = "teal",
                             solidHeader = TRUE, 
                             collapsible = TRUE,
                             
                             htmlOutput("ind_prov3"),
                             htmlOutput("tabla_prov3"),
                             htmlOutput("unidad_prov3"),
                             htmlOutput("sexo_prov3"),
                             htmlOutput("edad_prov3"),
                             htmlOutput("nacionalidad_prov3"),
                             htmlOutput("durmat_prov3"),
                             htmlOutput("nacord_prov3")
                           )
                           
                    )
                    
                    ## ------
                  )
    )
    
    
    
    
  ),
  
  
  
  
  
  
  server = function(input, output) {
    
    storeWarn<- getOption("warn")
    options(warn = -1) 
    
    ss_subset <- reactive({
      
      dbGetQuery(db, paste0("SELECT * FROM 'demo_phenom_data' WHERE `periodo` = '",
                            as.numeric(as_datetime(paste0(input$all_years, "-01-02"), 
                                                   tz = "Europe/Madrid")), "'")) %>%
        mutate(periodo = lubridate::as_datetime(periodo, origin = lubridate::origin, tz = "Europe/Madrid"))
   
       })
    
    ## VARIABLE 1 ------
    
    dd0 <- reactive({ss_subset() %>% filter(nivel == input$level)})
    
    output$ind_prov <- renderUI(
      
      selectInput(inputId="ind_prov",
                  label = "Indicador",
                  choices = sort(unique(dd0()$indicador)),
                  selected = sort(unique(dd0()$indicador))[1]
      )
    )
    
    dd1 <- reactive({ss_subset() %>% filter(nivel == input$level & indicador == input$ind_prov)})
    
    output$tabla_prov <- renderUI(
      
      selectInput(inputId="tabla_prov",
                  label = "Variable",
                  choices = sort(unique(dd1()$tabla)
                                 
                  )
      )
    )
    
    dd2 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                              indicador == input$ind_prov & 
                                              tabla == input$tabla_prov)})
    
    output$unidad_prov <- renderUI(
      
      selectInput(inputId="unidad_prov",
                  label = "Unidad",
                  choices = sort(unique(dd2()$unidad)
                                 
                  )
      )
    )
    
    
    dd4 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                              indicador == input$ind_prov & 
                                              tabla == input$tabla_prov &
                                              unidad == input$unidad_prov &
                                              as.integer(lubridate::year(periodo)) == input$all_years)})
    
    output$sexo_prov <- renderUI(
      
      selectInput(inputId="sexo_prov",
                  label = "Sexo",
                  choices = sort(unique(dd4()$sexo)
                                 
                  )
      )
    )
    
    dd5 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                              indicador == input$ind_prov & 
                                              tabla == input$tabla_prov &
                                              unidad == input$unidad_prov &
                                              as.integer(lubridate::year(periodo)) == input$all_years &
                                              sexo == input$sexo_prov)})
    
    output$edad_prov <- renderUI(
      
      selectInput(inputId="edad_prov",
                  label = "Edad",
                  choices = sort(unique(dd5()$edad)
                                 
                  )
      )
    )
    
    dd6 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                              indicador == input$ind_prov & 
                                              tabla == input$tabla_prov &
                                              unidad == input$unidad_prov &
                                              as.integer(lubridate::year(periodo)) == input$all_years &
                                              sexo == input$sexo_prov &
                                              edad == input$edad_prov)})
    
    
    output$nacionalidad_prov <- renderUI(
      
      selectInput(inputId="nacionalidad_prov",
                  label = "Nacionalidad",
                  choices = sort(unique(dd6()$nacionalidad)
                                 
                  )
      )
    )
    
    dd7 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                              indicador == input$ind_prov & 
                                              tabla == input$tabla_prov &
                                              unidad == input$unidad_prov &
                                              as.integer(lubridate::year(periodo)) == input$all_years &
                                              sexo == input$sexo_prov &
                                              edad == input$edad_prov &
                                              nacionalidad == input$nacionalidad_prov)})
    
    output$durmat_prov <- renderUI(
      
      selectInput(inputId="durmat_prov",
                  label = "Duración de matrimonio",
                  choices = sort(unique(dd7()$duracion_matrimonio)
                                 
                  )
      )
    )
    
    dd8 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                              indicador == input$ind_prov & 
                                              tabla == input$tabla_prov &
                                              unidad == input$unidad_prov &
                                              as.integer(lubridate::year(periodo)) == input$all_years &
                                              sexo == input$sexo_prov &
                                              edad == input$edad_prov &
                                              nacionalidad == input$nacionalidad_prov &
                                              duracion_matrimonio == input$durmat_prov)
    })
    
    output$nacord_prov <- renderUI(
      
      selectInput(inputId="nacord_prov",
                  label = "Orden de nacimiento",
                  choices = sort(unique(dd8()$nacimiento_orden))
                  
      )
    )
    
    
    
    ## PROV VARIABLE 2 ------
    
    
    output$ind_prov2 <- renderUI(
      
      selectInput(inputId="ind_prov2",
                  label = "Indicador",
                  choices = sort(unique(dd0()$indicador)),
                  selected = sort(unique(dd0()$indicador))[2]
      )
    )
    
    dd9 <- reactive({ss_subset() %>% filter(nivel == input$level & indicador == input$ind_prov2)})
    
    output$tabla_prov2 <- renderUI(
      
      selectInput(inputId="tabla_prov2",
                  label = "Variable",
                  choices = sort(unique(dd9()$tabla)
                                 
                  )
      )
    )
    
    dd10 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov2 & 
                                               tabla == input$tabla_prov2)})
    
    output$unidad_prov2 <- renderUI(
      
      selectInput(inputId="unidad_prov2",
                  label = "Unidad",
                  choices = sort(unique(dd10()$unidad)
                                 
                  )
      )
    )
    
    
    dd12 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov2 & 
                                               tabla == input$tabla_prov2 &
                                               unidad == input$unidad_prov2 &
                                               as.integer(lubridate::year(periodo)) == input$all_years)})
    
    output$sexo_prov2 <- renderUI(
      
      selectInput(inputId="sexo_prov2",
                  label = "Sexo",
                  choices = sort(unique(dd12()$sexo)
                                 
                  )
      )
    )
    
    dd13 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov2 & 
                                               tabla == input$tabla_prov2 &
                                               unidad == input$unidad_prov2 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov2)})
    
    output$edad_prov2 <- renderUI(
      
      selectInput(inputId="edad_prov2",
                  label = "Edad",
                  choices = sort(unique(dd13()$edad)
                                 
                  )
      )
    )
    
    dd14 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov2 & 
                                               tabla == input$tabla_prov2 &
                                               unidad == input$unidad_prov2 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov2 &
                                               edad == input$edad_prov2)})
    
    
    output$nacionalidad_prov2 <- renderUI(
      
      selectInput(inputId="nacionalidad_prov2",
                  label = "Nacionalidad",
                  choices = sort(unique(dd14()$nacionalidad)
                                 
                  )
      )
    )
    
    dd15 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov2 & 
                                               tabla == input$tabla_prov2 &
                                               unidad == input$unidad_prov2 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov2 &
                                               edad == input$edad_prov2 &
                                               nacionalidad == input$nacionalidad_prov2)})
    
    output$durmat_prov2 <- renderUI(
      
      selectInput(inputId="durmat_prov2",
                  label = "Duración de matrimonio",
                  choices = sort(unique(dd15()$duracion_matrimonio)
                                 
                  )
      )
    )
    
    dd16 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov2 & 
                                               tabla == input$tabla_prov2 &
                                               unidad == input$unidad_prov2 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov2 &
                                               edad == input$edad_prov2 &
                                               nacionalidad == input$nacionalidad_prov2 &
                                               duracion_matrimonio == input$durmat_prov2)
    })
    
    output$nacord_prov2 <- renderUI(
      
      selectInput(inputId="nacord_prov2",
                  label = "Orden de nacimiento",
                  choices = sort(unique(dd16()$nacimiento_orden))
                  
      )
    )
    
    
    ## PROV VARIABLE 3 ------
    
    output$ind_prov3 <- renderUI(
      
      selectInput(inputId="ind_prov3",
                  label = "Indicador",
                  choices = sort(unique(dd0()$indicador)),
                  selected = sort(unique(dd0()$indicador))[3]
      )
    )
    
    dd17 <- reactive({ss_subset() %>% filter(nivel == input$level & indicador == input$ind_prov3)})
    
    output$tabla_prov3 <- renderUI(
      
      selectInput(inputId="tabla_prov3",
                  label = "Variable",
                  choices = sort(unique(dd17()$tabla)
                                 
                  )
      )
    )
    
    dd18 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov3 & 
                                               tabla == input$tabla_prov3)})
    
    output$unidad_prov3 <- renderUI(
      
      selectInput(inputId="unidad_prov3",
                  label = "Unidad",
                  choices = sort(unique(dd18()$unidad)
                                 
                  )
      )
    )
    
    
    dd20 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov3 & 
                                               tabla == input$tabla_prov3 &
                                               unidad == input$unidad_prov3 &
                                               as.integer(lubridate::year(periodo)) == input$all_years)})
    
    output$sexo_prov3 <- renderUI(
      
      selectInput(inputId="sexo_prov3",
                  label = "Sexo",
                  choices = sort(unique(dd20()$sexo)
                                 
                  )
      )
    )
    
    dd21 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov3 & 
                                               tabla == input$tabla_prov3 &
                                               unidad == input$unidad_prov3 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov3)})
    
    output$edad_prov3 <- renderUI(
      
      selectInput(inputId="edad_prov3",
                  label = "Edad",
                  choices = sort(unique(dd21()$edad)
                                 
                  )
      )
    )
    
    dd22 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov3 & 
                                               tabla == input$tabla_prov3 &
                                               unidad == input$unidad_prov3 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov3 &
                                               edad == input$edad_prov3)})
    
    
    output$nacionalidad_prov3 <- renderUI(
      
      selectInput(inputId="nacionalidad_prov3",
                  label = "Nacionalidad",
                  choices = sort(unique(dd22()$nacionalidad)
                                 
                  )
      )
    )
    
    dd23 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov3 & 
                                               tabla == input$tabla_prov3 &
                                               unidad == input$unidad_prov3 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov3 &
                                               edad == input$edad_prov3 &
                                               nacionalidad == input$nacionalidad_prov3)})
    
    output$durmat_prov3 <- renderUI(
      
      selectInput(inputId="durmat_prov3",
                  label = "Duración de matrimonio",
                  choices = sort(unique(dd23()$duracion_matrimonio)
                                 
                  )
      )
    )
    
    dd24 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                               indicador == input$ind_prov3 & 
                                               tabla == input$tabla_prov3 &
                                               unidad == input$unidad_prov3 &
                                               as.integer(lubridate::year(periodo)) == input$all_years &
                                               sexo == input$sexo_prov3 &
                                               edad == input$edad_prov3 &
                                               nacionalidad == input$nacionalidad_prov3 &
                                               duracion_matrimonio == input$durmat_prov3)
    })
    
    output$nacord_prov3 <- renderUI(
      
      selectInput(inputId="nacord_prov3",
                  label = "Orden de nacimiento",
                  choices = sort(unique(dd24()$nacimiento_orden))
                  
      )
    )
    
    
    
    ## PROV MAPS -----
    
    ## UNIVARIATE -----
    
    dataInput1 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                                     indicador == input$ind_prov & 
                                                     tabla == input$tabla_prov &
                                                     unidad == input$unidad_prov &
                                                     as.integer(lubridate::year(periodo)) == input$all_years &
                                                     sexo == input$sexo_prov &
                                                     edad == input$edad_prov &
                                                     nacionalidad == input$nacionalidad_prov &
                                                     duracion_matrimonio == input$durmat_prov) %>%  
        select(INSPIREID, valor)  %>% 
        set_colnames(c("INSPIREID", "var1"))
      
    })
    
    dataMapProv1 <- reactive({
      
      merge(spain_sf_subset %>% 
              filter(nivel == input$level) %>% 
              select(INSPIREID,NAMEUNIT),
            dataInput1(),
            by = "INSPIREID")                                    
      
    })
    
    output$map_prov_peni_si <- renderLeaflet({
      
      data <- dataMapProv1()
      
      bins <- seq(min(data$var1, na.rm = T), max(data$var1, na.rm = T), length.out = 9)
      
      pal <- colorBin("YlOrRd", domain = data$var1, bins = bins)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g ",
        data$NAMEUNIT, data$var1
      ) %>% lapply(htmltools::HTML)
      
      leaflet(st_transform(data, crs = 4326)) %>% 
        setView(-3.5, 40, 5) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas,
                         options = providerTileOptions(opacity = .8))%>%
        addPolygons(
          fillColor = ~pal(var1),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 1,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
      
    }
    
    )    
    
    
    output$map_prov_legend_si <- renderPlot({
      
      df <- data.frame(value = round(seq(min(dataMapProv1()$var1, na.rm = T),
                                         max(dataMapProv1()$var1, na.rm = T), 
                                         length.out = 5),2),
                       hex_code = brewer.pal(5, name="YlOrRd"),
                       stringsAsFactors=F)
      
      gg <- ggplot(df, aes(x = value, y = 1)) +
        geom_tile(aes(fill = as.factor(value)), colour = "transparent")+
        scale_x_continuous(breaks=df$value,expand=c(0,0))+
        scale_y_continuous(expand=c(0,0))+
        scale_fill_manual(values = df$hex_code)+
        theme(legend.position="none",
              axis.title=element_blank(),
              axis.line=element_blank(),
              axis.text.y=element_blank(),
              axis.text.x=element_text(hjust = .5),
              axis.ticks=element_blank(),
              plot.background=element_blank())+
        coord_fixed(ratio=2)
      
      shinyjs::delay(expr =({ 
        options(warn = storeWarn) 
      }) ,ms = 100) 
      
      gg
      
    }
    
    )
    
    
    ## BIVARIATE -----
    
    dataInput2 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                                     indicador == input$ind_prov2 & 
                                                     tabla == input$tabla_prov2 &
                                                     unidad == input$unidad_prov2 &
                                                     as.integer(lubridate::year(periodo)) == input$all_years &
                                                     sexo == input$sexo_prov2 &
                                                     edad == input$edad_prov2 &
                                                     nacionalidad == input$nacionalidad_prov2 &
                                                     duracion_matrimonio == input$durmat_prov2) %>%  
        select(INSPIREID, valor)  %>%  
        set_colnames(c("INSPIREID", "var2"))
      
    })
    
    
    dataMapProv2 <- reactive({
      
      merge(spain_sf_subset %>% 
              filter(nivel == input$level) %>% 
              select(INSPIREID,NAMEUNIT),
            merge(dataInput1(), dataInput2(), by = "INSPIREID"),
            by = "INSPIREID")                                    
      
    })
    
    
    
    output$map_prov_peni_bi <- renderLeaflet({
      
      data <- bi_class(dataMapProv2(), x = "var1", y = "var2", style = "quantile", dim = 3)
      
      bi_pal(pal = "DkViolet", dim = 3, preview=FALSE) %>% as.data.frame() %>%
        tibble::rownames_to_column() %>%
        mutate_if(is.factor, as.character) %>%
        set_colnames(c("bi_class", "hex_code"))  -> hex_codes
      
      pal <- colorFactor(
        palette = hex_codes$hex_code,
        domain = hex_codes$bi_class
      )
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%s",
        data$NAMEUNIT, data$bi_class
      ) %>% lapply(htmltools::HTML)
      
      leaflet(st_transform(data, crs = 4326)) %>% 
        setView(-3.5, 40, 5) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas,
                         options = providerTileOptions(opacity = .8))%>%
        addPolygons(
          fillColor = ~pal(bi_class),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 1,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))
      
    }
    
    )    
    
    output$map_prov_legend_bi <- renderPlot({
      
      bi_legend(pal = "DkViolet",
                dim = 3,
                xlab = "Variable 1",
                ylab = "Variable 2",
                size = 8)
      
    }
    )
    
    
    
    ## TERNARY ------
    
    dataInput3 <- reactive({ss_subset() %>% filter(nivel == input$level & 
                                                     indicador == input$ind_prov3 & 
                                                     tabla == input$tabla_prov3 &
                                                     unidad == input$unidad_prov3 &
                                                     as.integer(lubridate::year(periodo)) == input$all_years &
                                                     sexo == input$sexo_prov3 &
                                                     edad == input$edad_prov3 &
                                                     nacionalidad == input$nacionalidad_prov3 &
                                                     duracion_matrimonio == input$durmat_prov3) %>%  
        select(INSPIREID, valor)  %>% 
        set_colnames(c("INSPIREID", "var3")) %>%
        merge(dataInput2(), by = "INSPIREID")
      
    })
    
    
    dataMapProv3 <- reactive({
      
      merge(spain_sf_subset %>% 
              filter(nivel == input$level) %>% 
              select(INSPIREID,NAMEUNIT),
            merge(dataInput3(), dataInput1(), by = "INSPIREID"),
            by = "INSPIREID") %>%
        mutate(var1_ntile = ntile(var1, 100),
               var2_ntile = ntile(var2, 100),
               var3_ntile = ntile(var3, 100))
      
    })
    
    output$map_prov_peni_tri <- renderLeaflet({
      
      data <- dataMapProv3()
      
      tric <- Tricolore(data,
                        p1 = 'var1_ntile', 
                        p2 = 'var2_ntile', 
                        p3 = 'var3_ntile',
                        center = NA, crop = TRUE,
                        breaks = 5)
      
      
      data$rgb <- tric$rgb
      
      labels <- sprintf(
        "<strong>%s</strong><br/> V1/V2/V3: %s",
        data$NAMEUNIT,
        paste0(round(data$var1,1),"/",
               round(data$var2,1),"/",
               round(data$var3,1))) %>% 
        lapply(HTML)
      
      leaflet(st_transform(data, crs = 4326)) %>% 
        setView(-3.5, 40, 5) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas,
                         options = providerTileOptions(opacity = .8))%>%
        addPolygons(
          # fill
          fillColor   = ~rgb,
          fillOpacity = 0.9,
          # line
          dashArray   = "3",
          weight      = 1,
          color       = "white",
          opacity     = 1,
          # interaction
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 1,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", 
                         padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        )
      
      
      
    })
    
    output$map_prov_legend_tri <- renderImage({
      
      data <- dataMapProv3()
      
      tric <- Tricolore(data,
                        p1 = 'var1_ntile', 
                        p2 = 'var2_ntile', 
                        p3 = 'var3_ntile',
                        center = NA, crop = TRUE,
                        breaks = 5)
      
      outfile <- tempfile(fileext = '.png')
      
      # Generate the PNG
      png(outfile, width =200, height = 200)
      print(tric$key + labs(L = 'V1', T = 'V2', R = 'V3') + theme(axis.text=element_blank(),
                                                                  axis.ticks=element_blank()))
      dev.off()
      
      # Return a list containing the filename
      list(src = outfile,
           contentType = 'image/png',
           width = 170,
           height = 170,
           alt = "This is alternate text")
    }, deleteFile = TRUE)
    
    
    ## COMBINE MAPS -----
    
    output$plots<-renderUI(
      
      if(input$layout == "Choropleth map") {
        
        leafletOutput("map_prov_peni_si") %>% withSpinner(type = 6, color = "#FE1F62")
        
      }
      
      else if (input$layout == "Bivariate map") {
        
        leafletOutput("map_prov_peni_bi")   %>% withSpinner(type = 6, color = "#FE1F62")        
      }
      
      else if (input$layout == "Ternary composition") {
        
        leafletOutput("map_prov_peni_tri")   %>% withSpinner(type = 6, color = "#FE1F62")       
      }
      
      
    )
    
    ## COMBINE LEGENDS ------
    
    output$legends<-renderUI(
      
      if(input$layout == "Choropleth map") {
        
        plotOutput("map_prov_legend_si", height = "170px") %>% withSpinner(type = 7
                                                                           , color = "#4CDAC2")}
      
      else if (input$layout == "Bivariate map") {
        
        plotOutput("map_prov_legend_bi", height = "170px") %>% withSpinner(type = 7
                                                                           , color = "#4CDAC2")           
      }
      
      else if (input$layout == "Ternary composition") {
        
        imageOutput("map_prov_legend_tri", height = "170px")  %>% withSpinner(type = 7
                                                                              , color = "#4CDAC2")          
      }
      
      
    )
    
    
    
    ## METHOD DESCRIPTION ------
    
    
    output$legend_method_si <- renderText({
      
      "A choropleth map is a thematic map in which areas are shaded or patterned in proportion to the measurement of the statistical variable being displayed on the map, such as population density or per-capita income."        
    })
    
    output$legend_method_bi <- renderText({
      
      "A bivariate map displays two variables on a single map by combining two different sets of graphic symbols or colors. Bivariate mapping is an important technique in cartography. It is a variation of simple choropleth map that portrays two separate phenomena simultaneously." 
      
    })
    
    output$legend_method_tri <- renderText({
      
      "A ternary plot, ternary graph, triangle plot, simplex plot, Gibbs triangle or de Finetti diagram is a barycentric plot on three variables which sum to a constant. It graphically depicts the ratios of the three variables as positions in an equilateral triangle. 
                                    "                                   
    })
    
    output$methods <-renderUI(
      
      if(input$layout == "Choropleth map") {
        
        textOutput("legend_method_si")
      }
      
      else if (input$layout == "Bivariate map") {
        
        textOutput("legend_method_bi")           
      }
      
      else if (input$layout == "Ternary composition") {
        
        textOutput("legend_method_tri")
      }
      
      
    )
    
    ## LINKS TO GITHUB -----
    
    
    output$github_title_si <- renderText({"R Interface to Leaflet Maps"})
    
    output$legend_method_link_si <- renderUI({
      
      
      socialButton(
        url = "https://github.com/rstudio/leaflet",
        type = "github"
      )
      
    })
    
    output$github_title_bi <- renderText({"Bivariate Mapping with ggplot2"})
    
    output$legend_method_link_bi <- renderUI({
      
      socialButton(
        url = "https://github.com/slu-openGIS/biscale",
        type = "github"
      )
    })
    
    output$github_title_tri <- renderText({"A flexible color scale for ternary compositions"})
    
    output$legend_method_link_tri <- renderUI({
      
      socialButton(
        url = "https://github.com/jschoeley/tricolore",
        type = "github"
      )
    })
    
    output$github <-renderUI(
      
      if(input$layout == "Choropleth map") {
        
        htmlOutput("legend_method_link_si",align="center")
      }
      
      else if (input$layout == "Bivariate map") {
        
        htmlOutput("legend_method_link_bi",align="center")           
      }
      
      else if (input$layout == "Ternary composition") {
        
        htmlOutput("legend_method_link_tri",align="center")
      }
      
      
    )
    
    output$github_titles <-renderUI(
      
      if(input$layout == "Choropleth map") {
        
        textOutput("github_title_si")
      }
      
      else if (input$layout == "Bivariate map") {
        
        textOutput("github_title_bi")           
      }
      
      else if (input$layout == "Ternary composition") {
        
        textOutput("github_title_tri")
      }
      
      
    )
  } 
)
