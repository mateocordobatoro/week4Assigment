library(shiny)
library(dplyr)
library(shinyjs)
library(plotly)
library(leaflet)
library(shinyBS)


data <- read.csv("occurence_poland.csv")
pics <- read.csv("picsPoland.csv")


ui <- fluidPage(
  theme = "mystyle.css",
  useShinyjs(),
  tags$head(tags$script(src="java.js")),
  
  # HEADER ----
  radioButtons("search_by","Search by:", c("Scientific name"="scientificName", "Vernacular name" = "vernacularName"), inline = TRUE),
  
  
  tags$div(class="container",
           class="nav",
           tags$li(textInput("name","Vernacular Name")),
           tags$li(actionButton("search","", icon = shiny::icon("search")))),
  
  
  # BODY ---- 
  
  
  br(),

  bsModal("search_modal",
          "Please select",
          "search",
          size = "large",
          uiOutput("formulas")),
  
  br(),

  uiOutput("ui2")
  
  
  
  )
  



server <- function(input, output, session) {
  

  # SEARCH MODAL ----

  observeEvent(input$id_prueba,{

    toggleModal(
      session,
      modalId = "search_modal",
      toggle = "toggle"
    )

  })


  observeEvent(input$search_by,{
    updateTextInput(
      session,
      "name",
      label = input$search_by
    )

  })


  search_results <- reactive({
    unique(data[,input$search_by])[grepl(trimws(input$name),unique(data[,input$search_by]))]
  })


  observeEvent(input$search,{

    output$formulas <- renderUI({

      req(input$name)
      
      
      if(length(search_results)<0){h3("No match found")}else{


      lapply(sort(isolate(search_results())), function(model) {

        tags$button(id = paste0(model),
                    style = "width:33%;margin-top: 15px;",
                    class = "btn action-button",
                    model, onClick="reply_click(this.id)"
        )

      })
      }


  })






  })

  output$map <- renderLeaflet({
    
    input$id_prueba
    
    data %>% filter(get(isolate(input$search_by)) %in% isolate(input$id_prueba)) %>%
      #mutate(content = paste(sep = "\n", get(input$search_by), scientificName, kingdom,family)) %>%
      select(one_of("vernacularName",
                    "scientificName",
                    "longitudeDecimal",
                    "latitudeDecimal",
                    "individualCount","content")) %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~longitudeDecimal,
                 lat = ~latitudeDecimal,
                 weight = 1,
                 radius =  ~individualCount*1000)



  })




  output$plot <- renderPlotly({


    data %>% filter(get(input$search_by) == input$id_prueba) %>%
      mutate(month = format(as.Date(eventDate),"%m"),
             year = format(as.Date(eventDate),"%Y"),
             date2 = paste("01",month,year, sep="-"),
             date2 = as.Date(date2, format = "%d-%m-%Y")) %>%
      group_by(date2) %>%
      summarise(n = sum(individualCount)) %>%
      arrange(date2) %>%
      ungroup() %>%
      plot_ly(x= ~date2,
              y = ~n,
              type = "scatter",
              mode = "lines+markers", fill = 'tozeroy',
              name = "Individual Count") %>%
      layout(showlegend = F,
             title="",
             yaxis = list(title = list(text ='', font = list(family = 'Arial'))),
             xaxis = list(rangeslider = list(visible = T),
                          title = "",
                                       showgrid = FALSE,
                                       showline = FALSE,
                                       zeroline = FALSE,
                          rangeselector=list(
                            buttons=list(
                              list(count=24, label="12m", step="month", stepmode="backward"),
                              list(count=36, label="36m", step="month", stepmode="backward"),
                              list(count=1, label="YTD", step="year", stepmode="todate"),
                              list(count=1, label="1y", step="year", stepmode="backward"),
                              list(step="all")
                            ))))



  })
  
  
  
  output$plot4 <- renderPlotly({
    
    

      df3 <- data %>%
                filter(get(input$search_by) %in% input$id_prueba) %>%
                count(locality) %>% arrange(desc(n)) %>% top_n(3) 
      
      df3$locality <- as.character(df3$locality)
      
      df3 %>%
        plot_ly(
          x = ~n,
          y = ~locality,
          type = 'bar',
          orientation = 'h',
          marker = list(color = 'rgba(58, 71, 80, 0.6)',
                        line = list(color = 'rgba(58, 71, 80, 1.0)',
                                    width = 3))) %>%
        layout(showlegend = F,
               title="",
               yaxis = list(title = list(text =''),showticklabels = FALSE),
               xaxis = list(title = list(text =''),
                            showgrid = FALSE,
                            showline = FALSE,
                            zeroline = FALSE))
    
    
    
  })
  
  
  output$plot5 <- renderPlotly({
    
    
    data %>% filter(get(input$search_by) %in% input$id_prueba) %>%
      mutate(month = format(as.Date(eventDate),"%m")) %>%
      group_by(month) %>%
      summarise(n = sum(individualCount)) %>%
      plot_ly(labels = ~month, values = ~n, type = 'pie') %>%
      layout(title = '')
    
    
    
    
  })
  
  

  observeEvent(input$id_prueba,{

    output$ui2 <- renderUI({
      
      
      input$id_prueba
      
      names_data <- data %>%
        distinct(scientificName,vernacularName) %>%
        left_join(pics %>% select(scientificName,accessURI), by = c("scientificName")) %>%
        rename(link = accessURI) %>%
        #mutate(link = ifelse(is.na(link),urlNA,link)) %>%
        filter(get(isolate(input$search_by)) == isolate(input$id_prueba))
      
        

      tags$div(class="grid-container",
               tags$div(class = "card",leafletOutput("map")),
               tags$div(class = "card1", img(src=names_data$link, style="width:100%; max-height: 300px;"),  
                        tags$div(class = "content-species",
                                 tags$div(class="item1", "Vernacular name:"),
                                 tags$div(class="item2",unique(names_data$vernacularName)[1]),
                                 tags$div(class="item3","Scientific name"),
                                 tags$div(class="item4",unique(names_data$scientificName)[1])
                        )),
               tags$div(class = "card", plotlyOutput("plot"), tags$div(class = "content-species", "Individual count figure time line")),
               tags$div(class = "card", plotlyOutput("plot4"), tags$div(class = "content-species", 'Highest localities occurrence')),
               tags$div(class = "card", plotlyOutput("plot5"), tags$div(class = "content-species", 'Highest months occurrence'))
      )



    })

  })


  output$print1 <- renderPrint({
    
    req(input$id_prueba)
    data %>%
      distinct(scientificName,vernacularName) %>%
      left_join(pics %>% select(scientificName,accessURI), by = c("scientificName")) %>%
      rename(link = accessURI) %>%
      #mutate(link = ifelse(is.na(link),urlNA,link)) %>%
      filter(get(input$search_by) == input$id_prueba)
    
  })


}

shinyApp(ui, server)




