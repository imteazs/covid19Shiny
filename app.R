#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(maps)

stateData = data.frame(read.csv('statewide_cases.csv'))
stateData$dateConvert = as.Date(stateData$date, '%Y-%m-%d')
counties = sort(unique(stateData$county))
# Define UI for application that draws a histogram
ui <- navbarPage(
    'California COVID19 Visualizer',
    tabPanel('CA Infection Rate',
             sidebarLayout(
               sidebarPanel(
                 selectInput(label = 'Test1', choices = c('blah', 'bhal'), inputId = 'grr')
               ),
               mainPanel(
                 leafletOutput('mycamap')
               )
             )),
    tabPanel('County Status',
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         label = 'Select County',
                         inputId = 'countySelect',
                         choices = counties
                         ),
                     selectInput(
                         label = 'Select Variables',
                         inputId = 'colSelect',
                         choices = c('Total Positive Cases',
                                     'Total Deaths',
                                     'New Cases',
                                     'New Deaths')
                         )
                     ),
                 mainPanel(
                     plotOutput('outfig')
                     )
                 )
             ),
    tabPanel('About')
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$mycamap = renderLeaflet({
    mapCounties = map("county", "california", fill = TRUE, plot = FALSE)
    leaflet(data = mapCounties) %>% addTiles() %>%
      addPolygons(fillColor = topo.colors(30, alpha = NULL), stroke = FALSE)
  })
  
  output$outfig = renderPlot({
    if(input$colSelect == 'Total Positive Cases'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, totalcountconfirmed)) + geom_line() + labs(x = 'Dates', y = 'Total Cases')
    }
    else if(input$colSelect == 'Total Deaths'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, totalcountdeaths)) + geom_line() + labs(x = 'Dates', y = 'Total Deaths')
    }
    else if(input$colSelect == 'New Cases'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, newcountconfirmed)) + geom_line() + labs(x = 'Dates', y = 'New Cases')
    }
    else if(input$colSelect == 'New Deaths'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, newcountdeaths)) + geom_line() + labs(x = 'Dates', y = 'New Deaths')
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
