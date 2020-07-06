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

stateData = data.frame(read.csv('statewide_cases.csv'))
stateData$dateConvert = as.Date(stateData$date, '%Y-%m-%d')
# Define UI for application that draws a histogram
ui <- navbarPage(
    'California COVID19 Visualizer',
    tabPanel('CA Infection Rate'),
    tabPanel('County Status',
             sidebarLayout(
                 sidebarPanel(
                     selectInput(
                         label = 'Select County',
                         inputId = 'countySelect',
                         choices = unique(stateData$county)
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
             )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$outfig = renderPlot({
    if(input$colSelect == 'Total Positive Cases'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, totalcountconfirmed)) + geom_line()
    }
    else if(input$colSelect == 'Total Deaths'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, totalcountdeaths)) + geom_line()
    }
    else if(input$colSelect == 'New Cases'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, newcountconfirmed)) + geom_line()
    }
    else if(input$colSelect == 'New Deaths'){
      newData = filter(stateData, county == input$countySelect)
      ggplot(data = newData, aes(dateConvert, newcountdeaths)) + geom_line()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
