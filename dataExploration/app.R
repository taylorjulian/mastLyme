library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

sidebarLayout(
  
  sidebarPanel(
    sliderInput("obs", "Number of observations:",  
                min = 1, max = 1000, value = 500)
  ),

ui <- fluidPage(
  titlePanel('Data exploration'), 
  sidebarLayout(
    sidebarPanel(
      radioButtons('site', 'Site',
                   c('Bartlett Experimental Forest' = 'BART',
                     'Blandy Experimental Farm' = 'BLAN', 
                     'Disney Wilderness Preserve' = 'DSNY',
                     'Great Smoky Mountains National Park, Twin Creeks' = 'GRSM',
                     'Harvard Forest' = 'HARV',
                     'Jones Ecological Research Center' = 'JERC', 
                     'Konza Prairie Biological Station' = 'KONZ', 
                     'Oak Ridge' = 'ORNL',
                     'Ordway-Swisher Biological Station' = 'OSBS', 
                     'Smithsonian Conservation Biology Institute' = 'SCBI', 
                     'Smithsonian Environmental Research Center' = 'SERC',
                     'The University of Kansas Field Station' = 'UKFS'))
    )
  ,
  mainPanel(
    plotOutput('abundance')
  )
 )
)
)


server <- function(input, output) {
  mammal <- read.csv('mammal.csv') %>%
    select(collectDate, siteID, total, totalTraps) %>%
    mutate(collectDate = ymd(collectDate))
  tick <- read.csv('tick.csv') %>%
    select(collectDate, siteID, totalCount, totalSampledArea) %>%
    mutate(collectDate = ymd(collectDate))
  
  output$abundance <- renderPlot({
    mammal <- mammal %>%
      filter(siteID %in% input$site) %>%
      mutate(tmp = total/totalTraps) %>%
      mutate(abundance = tmp/max(tmp)) %>%
      select(collectDate, siteID, total, totalTraps, abundance)
    tick <- tick %>%
      filter(siteID %in% input$site) %>%
      mutate(tmp = totalCount/totalSampledArea) %>%
      mutate(abundance = tmp/max(tmp)) %>%
      select(collectDate, siteID, totalCount, totalSampledArea, abundance)
    ggplot(data = NULL) +
      geom_point(data = mammal, aes(x = collectDate, y = abundance), colour = '#225ea8', alpha = 0.5) +
      geom_point(data = tick, aes(x = collectDate, y = abundance), colour = '#cc4c02', alpha = 0.5) +
      geom_line(data = mammal, aes(x = collectDate, y = abundance, colour = 'mammal'), stat="smooth", method = "loess", alpha = 0.9) +
      geom_line(data = tick, aes(x = collectDate, y = abundance, colour = 'tick'), 
                stat="smooth",method = "loess", alpha = 0.9) +
      scale_y_continuous(limits = c(0, 1)) +
      xlab('Collection date') +
      ylab('Relative abundance') +
      scale_colour_manual(name = '', values = c(mammal = '#225ea8', tick = '#cc4c02')) +
      theme_minimal() +
      theme(legend.position = 'bottom')
  })
}

shinyApp(ui = ui, server = server)
