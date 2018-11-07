library(shiny)
require(ggplot2)
require(sf)
setwd("/Users/taylorminich/Documents/grad/mp/data")
lymeMA <- read.csv('lymeMA.csv')
lymeMA <- lymeMA[-9, ]
lymeMA$group <- 1:length(lymeMA[,1])

dataBuilder <- function(x){
  county <- c()
  cases <- c()
  group <- c()
  for(i in 1:length(x[,1])){
    county <- append(county, paste(rep(x[i,1], length(2000:2016))))
    cases <- append(cases, as.numeric(x[i, 5:21]))
    group <- append(group, as.character(paste(rep(x[i,22], length(2000:2016)))))
  }
  year <- rep(2000:2016, length(x[,1]))
  data <- data.frame(county, group, year, cases)
  data
}
lymeMA <- dataBuilder(lymeMA)
maShape <- map_data('county', 'massachusetts')
maShape$group <- as.factor(maShape$group)
ma <- merge(lymeMA, maShape, sort = FALSE)
ma$year <- as.factor(ma$year)
plots1 <- list()
for(i in levels(ma$year)){
  name <- paste0('ma', i)
  plots1[[name]] <- ggplot() + geom_polygon(data = ma[ma$year == as.character(i),], 
                                           aes(x=long, y = lat, group = group, fill = cases)) + 
    coord_fixed(1.3) +
    scale_fill_gradient(low = "lightblue", high = "navy", limits=c(1, 1050)) + 
    theme_void()
}

year1 <- seq(1, 238, 17)
year2 <- seq(2, 238, 17)
year3 <- seq(3, 238, 17)
year4 <- seq(4, 238, 17)
year5 <- seq(5, 238, 17)
year6 <- seq(6, 238, 17)
year7 <- seq(7, 238, 17)
year8 <- seq(8, 238, 17)
year9 <- seq(9, 238, 17)
year10 <- seq(10, 238, 17)
year11 <- seq(11, 238, 17)
year12 <- seq(12, 238, 17)
year13 <- seq(13, 238, 17)
year14 <- seq(14, 238, 17)
year15 <- seq(15, 238, 17)
year16 <- seq(16, 238, 17)
year17 <- seq(17, 238, 17)
years <- rbind(year1, year2, year3, year4, year5,
               year6, year7, year8, year9, year10,
               year11, year12, year13, year14, year15,
               year16, year17)

plots2 <- list()

for(i in 1:length(years[,1])){
  name <- paste0('year', i)
  plots2[[name]] <- ggplot(data = lymeMA, aes(x = year, y = cases, group = group, col = cases)) +
    geom_line() + 
    scale_colour_gradient(low = "lightblue", high = "navy", limits=c(1, 1050)) +
    geom_point(aes(x = year[years[i,1]], y = cases[years[i,1]])) +
    geom_point(aes(x = year[years[i,2]], y = cases[years[i,2]])) +
    geom_point(aes(x = year[years[i,3]], y = cases[years[i,3]])) +
    geom_point(aes(x = year[years[i,4]], y = cases[years[i,4]])) +
    geom_point(aes(x = year[years[i,5]], y = cases[years[i,5]])) +
    geom_point(aes(x = year[years[i,6]], y = cases[years[i,6]])) +
    geom_point(aes(x = year[years[i,7]], y = cases[years[i,7]])) +
    geom_point(aes(x = year[years[i,8]], y = cases[years[i,8]])) +
    geom_point(aes(x = year[years[i,9]], y = cases[years[i,9]])) +
    geom_point(aes(x = year[years[i,10]], y = cases[years[i,10]])) +
    geom_point(aes(x = year[years[i,11]], y = cases[years[i,11]])) +
    geom_point(aes(x = year[years[i,12]], y = cases[years[i,12]])) +
    geom_point(aes(x = year[years[i,13]], y = cases[years[i,13]])) +
    geom_point(aes(x = year[years[i,14]], y = cases[years[i,14]])) +
    theme_bw()
}


ui <- fluidPage(
  sliderInput(inputId = 'year', 
              label = 'Choose a year',
              value = 2000, min = 2000, max = 2016, sep = ''),
  fluidPage(plotOutput('ggplot1'),
  plotOutput('ggplot2'), width = 8)
)

server <- function(input, output){
  output$ggplot1 <- renderPlot({
    plots1[[paste0('ma', input$year)]]
    })
    output$ggplot2 <- renderPlot({
      plots2[[paste0('year', input$year - 1999)]]
      
  })
}

shinyApp(ui = ui, server = server)