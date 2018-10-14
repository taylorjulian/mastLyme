setwd("/Users/taylorminich/Documents/grad/mp/data")
lymeMA <- read.csv('lymeMA.csv')
lymeMA$group <- 1:length(lymeMA[,1])
lymeNC <- read.csv('lymeNC.csv')
lymeNC$group <- 1:length(lymeNC[,1])

dataBuilder <- function(x){
  county <- c()
  cases <- c()
  group <- c()
  for(i in 1:length(x[,1])){
    county <- append(county, paste(rep(x[i,1], length(2000:2016))))
    cases <- append(cases, as.numeric(x[i, 5:21]))
    group <- append(group, paste(rep(x[i,22], length(2000:2016))))
  }
  year <- rep(2000:2016, length(x[,1]))
  data <- data.frame(county, group, year, cases)
  data
}

lymeMA <- dataBuilder(lymeMA)
lymeNC <- dataBuilder(lymeNC)

require(sf)
require(ggplot2)

ma <- map_data('county', 'massachusetts')
nc <- map_data('county', 'north carolina')


ggplot() + geom_polygon(data = ma, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
