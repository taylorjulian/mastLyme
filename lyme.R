# set working directory, upload lyme disease data, create group numbers for counties
setwd("/Users/taylorminich/Documents/grad/mp/data")
lymeMA <- read.csv('lymeMA.csv')
lymeMA$group <- 1:length(lymeMA[,1])
lymeNC <- read.csv('lymeNC.csv')
lymeNC$group <- 1:length(lymeNC[,1])

# function that creates a data frame with county name, group number, year, and number of cases
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

# runs function on lyme disease data
lymeMA <- dataBuilder(lymeMA)
lymeNC <- dataBuilder(lymeNC)

# creates cases vs year plot for MA
xrange <- c(2000, 2016)
yrange <- c(min(lymeMA$cases), max(lymeMA$cases))
plot(x = NULL, y = NULL, xlim = xrange, ylim = yrange, 
     xlab = 'year', ylab = 'cases', bty = 'l', las = 1)

for(i in 1:length(lymeMA$group)){
  lines(lymeMA$year[lymeMA$group == i], 
         lymeMA$cases[lymeMA$group == i], col = i)
}

# creates cases vs year plot for NC
xrange <- c(2000, 2016)
yrange <- c(min(lymeNC$cases), max(lymeNC$cases))
plot(x = NULL, y = NULL, xlim = xrange, ylim = yrange, 
     xlab = 'year', ylab = 'cases', bty = 'l', las = 1)

for(i in 1:length(lymeNC$group)){
  lines(lymeNC$year[lymeNC$group == i], 
        lymeNC$cases[lymeNC$group == i], col = i)
}


# maps results of lyme disease data
require(sf)
require(ggplot2)

ma <- map_data('county', 'massachusetts')
nc <- map_data('county', 'north carolina')


ggplot() + geom_polygon(data = ma, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
