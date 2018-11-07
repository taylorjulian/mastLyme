# set working directory, upload lyme disease data, create group numbers for counties
setwd("/Users/taylorminich/Documents/grad/mp/data")
lymeMA <- read.csv('lymeMA.csv')
lymeMA <- lymeMA[-9, ]
lymeMA$group <- 1:length(lymeMA[,1])
lymeNC <- read.csv('lymeNC.csv')
lymeNC <- lymeNC[-66, ]
lymeNC$group <- 1:length(lymeNC[,1])

# function that creates a data frame with county name, group number, year, and number of cases
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

legend(2000, 1100, levels(lymeMA$county), fill = levels(lymeMA$group), box.lty = 0)

years <- seq(1, 238, 17)
ggplot(data = lymeMA, aes(x = year, y = cases, group = group, col = cases)) +
  geom_line(show.legend = FALSE) + 
  scale_colour_gradient(low = "lightblue", high = "navy", limits=c(1, 1050)) +
  geom_point(aes(x = year[years[1]], y = cases[years[1]])) +
  geom_point(aes(x = year[years[2]], y = cases[years[2]])) +
  geom_point(aes(x = year[years[3]], y = cases[years[3]])) +
  geom_point(aes(x = year[years[4]], y = cases[years[4]])) +
  geom_point(aes(x = year[years[5]], y = cases[years[5]])) +
  geom_point(aes(x = year[years[6]], y = cases[years[6]])) +
  geom_point(aes(x = year[years[7]], y = cases[years[7]])) +
  geom_point(aes(x = year[years[8]], y = cases[years[8]])) +
  geom_point(aes(x = year[years[9]], y = cases[years[9]])) +
  geom_point(aes(x = year[years[10]], y = cases[years[10]])) +
  geom_point(aes(x = year[years[11]], y = cases[years[11]])) +
  geom_point(aes(x = year[years[12]], y = cases[years[12]])) +
  geom_point(aes(x = year[years[13]], y = cases[years[13]])) +
  geom_point(aes(x = year[years[14]], y = cases[years[14]])) +
  theme_bw()

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

maShape <- map_data('county', 'massachusetts')
maShape$group <- as.factor(maShape$group)
ncShape <- map_data('county', 'north carolina')
ncShape$group <- as.factor(ncShape$group)

ma <- merge(lymeMA, maShape, sort = FALSE)
ma$year <- as.factor(ma$year)
nc <- merge(lymeNC, ncShape, sort = FALSE)
nc$year <- as.factor(nc$year)

plots <- list()
for(i in levels(ma$year)){
  name <- paste0('ma', i)
  plots[[name]] <- ggplot() + geom_polygon(data = ma[ma$year == as.character(i),], 
                                  aes(x=long, y = lat, group = group, fill = cases)) + 
           coord_fixed(1.3) +
           scale_fill_gradient(low = "lightblue", high = "navy", limits=c(1, 1050)) + 
           theme_bw()
}

