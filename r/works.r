library(plotly)
library(ggtda)
library(TDAstats)
library(gganimate)
library(plyr)

myData <- read.csv( file = 'all.csv'
                  , header = FALSE
                  , sep = ','
                  , col.names = c('x','y','frame'))

myPlot <- plot_ly( data = myData
                 , x = ~x
                 , y = ~y
                 , frame = ~frame
                 , mode = 'markers'
                 , type = 'scatter'
                 , name = "Points"
                 , width = 800
                 , height = 800) %>% animation_opts(frame = 500, easing = "linear", transition = 0)

myPlot

myStats <- ddply(myData,~frame,summarise, xMean=mean(x),xSD=sd(x), yMean=mean(y),ySD = sd(y))

myStatsPlot <- plot_ly(data = myStats, frame = ~frame, name = "Stats") %>%
  add_trace(x = ~xMean, 
            y = ~yMean, 
            name = 'Means',
            mode = 'markers',
            type = 'scatter') %>%
  add_trace(x=~xSD, 
            y = ~ySD, 
            name = "Standard Deviations",
            mode = 'markers',
#            text = ~paste('(',format(xSD,digits = 6), ', ', format(ySD,digits=6), ')'),
#            textposition = "auto",
            type = 'scatter') %>%
  layout(xaxis = list(range = c(0,100)),
         yaxis = list(range = c(0,100)))

subplot(myPlot, myStatsPlot, nrows = 2)

myDataD <- subset(myData, frame %% 20 == 0, select=c(x,y,frame))

myPlotD <- plot_ly( data = myDataD
                   , x = ~x
                   , y = ~y
                   , frame = ~frame
                   , mode = 'markers'
                   , type = 'scatter'
                   , name = "Points"
                   , width = 800
                   , height = 800) %>% 
  animation_opts(frame = 1500, easing = "linear", transition = 0)

myPlotD

myStatsD <- ddply(myDataD,~frame,summarise, xMean=mean(x),xSD=sd(x), yMean=mean(y), ySD = sd(y))

myStatsPlotD <- plot_ly(data = myStatsD, frame = ~frame, name = "Stats") %>%
  add_trace(x = ~xMean, 
            y = ~yMean, 
            name = 'Means',
            mode = 'markers',
            type = 'scatter') %>%
  add_trace(x=~xSD, 
            y = ~ySD, 
            name = "Standard Deviations",
            mode = 'markers',
            #            text = ~paste('(',format(xSD,digits = 6), ', ', format(ySD,digits=6), ')'),
            #            textposition = "auto",
            type = 'scatter') %>%
  layout(xaxis = list(range = c(0,100)),
         yaxis = list(range = c(0,100)))

results <- subplot(myPlot, myStatsPlot, nrows = 2) %>%
  animation_opts(frame = 250, transition = 0)

results

myPh <- list()
for (i in 0:220) {
  tmp <- as.data.frame(calculate_homology(as.matrix(subset(myData, frame == i, select = c(x,y)))))
  tmp$frame <- i
  myPh[[i+1]] <- transform(tmp, dim=as.factor(dimension))
}

myPh <- do.call("rbind", myPh)

myPersPlot <- ggplot(myPh, aes(start=birth, end=death, colour=dim)) +
  theme_tda() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(aes(x = birth, y = death, frame=frame)) +
  xlim(0,50)

myPlotlyPers <- ggplotly(myPersPlot, frame=frame, width=800, height=800) %>% animation_opts(frame = 500, easing = "linear", transition = 0)

myPlotlyPers

subplot(myPlot, myPlotlyPers, nrows = 2)

myPhD <- subset(myPh, frame %% 20 == 0)

myPersPlotD <- ggplot(myPhD, aes(start=birth, end=death, colour=dim)) +
  theme_tda() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(aes(x = birth, y = death, frame=frame)) +
  xlim(0,50)

myPlotlyPersD <- ggplotly(myPersPlotD, frame=frame, width=600, height=800) %>% 
  animation_opts(frame = 1500, easing = "linear", transition = 0) %>%
  layout(xaxis = list(range = c(0,50)),
         yaxis = list(range = c(0,50)))

myPlotlyPersD

resultsD <- subplot(myPlotD, myPlotlyPersD, nrows = 2) %>%
  animation_opts(frame = 1500, transition = 0)

resultsD

myStats <- ggplot(myData, aes(x=mean(x), y=mean(y))) +
  geom_point() +
  geom_text(label = mean(myData$x)) +
  transition_time(time = frame) +
  ease_aes('sine-in-out') +
  enter_fade() +
  exit_shrink()

myStats
