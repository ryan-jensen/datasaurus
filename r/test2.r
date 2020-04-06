library(plotly)

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
                 , width = 800
                 , height = 800) %>% animation_opts(frame = 500, easing = "linear", transition = 0)

myPlot

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
  geom_point(aes(x = birth, y = death, frame=frame))

myPlotlyPers <- ggplotly(myPersPlot, frame=frame, width=800, height=800) %>% animation_opts(frame = 500, easing = "linear", transition = 0)

myPlotlyPers

subplot(myPlot, myPlotlyPers, nrows = 2)
