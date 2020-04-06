library(plotly)

packageVersion('plotly')

df <- data.frame(
  x = c(1,2,2,3),
  y = c(1,2,2,3),
  f = c(1,1,2,2)
#  x = c(1,2,1), 
#  y = c(1,2,1), 
#  f = c(1,2,3)
)

p <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )

p

myMean = mean(iris$Petal.Length)
myVar = sd(iris$Petal.Length)

pp <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width, type = 'scatter', mode = 'markers',
             text = ~paste('Mean: ', myMean, '\nSD: ', myVar))
pp

myData <- read.table( file = "all.csv"
                    , header = FALSE
                    , sep = ','
                    , col.names = c("x", "y"))
mean(datas0$x)
mean(datas0$y)

datas20000 <- read.table( file = "test20000.csv"
                        , header = FALSE
                        , sep = ','
                        , col.names = c("x", "y"))
mean(datas20000$x)
mean(datas20000$y)
plot(datas0)
plot(datas20000)

datas40000 <- read.table( file = "test40000.csv"
                        , header = FALSE
                        , sep = ','
                        , col.names = c("x", "y"))

plot(datas40000)

ptly <- plot_ly( data = myData
               , x = ~x
               , y = ~y
               , frame = ~frame
               , type = 'scatter'
               , mode = 'markers'
               , text = ~paste( 'X-Mean: ', mean(x)
                              , '\nY-Mean: ', mean(y)
                              , '\nX-SD: ', sd(x)
                              , '\nY-SD: ', sd(y)))
ptly

ptl2 <- plot_ly( data = datas42000
                 , x = ~x
                 , y = ~y
                 , type = 'scatter'
                 , mode = 'markers'
                 , text = ~paste( 'X-Mean: ', mean(x)
                                  , '\nY-Mean: ', mean(y)
                                  , '\nX-SD: ', sd(x)
                                  , '\nY-SD: ', sd(y)))

ptl2

xax = list( title = 'X-Mean: ')

ptly <- plot_ly( data = myData
                 , x = ~x
                 , y = ~y
                 , frame = frame
                 , type = 'scatter'
                 , mode = 'markers'
                 , text = ~paste( 'X-Mean: ', mean(x)
                                  , '\nY-Mean: ', mean(y)
                                  , '\nX-SD: ', sd(x)
                                  , '\nY-SD: ', sd(y)))

ptly


f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = paste("X-Mean: ", mean(datas0$x))
)
y <- list(
  title = "y Axis",
  titlefont = f
)
p <- plot_ly(data=myData, x = ~x, y=~y, mode = "markers") %>%
  layout(xaxis = x, yaxis = y)
p
