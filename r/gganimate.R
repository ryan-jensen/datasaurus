# install from GitHub
remotes::install_github("rrrlw/ggtda", vignettes = FALSE)

library(ggplot2)
library(gganimate)
library(ggtda)
library(TDAstats)
library(plyr)

myData <- read.table( file = "all.csv"
                      , header = FALSE
                      , sep = ','
                      , col.names = c("x", "y", "frame"))

myAnimate <- ggplot(myData, aes(x=x,y=y)) + 
  geom_point() +
  transition_time(time = frame) +
  ease_aes('sine-in-out') +
  enter_fade() +
  exit_shrink()


animate(myAnimate, fps = 20, nframes = 20*12)

calculate_homology(subset(myData, frame == 0))

ph0 <- as.data.frame(calculate_homology(as.matrix(subset(myData, frame == 0, select=c(x,y)))))
ph0 <- transform(ph0, dim = as.factor(dimension))
ph0$frame <- 0
ph20 <- as.data.frame(calculate_homology(as.matrix(subset(myData, frame == 20, select=c(x,y)))))
ph20 <- transform(ph20, dim=as.factor(dimension))
ph20$frame <- 20

ph <- rbind(ph0, ph20)

myPh <- list()
for (i in 0:80) {
  #myPh[i] <- i*i
  tmp <- as.data.frame(calculate_homology(as.matrix(subset(myData, frame == i, select = c(x,y)))))
  tmp$frame <- i
  myPh[[i+1]] <- transform(tmp, dim=as.factor(dimension))
}

myPh <- do.call("rbind", myPh)

myAnimate2 <- ggplot(myPh, aes(start = birth, end = death, colour=dim)) + 
  theme_tda () + 
  coord_fixed() +
  stat_persistence() +
  transition_time(time = frame) +
  ease_aes('sine-in-out') +
  enter_fade() +
  exit_shrink()

animate(myAnimate2, fps = 20, nframes = 20*8)
