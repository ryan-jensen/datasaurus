library(TDAstats)
library(ggtda)
library(plotly)

# remotes::install_github("rrrlw/ggtda", vignettes = TRUE)

num.pts <- 100
rand.angle <- runif(num.pts, 0, 2*pi)
pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
pt.cloud <- data.frame(
  x = cos(rand.angle),
  y = sin(rand.angle)
)

pers.hom <- as.data.frame(calculate_homology(as.matrix(pt.cloud)))

# plot_barcode(pers.hom)

print(head(pers.hom.frame))
pers.hom <- transform(pers.hom, dim= as.factor(dimension))

prox <- 2/3

per.dia <- ggplot(pt.cloud, aes(x=x, y=y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3", alpha=0.15) +
  geom_point()

per.dia

per.simp <- ggplot(pt.cloud, aes(x=x,y=y))+
  theme_bw() +
  coord_fixed() +
  stat_vietoris2(diameter = prox, fill="darkgoldenrod", alpha=0.1) +
  stat_vietoris1(diameter = prox, alpha = 0.25) +
  stat_vietoris0()

per.simp

myData <- read.csv( file = 'all.csv'
                    , header = FALSE
                    , sep = ','
                    , col.names = c('x','y','frame'))


myAll <- subset(myData, frame==20, select=c(x,y))
myAll
plot(myAll)

prox <- 10
per.dia <- ggplot(myWedge, aes(x=x, y=y)) +
  theme_bw() +
  coord_fixed() +
  stat_disk(radius = prox/2, fill = "aquamarine3", alpha=0.15) +
  geom_point()

per.dia

per.simp <- ggplot(myWedge, aes(x=x,y=y))+
  theme_bw() +
  coord_fixed() +
  stat_vietoris2(diameter = prox, fill="darkgoldenrod", alpha=0.1) +
  stat_vietoris1(diameter = prox, alpha = 0.25) +
  stat_vietoris0()

per.simp

pers.hom <- as.data.frame(calculate_homology(as.matrix(myWedge)))
pers.hom <- transform(pers.hom, dim= as.factor(dimension))

per.barcode <- ggplot(pers.hom, aes(start = birth, end = death, colour = dim)) +
  theme_tda() +
  geom_barcode() +
  labs(x="Diameter", y="Homological feature")

per.barcode

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

pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
pers.hom <- calculate_homology(pt.cloud)
preplot <- plot_barcode(pers.hom)
preplot

ptly1 <- ggplotly(preplot)
ptly1
