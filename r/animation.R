library(animation)

ani.options(interval = 1, nmax = 50)

par(pch = 20)

xx = newton.method()

xx$root

newton.method(function(x) 5 * x^3 - 7 * x^2 - 40 * x + 120, 7.15, c(-6.2,7.1))

