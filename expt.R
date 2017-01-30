
library(grid)

# x and y are vectors of length 3
grad <- function(x, y) {
    slope1 <- (y[2] - y[1])/(x[2] - x[1])
    slope2 <- (y[3] - y[2])/(x[3] - x[2])
    (slope1 + slope2)/2
}

angle <- function(x, y) {
    atan2(y[3] - y[1], x[3] - x[1])
}

perp <- function(x, y, len) {
    a <- angle(x, y)
    dx <- len*cos(a + pi/2)
    dy <- len*sin(a + pi/2)
    upper <- c(x[2] + dx, y[2] + dy)
    lower <- c(x[2] - dx, y[2] - dy)
    list(upper, lower)
}

grid.newpage()
pushViewport(viewport(width=.9, height=.9))
path <- xsplinePoints(xsplineGrob(c(0, .5, 1), c(0, .5, 0), shape=1))
grid.points(path$x, path$y, pch=16, size=unit(2, "mm"))
N <- length(path$x)
if (N %/% 2 * 2 == N) {
    s <- seq(0, .1, length.out=N/2)
    width <- c(s, rev(s))
} else {
    s <- seq(0, .1, length.out=N %/% 2)
    width <- c(s, .1, rev(s))
}
upperx <- as.numeric(path$x)
lowerx <- as.numeric(path$x)
uppery <- as.numeric(path$y)
lowery <- as.numeric(path$y)
for (i in 2:(N - 1)) {
    seq <- (i - 1):(i + 1)
    ends <- perp(as.numeric(path$x[seq]), as.numeric(path$y[seq]), width[i])
    upperx[i] <- ends[[1]][1]
    uppery[i] <- ends[[1]][2]
    lowerx[i] <- ends[[2]][1]
    lowery[i] <- ends[[2]][2]
    grid.segments(upperx, uppery, lowerx, lowery,
                  default.units="in")
}
grid.path(c(upperx, rev(lowerx)), c(uppery, rev(lowery)),
          default.units="in")
popViewport()
pushViewport(viewport(y=1, width=.9, height=.9))
grid.path(c(upperx, rev(lowerx)), c(uppery, rev(lowery)),
          default.units="in",
          gp=gpar(col="black", fill="black"))


