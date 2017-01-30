
library(grid)

################################################################################
## Utility functions

## x and y are vectors of length 2
angle <- function(x, y) {
    atan2(y[2] - y[1], x[2] - x[1])
}

perp <- function(x, y, len, ends, mid) {
    a <- angle(x[ends], y[ends])
    dx <- len*cos(a + pi/2)
    dy <- len*sin(a + pi/2)
    upper <- c(x[mid] + dx, y[mid] + dy)
    lower <- c(x[mid] - dx, y[mid] - dy)
    rbind(upper, lower)
}

## x and y are vectors of length 2
perpStart <- function(x, y, len) {
    perp(x, y, len, 1:2, 1)
}

perpEnd <- function(x, y, len) {
    perp(x, y, len, 1:2, 2)
}

## x and y are vectors of length 3
perp3 <- function(x, y, len) {
    perp(x, y, len, c(1, 3), 2)
}

################################################################################
## "variable-width" line

grid.vwline <- function(...) {
    grid.draw(vwlineGrob(...))
}

vwlineGrob <- function(x, y, w, shape=0, endShape=shape, ...,
                       gp=NULL, debug=FALSE) {
    if (max(length(x), length(y), length(w)) < 3)
        stop("A vwline must have at least three control points")
    gTree(x=x, y=y, w=w, shape=shape, endShape=endShape, debug=debug,
          cl="vwlineGrob", gp=gp)
}

vwXSpline <- function(grob) {
    nx <- length(grob$x)
    ny <- length(grob$y)
    nw <- length(grob$w)
    if (nx != ny || nx != nw) {
        stop("x, y, and w must all have same length (for now)")
    }
    N <- nx
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- pmin(convertWidth(grob$w, "in", valueOnly=TRUE),
              convertHeight(grob$w, "in", valueOnly=TRUE))
    upperx <- numeric(N)
    lowerx <- numeric(N)
    uppery <- numeric(N)
    lowery <- numeric(N)
    ends <- perpStart(x[1:2], y[1:2], w[1])
    upperx[1] <- ends[1, 1]
    uppery[1] <- ends[1, 2]
    lowerx[1] <- ends[2, 1]
    lowery[1] <- ends[2, 2]
    for (i in 2:(N - 1)) {
        seq <- (i - 1):(i + 1)
        ends <- perp3(as.numeric(x[seq]), as.numeric(y[seq]), w[i])
        ## FIXME: need to remove ends where perpendicular intersects
        ##        with previous perpendicular (upper or lower)
        upperx[i] <- ends[1, 1]
        uppery[i] <- ends[1, 2]
        lowerx[i] <- ends[2, 1]
        lowery[i] <- ends[2, 2]
    }
    ends <- perpEnd(x[(N-1):N], y[(N-1):N], w[N])
    upperx[N] <- ends[1, 1]
    uppery[N] <- ends[1, 2]
    lowerx[N] <- ends[2, 1]
    lowery[N] <- ends[2, 2]
    ## Debugging
    if (grob$debug) {
        grid.points(x, y, pch=16, size=unit(2, "mm"),
                    default.units="in",
                    gp=gpar(col="red"))
        grid.segments(upperx, uppery, lowerx, lowery,
                      default.units="in",
                      gp=gpar(col="red"))
    }
    ## Allow for endShape to be length 2
    startShape <- grob$endShape[1]
    if (length(grob$endShape) > 1) {
        endShape <- grob$endShape[2]
    } else {
        endShape <- grob$endShape
    }
    vwShape <- c(startShape, rep(grob$shape, N - 2),
                 rep(endShape, 2),
                 rep(grob$shape, N - 2), startShape)
    xsplineGrob(c(upperx, rev(lowerx)),
                c(uppery, rev(lowery)),
                default.units="in",
                open=FALSE, shape=vwShape)
}

makeContent.vwlineGrob <- function(x, ...) {
    addGrob(x, vwXSpline(x))            
}

xDetails.vwlineGrob <- function(x, theta) {
    xDetails(vwXSpline(x), theta)
}

yDetails.vwlineGrob <- function(x, theta) {
    yDetails(vwXSpline(x), theta)
}

vwPoints <- function(x) {
    xsplinePoints(vwXSpline(x))
}

## Generate 'n' equally(-ish)-spaced points along boundary of vwline
vwEdgePoints <- function(x, n, offset=0, debug=FALSE) {
    pts <- vwPoints(x)
    ## pts is a unit in "in"
    x <- as.numeric(pts$x)
    y <- as.numeric(pts$y)
    ## Calculate total length of boundary
    lengths <- sqrt((c(x[-1], x[1]) - x)^2 + (c(y[-1], y[1]) - y)^2)
    cumLength <- cumsum(lengths)
    length <- sum(lengths)
    ## Adjust for offset by reordering x/y
    start <- min(which(cumLength > offset*length))
    if (start > 1) {
        x <- x[c(start:length(x), 1:(start-1))]
        y <- y[c(start:length(x), 1:(start-1))]
        ## Recalculate
        lengths <- sqrt((c(x[-1], x[1]) - x)^2 + (c(y[-1], y[1]) - y)^2)
        cumLength <- cumsum(lengths)
    }
    ## Determine point selection
    step <- length / n
    breaks <- seq(0, length, step)[-(n + 1)]
    index <- apply(outer(breaks, cumLength, "<"), 1,
                   function(x) min(which(x)))
    ## Add tangent info
    tangent <- numeric(n)
    for (i in 1:n) {
        below <- index[i] - 1
        if (below < 1) below <- length(x)
        above <- index[i] + 1
        if (above > length(x)) above <- 1
        tangent[i] <- angle(x[c(above, below)], y[c(above, below)])
    }
    x <- unit(x[index], "in")
    y <- unit(y[index], "in")
    if (debug) {
        grid.points(x, y, size=unit(2, "mm"), 
                    pch=16, gp=gpar(col="red"))
        grid.segments(x, y,
                      x + unit(5*cos(tangent - pi/2), "mm"),
                      y + unit(5*sin(tangent - pi/2), "mm"),
                      gp=gpar(col="red"))
        grid.text(1:n, x, y)
    }
    list(x=x, y=y, tangent=tangent)
}

################################################################################
## Testing

test <- function() {
    grid.newpage()
    pushViewport(viewport(width=.9, height=.9))
    path <- xsplinePoints(xsplineGrob(c(0, .5, 1), c(0, .5, 0), shape=1))
    N <- length(path$x)
    if (N %/% 2 * 2 == N) {
        s <- seq(0, 5, length.out=N/2)
        width <- unit(c(s, rev(s)), "mm")
    } else {
        s <- seq(0, 5, length.out=N %/% 2)
        width <- unit(c(s, 5, rev(s)), "mm")
    }
    grid.vwline(path$x, path$y, width, debug=TRUE)
    popViewport()
}

test2 <- function() {
    testShape <- function(row, col, shape, endShape, width, border=FALSE) {
        pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
        if (border) grid.rect()
        grid.vwline(unit(c(.2, .5, .8, .8), "npc"),
                    unit(c(.8, .8, .5, .2), "npc"),
                    unit(width/2, "cm"),
                    shape=shape, endShape=endShape,
                    gp=gpar(fill=rgb(0,0,0,.5)),
                    debug=TRUE)
        popViewport()
    }
    grid.newpage()
    pushViewport(viewport(width=.9, height=.9,
                          layout=grid.layout(4, 3, respect=TRUE)))
    pushViewport(viewport(layout.pos.col=1))
    grid.text("endShape = 0", y=1, just="bottom")
    popViewport()
    pushViewport(viewport(layout.pos.col=2))
    grid.text("endShape = 1", y=1, just="bottom")
    popViewport()
    pushViewport(viewport(layout.pos.col=3))
    grid.text("endShape = -1", y=1, just="bottom")
    popViewport()
    pushViewport(viewport(layout.pos.row=1))
    grid.text("shape = 0", x=0, just="right")
    popViewport()
    pushViewport(viewport(layout.pos.row=2))
    grid.text("shape = 1", x=0, just="right")
    popViewport()
    pushViewport(viewport(layout.pos.row=3))
    grid.text("shape = -1", x=0, just="right")
    popViewport()
    testShape(1, 1, 0, 0, rep(1, 4))
    testShape(1, 2, 0, 1, rep(1, 4))
    testShape(1, 3, 0, -1, rep(1, 4))
    testShape(2, 1, 1, 0, rep(1, 4))
    testShape(2, 2, 1, 1, rep(1, 4))
    testShape(2, 3, 1, -1, rep(1, 4))
    testShape(3, 1, -1, 0, rep(1, 4))
    testShape(3, 2, -1, 1, rep(1, 4))
    testShape(3, 3, -1, -1, rep(1, 4))
    testShape(4, 1, 1, c(0, 0), c(1, 2/3, 1/3, 0), border=TRUE)
    testShape(4, 2, 1, c(1, 0), c(1, 2/3, 1/3, 0), border=TRUE)
    testShape(4, 3, 1, 1, c(1, 3/4, 2/4, 1/4), border=TRUE)
}

test3 <- function() {
    grid.newpage()
    pushViewport(viewport(width=.9, height=.9))
    path <- xsplinePoints(xsplineGrob(c(0, .5, 1), c(.4, .6, .4), shape=1))
    N <- length(path$x)
    if (N %/% 2 * 2 == N) {
        s <- seq(0, 5, length.out=N/2)
        width <- unit(c(s, rev(s)), "mm")
    } else {
        s <- seq(0, 5, length.out=N %/% 2)
        width <- unit(c(s, 5, rev(s)), "mm")
    }
    vw <- vwlineGrob(path$x, path$y, width)
    grid.draw(vw)
    for (i in seq(0, 360, 10)) {
        grid.points(grobX(vw, i), grobY(vw, i), size=unit(2, "mm"),
                    pch=16, gp=gpar(col="red"))
    }
    popViewport()
}

test4 <- function() {
    grid.newpage()
    pushViewport(viewport(width=.9, height=.9))
    path <- xsplinePoints(xsplineGrob(c(0, .5, 1), c(.4, .6, .4), shape=1))
    N <- length(path$x)
    if (N %/% 2 * 2 == N) {
        s <- seq(0, 5, length.out=N/2)
        width <- unit(c(s, rev(s)), "mm")
    } else {
        s <- seq(0, 5, length.out=N %/% 2)
        width <- unit(c(s, 5, rev(s)), "mm")
    }
    vw <- vwlineGrob(path$x, path$y, width)
    grid.draw(vw)
    pts <- vwPoints(vw)
    grid.points(pts$x, pts$y, size=unit(2, "mm"),
                pch=16, gp=gpar(col="red"))
    popViewport()
}

test5 <- function(n=11, offset=0) {
    grid.newpage()
    pushViewport(viewport(width=.9, height=.9))
    path <- xsplinePoints(xsplineGrob(c(0, .5, 1), c(.4, .6, .4), shape=1))
    N <- length(path$x)
    if (N %/% 2 * 2 == N) {
        s <- seq(0, 5, length.out=N/2)
        width <- unit(c(s, rev(s)), "mm")
    } else {
        s <- seq(0, 5, length.out=N %/% 2)
        width <- unit(c(s, 5, rev(s)), "mm")
    }
    vw <- vwlineGrob(path$x, path$y, width)
    grid.draw(vw)
    pts <- vwEdgePoints(vw, n, offset=offset, debug=TRUE)
    popViewport()
}
