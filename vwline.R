
library(grid)

################################################################################
## Utility functions

## x and y are vectors of length 2
angle <- function(x, y) {
    atan2(y[2] - y[1], x[2] - x[1])
}

## extend direction from pt 2 to pt 1
extend <- function(x, y, len) {
    a <- angle(x[2:1], y[2:1])
    dx <- len*cos(a)
    dy <- len*sin(a)
    c(x[1] + dx, y[1] + dy)
}

## x and y are vectors; ends defines subset of length 2
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

vwlineGrob <- function(x, y, w, shape=0,
                       endWidth=unit(0, "mm"), endShape=shape, ...,
                       gp=NULL, debug=FALSE) {
    checkvwline(x, y, w)
    gTree(x=x, y=y, w=w, shape=shape, endWidth=endWidth, endShape=endShape,
          debug=debug, gp=gp, cl="vwlineGrob")
}

checkvwline <- function(x, y, w) {
    if (max(length(x), length(y), length(w)) < 3)
        stop("A vwline must have at least three control points")
    nx <- length(x)
    ny <- length(y)
    nw <- length(w)
    if (nx != ny || nx != nw) {
        stop("x, y, and w must all have same length (for now)")
    }
}

## Generate a set of control points from which we can produce
## one or more XSplines for the outline of the vwline
## NOTE that we want to be able to produce separate upper and lower (and mid)
## XSplines (for edge points) as well as a single overall boundary XSpline
## (for drawing)
vwControlPoints <- function(grob) {
    N <- length(grob$x)
    x <- convertX(grob$x, "in", valueOnly=TRUE)
    y <- convertY(grob$y, "in", valueOnly=TRUE)
    w <- pmin(convertWidth(grob$w, "in", valueOnly=TRUE),
              convertHeight(grob$w, "in", valueOnly=TRUE))
    ew <- pmin(convertWidth(grob$endWidth, "in", valueOnly=TRUE),
               convertHeight(grob$endWidth, "in", valueOnly=TRUE))
    startw <- ew[1]
    if (length(grob$endWidth) > 1) {
        endw <- ew[2]
    } else {
        endw <- startw
    }
    upperx <- numeric(N)
    uppery <- numeric(N)
    lowerx <- numeric(N)
    lowery <- numeric(N)
    midx <- numeric(N)
    midy <- numeric(N)
    perps <- perpStart(x[1:2], y[1:2], w[1])
    upperx[1] <- perps[1, 1]
    uppery[1] <- perps[1, 2]
    lowerx[1] <- perps[2, 1]
    lowery[1] <- perps[2, 2]
    ends <- extend(x[1:2], y[1:2], startw)
    midx[1] <- ends[1]
    midy[1] <- ends[2]
    for (i in 2:(N - 1)) {
        seq <- (i - 1):(i + 1)
        perps <- perp3(as.numeric(x[seq]), as.numeric(y[seq]), w[i])
        ## FIXME: need to remove ends where perpendicular intersects
        ##        with previous perpendicular (upper or lower)
        upperx[i] <- perps[1, 1]
        uppery[i] <- perps[1, 2]
        lowerx[i] <- perps[2, 1]
        lowery[i] <- perps[2, 2]
        midx[i] <- x[i]
        midy[i] <- y[i]
    }
    perps <- perpEnd(x[(N-1):N], y[(N-1):N], w[N])
    upperx[N] <- perps[1, 1]
    uppery[N] <- perps[1, 2]
    lowerx[N] <- perps[2, 1]
    lowery[N] <- perps[2, 2]
    ends <- extend(x[N:(N-1)], y[N:(N-1)], endw)
    midx[N] <- ends[1]
    midy[N] <- ends[2]
    list(upperx=upperx, uppery=uppery,
         lowerx=lowerx, lowery=lowery,
         midx=midx, midy=midy)
}

## A single XSpline for makeContent() method (and for xDetails() method)
vwSingleXSpline <- function(grob) {
    N <- length(grob$x)
    cp <- vwControlPoints(grob)
    ## Debugging
    if (grob$debug) {
        grid.points(cp$midx, cp$midy, pch=16, size=unit(2, "mm"),
                    default.units="in",
                    gp=gpar(col="red"))
        grid.segments(cp$upperx, cp$uppery, cp$lowerx, cp$lowery,
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
    vwShape <- c(rep(startShape, 2), rep(grob$shape, N - 2),
                 rep(endShape, 3),
                 rep(grob$shape, N - 2), rep(startShape, 2))
    xsplineGrob(c(cp$midx[1], cp$upperx, cp$midx[N], rev(cp$lowerx)),
                c(cp$midy[1], cp$uppery, cp$midy[N], rev(cp$lowery)),
                default.units="in",
                open=FALSE, shape=vwShape)
}

makeContent.vwlineGrob <- function(x, ...) {
    addGrob(x, vwSingleXSpline(x))            
}

xDetails.vwlineGrob <- function(x, theta) {
    xDetails(vwSingleXSpline(x), theta)
}

yDetails.vwlineGrob <- function(x, theta) {
    yDetails(vwSingleXSpline(x), theta)
}

## Calculate sets of points along upper, lower, and mid of vwline
vwPoints <- function(grob) {
    N <- length(grob$x)
    cp <- vwControlPoints(grob)
    ## Allow for endShape to be length 2
    startShape <- grob$endShape[1]
    if (length(grob$endShape) > 1) {
        endShape <- grob$endShape[2]
    } else {
        endShape <- grob$endShape
    }
    upperShape <- c(rep(startShape, 3), rep(grob$shape, N - 2),
                    rep(endShape, 3))
    upperXSpline <- xsplineGrob(c(cp$lowerx[1], cp$midx[1], cp$upperx,
                                  cp$midx[N], cp$lowerx[N]),
                                c(cp$lowery[1], cp$midy[1], cp$uppery,
                                  cp$midy[N], cp$lowery[N]),
                                default.units="in",
                                shape=upperShape, repEnds=FALSE)
    lowerShape <- c(rep(startShape, 3), rep(grob$shape, N - 2),
                    rep(endShape, 3))
    lowerXSpline <- xsplineGrob(c(cp$upperx[1], cp$midx[1], cp$lowerx,
                                  cp$midx[N], cp$upperx[N]),
                                c(cp$uppery[1], cp$midy[1], cp$lowery,
                                  cp$midy[N], cp$uppery[N]),
                                default.units="in",
                                shape=lowerShape, repEnds=FALSE)
    midShape <- c(startShape, rep(grob$shape, N - 2), endShape)
    midXSpline <- xsplineGrob(cp$midx, cp$midy, 
                              default.units="in",
                              shape=midShape, repEnds=FALSE)
    list(upper=xsplinePoints(upperXSpline),
         lower=xsplinePoints(lowerXSpline),
         mid=xsplinePoints(midXSpline))
}

## Generate points specified propn along upper/lower/mid of vwline at specified
vwEdgePoints <- function(x, p, which=c("upper", "lower", "mid"), debug=FALSE) {
    pts <- vwPoints(x)
    result <- list(uppper=NULL, lower=NULL, mid=NULL)
    if ("upper" %in% which) {
        result$upper=boundaryPoints(pts$upper, p, debug)
    }
    if ("lower" %in% which) {
        result$lower=boundaryPoints(pts$lower, p, debug)
    }
    if ("mid" %in% which) {
        result$mid=boundaryPoints(pts$mid, p, debug)
    }
    result
}

boundaryPoints <- function(pts, p, debug) {
    ## each boundary is a unit in "in"
    x <- as.numeric(pts$x)
    y <- as.numeric(pts$y)
    ## Calculate total length of boundary
    lengths <- sqrt(diff(x)^2 + diff(y)^2)
    cumLength <- cumsum(lengths)
    length <- sum(lengths)
    ## Determine point selection (clamp to [0,length])
    breaks <- p*length
    ifelse(breaks < 0, 0, ifelse(breaks > length, length, breaks))
    index <- apply(outer(breaks, cumLength, "<="), 1,
                   function(x) min(which(x)))
    ## Add tangent info
    n <- length(breaks)
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

## Simple curve with variable width
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

## Variations on shape and endshape
test2 <- function() {
    testShape <- function(row, col, shape, endShape, width,
                          endWidth=0, border=FALSE) {
        pushViewport(viewport(layout.pos.col=col, layout.pos.row=row))
        if (border) grid.rect()
        grid.vwline(unit(c(.2, .5, .8, .8), "npc"),
                    unit(c(.8, .8, .5, .2), "npc"),
                    unit(width/2, "cm"),
                    shape=shape, endShape=endShape,
                    endWidth=unit(endWidth/2, "cm"),
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
    testShape(4, 2, 1, c(1, 0), c(1, 2/3, 1/3, 0), endWidth=c(1, 0),
              border=TRUE)
    testShape(4, 3, 1, 1, c(1, 3/4, 2/4, 1/4), endWidth=1, border=TRUE)
}

## Get boundary points via grobX()
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

## Get boundary points via vwPoints()
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

## Get boundary points via vwPoints() with labels and perps
test5 <- function(p=seq(.1, .9, length=5)) {
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
    pts <- vwEdgePoints(vw, p, debug=TRUE)
    popViewport()
}

