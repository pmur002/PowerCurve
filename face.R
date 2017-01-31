
## Draw a face using vwline()s
source("vwline.R")

eyelash <- function(x, y, angle, flip=FALSE) {
    xoffset <- unit(c(0, 1, 2, 3), "mm")
    if (flip) {
        xoffset <- -1*xoffset
    } 
    pushViewport(viewport(x, y, just=c("centre", "bottom"), angle=angle))
    grid.vwline(x=unit(.5, "native") - xoffset,
                y=unit(c(0, 4, 6, 8), "mm"),
                w=unit(3:0/4, "mm"), shape=1, endShape=0,
                gp=gpar(fill="black"))
    ## grid.rect(.5, 0, .1, .1, just=c("centre", "bottom"))
    popViewport()
}

test <- function() {
    grid.newpage()
    eyelash(.5, .5, 0)
    eyelash(.45, .5, 10)
    eyelash(.55, .5, -10)
    eyelash(.75, .5, 0, flip=TRUE)
}

eyelid <- function(x, y, w, angle, flip=FALSE) {
    xx <- unit(c(0, .2, .5, .75, 1), "npc")
    yy <- unit(c(0, -4, -6, -4, 0), "mm")
    pushViewport(viewport(x, y, just=c("centre", "bottom"),
                          width=w, angle=angle))
    lid <- vwlineGrob(x=xx, y=yy,
                      w=unit(c(0, 1, 2, 1, 0), "mm"), shape=1, endShape=0,
                      gp=gpar(fill="black"))
    grid.draw(lid)
    pts <- vwEdgePoints(lid, seq(.2, .8, .2), "lower")
    for (i in 1:4) {
        eyelash(pts$lower$x[i], pts$lower$y[i],
                180*pts$lower$tangent[i]/pi, flip=flip)
    }
    ## grid.rect()
    popViewport()    
}

nose <- function(x, y) {
    pushViewport(viewport(x, y, just=c("left", "bottom")))
    grid.xspline(unit(c(-10, 10, 3, -3), "mm"),
                 unit(c(8, 8, -8, -8), "mm"),
                 open=FALSE, shape=1, gp=gpar(lwd=4))
    popViewport()
}

grid.newpage()
eyelid(.7, .7, .25, -20)
eyelid(.3, .7, .25, 20, flip=TRUE)
nose(.5, .5)
