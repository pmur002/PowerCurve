
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
                w=unit(3:0/8, "mm"), shape=1, endShape=0,
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
                      w=unit(c(0, 1, 2, 1, 0)/2, "mm"), shape=1, endShape=0,
                      gp=gpar(fill="black"))
    grid.draw(lid)
    pts <- vwEdgePoints(lid, seq(.2, .8, .2), "lower")
    for (i in 1:4) {
        eyelash(pts$lower$x[i], pts$lower$y[i],
                180*pts$lower$tangent[i]/pi + 180, flip=flip)
    }
    ## grid.rect()
    popViewport()    
}

lip <- function(x, y, angle, flip=FALSE) {
    xoffset <- unit(c(0, 1, 4, 9)*2, "mm")
    if (flip) {
        xoffset <- -1*xoffset
    } 
    pushViewport(viewport(x, y, just=c("centre", "bottom"), angle=angle))
    grid.vwline(x=unit(.5, "native") - xoffset,
                y=unit(c(0, 4, 8, 12)*2, "mm"),
                w=unit(3:0/8, "mm"), shape=1, endShape=0,
                gp=gpar(fill="black"))
    popViewport()
}

nose <- function(x, y) {
    pushViewport(viewport(x, y, just=c("left", "bottom")))
    nosegrob <- vwlineGrob(unit(c(0, -15, 15), "mm"),
                           unit(c(-10, 10, 10), "mm"),
                           unit(c(1, 2, 2)/2, "mm"),
                           open=FALSE, shape=.7,
                           gp=gpar(fill="black"),
                           name="nose")
    grid.draw(nosegrob)
    pts <- vwEdgePoints(nosegrob, c(.03, .97), "upper")
    lip(pts$upper$x[1], pts$upper$y[1],
        180*pts$upper$tangent[1]/pi, flip=TRUE)
    lip(pts$upper$x[2], pts$upper$y[2],
        180*pts$upper$tangent[2]/pi)
    popViewport()
}

whiskers <- function(x, y) {
    pushViewport(viewport(x, y, just=c("left", "bottom")))
    grid.points(unit(runif(6, -1, 1)/2, "cm"),
                unit(runif(6, -1, 1)/2, "cm"),
                size=unit(2, "mm"), pch=16)
    popViewport()
}

png("face.png", width=20, height=10, units="in", res=100, bg="transparent")
grid.newpage()
pushViewport(viewport(width=unit(7, "in"), height=unit(7, "in")))
eyelid(.65, .7, .2, -20)
eyelid(.35, .7, .2, 20, flip=TRUE)
nose(.5, .5)
whiskers(.35, .5)
whiskers(.65, .5)
dev.off()

