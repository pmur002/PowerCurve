
library(grid)

## Test fill rule (on power-curve-type shape with tight corner)
grid.newpage()
pushViewport(viewport(y=.5, height=.5, just="bottom"))
grid.path(c(.1, .2, .3, .4, .5, .4, .25, .3, .35, .2),
          c(.1, .5, .6, .5, .1, .1, .4, .35, .4, .1),
          ## NOTE the need to specify 'id', else x/y are drawn as polygon
          ## which ignores the fill rule !!!
          id=rep(1, 10),
          gp=gpar(fill=rgb(0,0,0,.5)), rule="evenodd")
popViewport()
pushViewport(viewport(y=0, height=.5, just="bottom"))
grid.path(c(.1, .2, .3, .4, .5, .4, .25, .3, .35, .2),
          c(.1, .5, .6, .5, .1, .1, .4, .35, .4, .1),
          id=rep(1, 10),
          gp=gpar(fill=rgb(0,0,0,.5)), rule="winding")
popViewport()

## Test drawing of xspline curve from set of upper and lower (and mid)
## control points
upper <- list(x=c(.1, .5, .9), y=c(.7, .6, .5))
lower <- list(x=c(.1, .5, .9), y=c(.3, .4, .5))
testspline <- function(shape) {
    grid.newpage()
    grid.xspline(c(0, upper$x, 1, rev(lower$x)),
                 c(.5, upper$y, .5, rev(lower$y)),
                 shape=shape, open=FALSE,
                 gp=gpar(col="green", lwd=5))
    grid.xspline(upper$x, upper$y, shape=shape, gp=gpar(lwd=2))
    grid.xspline(lower$x, lower$y, shape=shape,  gp=gpar(lwd=2))
}
testspline(0)
testspline(1)
testspline(-1)
testspline2 <- function(shape) {
    grid.newpage()
    grid.xspline(c(0, upper$x, 1, rev(lower$x)),
                 c(.5, upper$y, .5, rev(lower$y)),
                 shape=shape, open=FALSE,
                 gp=gpar(col="green", lwd=5))
    grid.xspline(c(lower$x[1], 0, upper$x, 1, lower$x[3]),
                 c(lower$y[1], .5, upper$y, .5, lower$y[3]),
                 shape=shape, repEnds=FALSE, gp=gpar(lwd=2))
}
testspline2(0)
testspline2(1)
testspline2(-1)
