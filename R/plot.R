#'@title plot function for objects of triad class
#'@description Plots the Delaunay triangulation on passing a triad object
#'@param x An object of triad class
#'@param ... Additional arguments to pass to plot()
#'@export
plot.triad <- function(x, ...){
  y <- x$y
  v <- x$v
  x <- x$x
  plot(x, y, ...)
  ntri <- length(v)/3
  for(i in 1:ntri){
    xplot <- c(
      x[v[1,i]],
      x[v[2,i]],
      x[v[3,i]],
      x[v[1,i]])
    yplot <- c(
      y[v[1,i]],
      y[v[2,i]],
      y[v[3,i]],
      y[v[1,i]])
    lines(xplot,yplot)
  }
}
