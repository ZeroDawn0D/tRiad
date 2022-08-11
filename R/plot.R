#x <- c(0,1,-2,3,0,0,1,1,-1,0)
#y <- c(1,-2,-2,2,6,-1,0,2,0,0)
#v <- matrix(nrow=3, ncol = 4)
#e <- matrix(nrow=3, ncol = 4)
#v[1,] <- c(1,1,1,1)
#v[2,] <- c(3,2,4,5)
#v[3,] <- c(2,4,5,3)
#e[1,] <- c(4,1,2,3)
#e[2,] <- c(0,0,0,0)
#e[3,] <- c(2,3,4,1)
#t <- new_triad(x,y,v,e)
#plot(t)


#'@title plot function for objects of triad class
#'@description Plots the Delaunay triangulation on passing a triad object
#'@param x An object of triad class
#'@param numtext Whether points are numbered in the plot
#'@param ... Additional arguments to pass to plot()
#'@export
plot.triad <- function(x,numtext=FALSE, ...){
  y <- x$y
  v <- x$v
  x <- x$x
  plot(x, y, ...)
  n<-length(y)
  if(numtext){
    graphics::text(x,y,1:n, pos = 3)
  }
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
    graphics::lines(xplot,yplot)
  }
}
