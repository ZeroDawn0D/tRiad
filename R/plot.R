plot.triad <- function(triad.obj, ...){
  x <- triad.obj$x
  y <- triad.obj$y
  v <- triad.obj$v
  plot(x, y)
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
