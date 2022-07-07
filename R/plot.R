# center (a,b) circle (x=a+r*cos(theta), y=b+r*sin(theta) ) theta = [0,2pi]
circle <- function(a,b,r, ...){
  angle <- seq(0,2*pi,length = 360)
  x <- a + r*cos(angle)
  y <- b + r*sin(angle)
  lines(x,y)
}

circles <- function(x,y,r, ...){
  n <- length(x)
  stopifnot(n==length(y) && n==length(r))
  for(i in 1:n){
    circle(x[i],y[i],r[i], ...)
  }
}
