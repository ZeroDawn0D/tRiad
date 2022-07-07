
retriangulate <- function(nodeset,a,b,triangle_set,reqd_edges){
  #(1) Define X as the set of elements of nodeset strictly left of a->b and unseparated from the midpoint of a and b by any element of R.
  #If X is not empty:
    #(2) Determine x E X that maximizes angle axb.
    #(3) Add triangle abx to triangle_set
    #(4) Delete x from nodeset.
    #(5) If [a, x] not in R, retriangulate (nodeset, a, x, triangle_set, reqd_edges).
    #(6) If [x, b] not in R, retriangulate (nodeset, x, b, triangle_set, reqd_edges).
}

del_tri <- function(x,y=NULL, ...){
  if(is.null(y)){
    x_p <- x$x
    y_p <- y$y
  }
  else{
    x_p <- x
    y_p <- y
  }
  n <- length(x_p)
  if(n != length(y_p)){
    stop("unequal size of x and y")
  }
  x_min <- min(x_p)
  x_max <- max(x_p)
  y_min <- min(y_p)
  y_max <- max(y_p)
  d_max <- max(x_max-x_min, y_max-y_min)
  norm_x <- (x_p-x_min)/d_max
  norm_y <- (y_p - y_min)/d_max
  points <- data.frame(index = 1:n,
                          x=x_p,y=y_p,
                          norm_x,norm_y)
  delaun(n,points)
}


delaun <- function(n,points){
  x <- points$norm_x
  y <- points$norm_y
  x <- c(x,-100,100,0)
  y <- c(y,-100,-100,100)
  # v1[i],v2[i],v3[i] are the three vertices of the i'th triangle
  v1 <- c(n+1)
  v2 <- c(n+2)
  v3 <- c(n+3)
  # e1,e2,e3 are the adj tri of ith triangle
  e1 <- c(0)
  e2 <- c(0)
  e3 <- c(0)
  ntri = 1
  for(i in 1:n){
    enc_tri <- triloc(i,x,y,v1,v2,v3,e1,e2,e3)
  }
}

triloc <- function(i,x,y,v1,v2,v3,e1,e2,e3){
  cur.tri = 1
  # arc1 is opposite v1
  triangle.found = FALSE
  Px <- x[i]
  Py <- y[i]
  while(!triangle.found){
    #edge1->2
    Ax <- x[v1[cur.tri]]
    Ay <- y[v1[cur.tri]]
    Bx <- x[v2[cur.tri]]
    By <- y[v2[cur.tri]]
    lr12 <- left.right(Px,Py,Ax,Ay,Bx,By)
    if(lr12<0){
      cur.tri <- e1[cur.tri]
      next
    }

    #edge2->3
    Ax <- x[v2[cur.tri]]
    Ay <- y[v2[cur.tri]]
    Bx <- x[v3[cur.tri]]
    By <- y[v3[cur.tri]]
    lr23 <- left.right(Px,Py,Ax,Ay,Bx,By)
    if(lr23<0){
      cur.tri <- e2[cur.tri]
      next
    }

    #edge3->1
    Ax <- x[v3[cur.tri]]
    Ay <- y[v3[cur.tri]]
    Bx <- x[v1[cur.tri]]
    By <- y[v1[cur.tri]]
    lr31 <- left.right(Px,Py,Ax,Ay,Bx,By)
    if(lr31<0){
      cur.tri <- e3[cur.tri]
      next
    }

    return(cur.tri)
  }
}

#checks if P is left or right of A->B
left.right <- function(Px,Py,Ax,Ay,Bx,By){
  ABx <- Bx-Ax
  ABy <- By-Ay
  APx <- Px-Ax
  APy <- Py-Ay
  cross_prod <- ABx*APy - APx*ABy
  cross_prod
}
