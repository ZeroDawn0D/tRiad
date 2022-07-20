retriangulate <- function(nodeset,a,b,triangle_set,reqd_edges){
  #(1) Define X as the set of elements of nodeset strictly left of a->b and unseparated from the midpoint of a and b by any element of R.
  #If X is not empty:
    #(2) Determine x E X that maximizes angle axb.
    #(3) Add triangle abx to triangle_set
    #(4) Delete x from nodeset.
    #(5) If [a, x] not in R, retriangulate (nodeset, a, x, triangle_set, reqd_edges).
    #(6) If [x, b] not in R, retriangulate (nodeset, x, b, triangle_set, reqd_edges).
}

new_triad <- function(x,y,v,e){
  triad.obj <- list(
    x=x,
    y=y,
    v=v,
    e=e)
  class(triad.obj) <- "triad"
  triad.obj
}


del_tri <- function(x,y=NULL, maxrange=TRUE, ...){
  if(is.null(y)){
    x_p <- x$x
    y_p <- x$y
  }
  else {
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
  x_range <- x_max-x_min
  y_range <- y_max-y_min
  d_max <- max(x_range, y_range)
  if(maxrange){
    x_div <- d_max
    y_div <- d_max
  }else{
    x_div <- x_range
    y_div <- y_range
  }
  norm_x <- (x_p - x_min)/x_div
  norm_y <- (y_p - y_min)/x_div

  delaun(n,norm_x, norm_y)
}


delaun <- function(n, norm_x, norm_y){
  x <- norm_x
  y <- norm_y
  x <- c(x,-100,100,0)
  y <- c(y,-100,-100,100)
  # v1[i],v2[i],v3[i] are the three vertices of the i'th triangle
  v1 <- c(n+1)
  v2 <- c(n+2)
  v3 <- c(n+3)
  v <- matrix(c(n+1,n+2,n+3), nrow = 3, ncol = 1)
  # e1,e2,e3 are the adj tri of ith triangle
  e1 <- c(0)
  e2 <- c(0)
  e3 <- c(0)
  e <- matrix(c(0,0,0), nrow = 3, ncol = 1)
  ntri = 1
  tstack <- c()
  for(i in 1:n){
    enc_tri <- triloc(i,x,y,v,e)
    numtri <- length(v)/3
    V1 <- v[1,enc_tri]
    V2 <- v[2,enc_tri]
    V3 <- v[3,enc_tri]
    E1 <- e[1,enc_tri]
    E2 <- e[2,enc_tri]
    E3 <- e[3,enc_tri]
    #triangle T
    v[1,enc_tri] <- i
    v[2,enc_tri] <- V1
    v[3,enc_tri] <- V2

    e[1,enc_tri] <- E1
    e[2,enc_tri] <- numtri+1
    e[3,enc_tri] <- numtri+2

    newcol <- c(0,0,0)
    v <- cbind(v,newcol)
    v <- cbind(v,newcol)
    e <- cbind(e,newcol)
    e <- cbind(e,newcol)
    #triangle numtri+1

    v[1,numtri+1] <- i
    v[2,numtri+1] <- V2
    v[3,numtri+1] <- V3

    e[1,numtri+1] <- enc_tri
    e[2,numtri+1] <- E2
    e[3,numtri+1] <- numtri+2

    #triangle numtri+2
    v[1,numtri+2] <- i
    v[2,numtri+2] <- V3
    v[3,numtri+2] <- V1

    e[1,numtri+2] <- numtri+1
    e[2,numtri+2] <- E3
    e[3,numtri+2] <- enc_tri



    #stack if the edge opp P is adjacent to some other triangle
    if(e[2,enc_tri] != 0){
      tstack <- c(enc_tri,tstack)
    }
    if(e[2,numtri+1] != 0){
      tstack <- c(numtri+1, tstack)
    }
    if(e[2,numtri+2] != 0){
      tstack <- c(numtri+2,tstack)
    }
  }

  #Lawson's procedure

}
#' Check which triangles a point lies within
#'
#' This function takes partial triangulation
#' and checks whether points are inside of
#' the existing triangles
#'
#' @param i row index
#' @param d tibble with two components, x, y
#'  giving positions of points to be triangulated
#' @param v vertices of triangles, columns are triangle
#'   number and rows are vertices 1, 2, and 3
#' @param e adjacency of triangles, columns are triangle
#'   number and rows are ids of adjacent triangles
#'
#' @export

triloc <- function(i,x,y,v,e){
  cur.tri = 1
  # arc1 is opposite v1
  triangle.found = FALSE
  Px <- x[i]
  Py <- y[i]
  while(!triangle.found){
    #edge1->2
    Ax <- x[v[1,cur.tri]]
    Ay <- y[v[1,cur.tri]]
    Bx <- x[v[2,cur.tri]]
    By <- y[v[2,cur.tri]]
    lr12 <- left.right(Px,Py,Ax,Ay,Bx,By)
    if(lr12<0){
      cur.tri <- e[1,cur.tri]
      next
    }

    #edge2->3
    Ax <- x[v[2,cur.tri]]
    Ay <- y[v[2,cur.tri]]
    Bx <- x[v[3,cur.tri]]
    By <- y[v[3,cur.tri]]
    lr23 <- left.right(Px,Py,Ax,Ay,Bx,By)
    if(lr23<0){
      cur.tri <- e[2,cur.tri]
      next
    }

    #edge3->1
    Ax <- x[v[3,cur.tri]]
    Ay <- y[v[3,cur.tri]]
    Bx <- x[v[1,cur.tri]]
    By <- y[v[1,cur.tri]]
    lr31 <- left.right(Px,Py,Ax,Ay,Bx,By)
    if(lr31<0){
      cur.tri <- e[3,cur.tri]
      next
    }

    return(cur.tri)
  }
  return(-1)
}

# if true, new point is inside circumcircle for triangle R
swap <- function(){

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
