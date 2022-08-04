#retriangulate <- function(nodeset,a,b,triangle_set,reqd_edges){
  #(1) Define X as the set of elements of nodeset strictly left of a->b and unseparated from the midpoint of a and b by any element of R.
  #If X is not empty:
    #(2) Determine x E X that maximizes angle axb.
    #(3) Add triangle abx to triangle_set
    #(4) Delete x from nodeset.
    #(5) If [a, x] not in R, retriangulate (nodeset, a, x, triangle_set, reqd_edges).
    #(6) If [x, b] not in R, retriangulate (nodeset, x, b, triangle_set, reqd_edges).
#}



x <- c(0,10,-20,30,10,-10)
y <- c(10,-20,-20,20,0,0)

#'@title Constructor for the triad class
#'@description An object which stores the information of all vertices and triangles for the Delaunay Triangulation
#'@param x X coordinates of points
#'@param y y coordinates of points
#'@param v A 2D matrix storing information of vertices that make up a triangle
#'@param e A 2D matrix storing information of the triangles adjacent to each other
#'
#'@export
new_triad <- function(x,y,v,e){
  triad.obj <- list(
    x=x,
    y=y,
    v=v,
    e=e)
  class(triad.obj) <- "triad"
  triad.obj
}


#'@title Implementation of the DELTRI subroutine
#'@description Calculates the Delaunay Triangulation of the given set of points.
#'@param x stores the x coordinates of the points. If y is NULL, it is treated as a data.frame
#'@param y stores the Y coordinates of the points
#'@param maxrange if TRUE, normalisation is done with max(x range, y range) for both x and y values
#'
#'@export
del_tri <- function(x,y=NULL, maxrange=TRUE){
  print("del_tri()")
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

  delaun(norm_x, norm_y)
}

#'@title Implementation of the DELAUN subroutine
#'@description Returns a Delaunay Triangulation but with normalised points
#'@param norm_x Normalised X coordinates of points
#'@param norm_y NormalisedY coordinates of points
#'
delaun <- function(norm_x, norm_y){
  x <- norm_x
  y <- norm_y
  n <- length(x)
  x <- c(x,-100,100,0)
  y <- c(y,-100,-100,100)
  # v1[i],v2[i],v3[i] are the three vertices of the i'th triangle
  v <- matrix(c(n+1,n+2,n+3), nrow = 3, ncol = 1)
  # e1,e2,e3 are the adj tri of ith triangle
  e <- matrix(c(0,0,0), nrow = 3, ncol = 1)
  ntri = 1
  tstack <- c()
  triad.obj <- new_triad(x,y,v,e)
  plot(triad.obj)

  for(i in 1:n){

    enc_tri <- triloc(i,x,y,v,e)
    cat("Point ", i, " found in triangle ", enc_tri)
    readline(prompt="Press [enter] to continue")
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

    e[1,enc_tri] <- numtri+2
    e[2,enc_tri] <- E1
    e[3,enc_tri] <- numtri+1

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


    numtri <- numtri+2
    #stack if the edge opp P is adjacent to some other triangle
    if(E1 != 0){
      cat("pushing ", enc_tri, "\n")
      tstack <- c(enc_tri,tstack)
    }
    if(E2 != 0){
      e[edg(e,E2,enc_tri), E2] <- numtri-1
      cat("pushing ", numtri-1, "\n")
      tstack <- c(numtri-1, tstack)
    }
    if(E3 != 0){
      e[edg(e,E3,enc_tri), E3] <- numtri
      cat("pushing ", numtri, "\n")
      tstack <- c(numtri,tstack)
    }
    triad.obj <- new_triad(x,y,v,e)
    plot(triad.obj)

  }

  print("Current state of V")
  print(v)

  print("Current state of E")
  print(e)
  print("Lawson")
  #Lawson's procedure
  while(length(tstack) > 0){
    cat("Current Stack: ", tstack, "\n")
    L <- tstack[1]
    R <- e[2,L]
    tstack <- tstack[-1]
    cat("popping ", L, "\n")
    cat("Vertices of Triangle L: ",v[1,L]," ",v[2,L]," ",v[3,L],"\n")
    cat("Vertices of Triangle R: ",v[1,R]," ",v[2,R]," ",v[3,R],"\n")
    ERL <- edg(e,R,L)
    ERA <- ERL%%3 + 1
    ERB <- ERA%%3 + 1
    V1 <- v[ERL,R]
    V2 <- v[ERA,R]
    V3 <- v[ERB,R]
    P <- -1
    for(i in 1:3){
      if(v[i,L]!=V1 && v[i,L] !=V2){
        P<-v[i,L]
        break
      }
    }
    if(P==-1){
      print("P is negative")
    }
    if(swap(x,y,P,V1,V2,V3)){
      print("swap true")
      A <- e[ERA,R]
      B <- e[ERB,R]
      C <- e[3,L]

      #triangle L
      v[3,L] <- V3
      e[2,L] <- A
      e[3,L] <- R

      #triangle R
      v[1,R] <- P
      v[2,R] <- V3
      v[3,R] <- V1
      e[1,R] <- L
      e[2,R] <- B
      e[3,R] <- C

      if(A !=0){
        e[edg(e,A,R),A] <- L
        cat("pushing ", L, "\n")
        tstack <- c(L,tstack)
      }
      if(B != 0){
        cat("pushing ", R, "\n")
        tstack <- c(R,tstack)
      }
      if(C != 0){
        e[edg(e,C,L),C] <- R
      }
      readline(prompt="Press [enter] to continue")
      triad.obj <- new_triad(x,y,v,e)
      plot(triad.obj)
    }
    ntri <- length(v)/3
  }

  if(ntri != 2*n+1){
    print('incorrect number of triangles formed')
  }
  ntri <- length(v)/3
  cat("number of triangles:",ntri, "\n")
  for(t in 1:ntri){
    if(v[1,t]>n || v[2,t]>n || v[3,t]>n){
      for(i in 1:3){
        A <- e[i,t]
        if(A!=0){
          e[edg(e,A,t),A] <- 0
        }
      }
      break
    }
  }
  tstrt <- t+1
  tstop <- ntri
  ntri <- t-1
  for(t in tstrt:tstop){
    if(v[1,t]>n || v[2,t]>n || v[3,t]>n){
      for(i in 1:3){
        A <- e[i,t]
        if(A!=0){
          e[edg(e,A,t),A] <- 0
        }
      }
    }else{
      ntri <- ntri+1
      for(i in 1:3){
        A <- e[i,t]
        e[i,ntri] <- A
        v[i,ntri] <- v[i,t]
        if(A!=0){
          e[edg(e,A,t),A] <- ntri
        }
      }
    }
  }

  print("last plot")
  readline(prompt="Press [enter] to continue")
  triad.obj <- new_triad(x,y,v,e)
  plot(triad.obj)
}



#'@title Check which triangles a point lies within
#'@description Returns the index of the triangle which contains the given point
#'@param i index of the point to be located
#'@param x X coordinates of points
#'@param y Y coordinates of points
#'@param v vertices of triangles, columns are triangle number and rows are vertices 1, 2, and 3
#'@param e adjacency of triangles, columns are triangle number and rows are ids of adjacent triangles
#'
#' @export

triloc <- function(i,x,y,v,e){

  cur.tri = 1
  # arc1 is opposite v1
  triangle.found = FALSE
  Px <- x[i]
  Py <- y[i]
  cat("Searching for point ", i, "\n")
  print("Current state of v")
  print(v)
  print("Current state of e")
  print(e)
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

#'@title Implementation of the SWAP subroutine
#'@description Checks to see if triangle P-V2-V1 and V3-V1-V2 need to swap common edge
#'
#'@param x X coordinates of points
#'@param y Y coordinates of points
#'@param P Index number of point P
#'@param V1 Index number of point V1
#'@param V2 Index number of point V2
#'@param V3 Index number of point V3
#'
swap <- function(x,y,P,V1,V2,V3){
  xp <- x[P]
  yp <- y[P]
  x1 <- x[V1]
  y1 <- y[V1]
  x2 <- x[V2]
  y2 <- y[V2]
  x3 <- x[V3]
  y3 <- y[V3]

  x13 <- x1 - x3
  x23 <- x1 - xp
  x1p <- x1 - xp
  x2p <- x2 - xp
  y13 <- y1 - y3
  y23 <- y2 - y3
  y1p <- y1 - yp
  y2p <- y2 - yp

  cosa <- x13*x23 + y13*y23
  cosb <- x2p*x1p + y2p*y1p

  if(cosa >= 0 && cosb>=0){
    return(FALSE)
  }

  if(cosa<0 && cosb<0){
    return(TRUE)
  }

  sina <- x13*y23 - x23*y13
  sinb <- x2p*y1p - x1p*y2p
  sinab <- sina*cosb + sinb*cosa

  if(sinab < 0){
    return(TRUE)
  }

  return(FALSE)
}

#'@title Implementation of the EDG subroutine
#'@description returns which edge of I is adjacent to J
#'
#'@param e List of adjacent triangles
#'@param I index of triangle I
#'@param J index of triangle J
edg <- function(e,I,J){
  for(i in 1:3){
    if(e[i,I] == J){
      return(i)
    }
  }
  print("unexpected edg output")
  return(-1)
}
