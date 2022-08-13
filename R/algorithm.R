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
#'
#'@export
new_triad <- function(x,y,v){
  triad.obj <- list(
    x=x,
    y=y,
    v=v)
  class(triad.obj) <- "triad"
  triad.obj
}


#'@title Implementation of the DELTRI subroutine
#'@description Calculates the Delaunay Triangulation of the given set of points.
#'@param x stores the x coordinates of the points. If y is NULL, it is treated as a data.frame
#'@param y stores the Y coordinates of the points
#'@param maxrange if TRUE, normalisation is done with max(x range, y range) for both x and y values, otherwise uses x/y range for x/y normalisation
#'
#'@export
del_tri <- function(x,y=NULL, maxrange=TRUE){
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
  norm_y <- (y_p - y_min)/y_div
  v <- delaun(norm_x, norm_y)
  triad.obj <- new_triad(x_p, y_p, v)
  return(triad.obj)
}

#'@title Implementation of the DELAUN subroutine
#'@description Returns a Delaunay Triangulation but with normalised points
#'@param norm_x Normalised X coordinates of points
#'@param norm_y NormalisedY coordinates of points
#'
#'@export
delaun <- function(norm_x, norm_y){
  x <- norm_x
  y <- norm_y
  numpts <- length(x)
  x <- c(x,0,0,0)
  y <- c(y,0,0,0)

  #define vertex and adjacency list
  v <- matrix(nrow=3)
  v1 <- numpts+1
  v2 <- numpts+2
  v3 <- numpts+3
  v[1,1] <- v1
  v[2,1] <- v2
  v[3,1] <- v3

  e <- matrix(nrow=3)
  e[1,1] <- 0
  e[2,1] <- 0
  e[3,1] <- 0

  #set coords of supertriangle
  x[v1] <- -100
  x[v2] <- 100
  x[v3] <- 0

  y[v1] <- -100
  y[v2] <- -100
  y[v3] <- 100

  #loop over each point
  numtri <- 1
  tstack <- c()
  for(i in 1:numpts){
    xp <- x[i]
    yp <- y[i]
    #locate triangle in which point lies
    t <- triloc(i,x,y,v,e)

    #create new vertex and adjacency list for triangle t
    a <- e[1,t]
    b <- e[2,t]
    c <- e[3,t]

    v1 <- v[1,t]
    v2 <- v[2,t]
    v3 <- v[3,t]
    v[1,t] <- i
    v[2,t] <- v1
    v[3,t] <- v2
    e[1,t] <- numtri+2
    e[2,t] <- a
    e[3,t] <- numtri+1

    #create new triangles
    newcol <- c(0,0,0)
    v <- cbind(v,newcol)
    v <- cbind(v,newcol)
    e <- cbind(e,newcol)
    e <- cbind(e,newcol)

    numtri <- numtri+1
    v[1,numtri] <- i
    v[2,numtri] <- v2
    v[3,numtri] <- v3
    e[1,numtri] <- t
    e[2,numtri] <- b
    e[3,numtri] <- numtri+1

    numtri <- numtri+1
    v[1,numtri] <- i
    v[2,numtri] <- v3
    v[3,numtri] <- v1
    e[1,numtri] <- numtri-1
    e[2,numtri] <- c
    e[3,numtri] <- t
    #put each edge of triangle t on stack
    #store triangles on left side of each edge
    #update adjacency lists for adjacent triangles
    #adjacency list for element a does not need to be updated
    if(a!=0){
      tstack <- c(t, tstack)
    }
    if(b!=0){
      e[edg(b,t,e),b] <- numtri-1
      tstack <- c(numtri-1,tstack)

    }
    if(c!=0){
      e[edg(c,t,e),c] <- numtri
      tstack <- c(numtri,tstack)
    }
    #loop while stack is not empty
    while(length(tstack) > 0){
      l <- tstack[1]
      tstack <- tstack[-1]
      r <- e[2,l]
      #check if new point is in circumcircle for triangle r
      erl <- edg(r,l,e)
      era <- erl %% 3 + 1
      erb <- era %% 3 + 1
      v1 <- v[erl,r]
      v2 <- v[era,r]
      v3 <- v[erb,r]
      swap.true <- swap(x[v1],y[v1],
                        x[v2],y[v2],
                        x[v3],y[v3],
                        xp,yp)
      if(swap.true){
        #new point is inside circumcircle for triangle r
        #swap diagonal for convex quad formed by P-V2-V3-V1
        a <- e[era,r]
        b <- e[erb,r]
        c <- e[3,l]
        # update vertex amd adjacency list for triangle l
        v[3,l] <- v3
        e[2,l] <- a
        e[3,l] <- r
        #update vertex and adjacency list for triangle r
        v[1,r] <- i
        v[2,r] <- v3
        v[3,r] <- v1
        e[1,r] <- l
        e[2,r] <- b
        e[3,r] <- c

        #put edges l-a and r-b on stack
        #update adjacency lists for triangles a and c
        if(a!=0){
          e[edg(a,r,e),a] <- l
          tstack <- c(l,tstack)
        }
        if(b!=0){
          tstack <- c(r,tstack)
        }
        if(c!=0){
          e[edg(c,l,e),c] <- r
        }
      }
    }
  }

  #check consistency of triangulation
  if(numtri!= 2*numpts+1){
    print("Incorrect number of triangles formed")
  }

  #numtri <- length(v)/3
  t <- 1
  while(t <= numtri){
    if(v[1,t] > numpts || v[2,t] > numpts || v[3,t] > numpts){
      v <- v[,-t]
      numtri <- numtri-1
    }
    else{
     t<-t+1
    }
  }
  #x <- x[c(-numpts-1, -numpts-2, -numpts-3)]
  #y <- y[c(-numpts-1, -numpts-2, -numpts-3)]

  #triad.obj <- new_triad(x,y,v,e)

  return(v)
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
#'@param x1 X coordinate of V1
#'@param y1 Y coordinate of V1
#'@param x2 X coordinate of V2
#'@param y2 Y coordinate of V2
#'@param x3 X coordinate of V3
#'@param y3 Y coordinate of V3
#'@param xp X coordinate of P
#'@param yp Y coordinate of P
#'@export
swap <- function(x1, y1, x2, y2, x3, y3, xp, yp){

  #angle between vector p->1 and p->2
  x1p <- x1 - xp
  y1p <- y1 - yp
  x2p <- x2 - xp
  y2p <- y2 - yp
  dot.1p.2p <- x1p*x2p + y1p*y2p
  mod1p <- sqrt(x1p*x1p + y1p*y1p)
  mod2p <- sqrt(x2p*x2p + y2p*y2p)
  cos.alpha <- dot.1p.2p / (mod1p * mod2p)
  if(cos.alpha > 1){
    cos.alpha <- 1
  }
  if(cos.alpha < -1){
    cos.alpha <- -1
  }
  alpha <- acos(cos.alpha)


  #angle between vector 3->1 and 3->2
  x13 <- x1 - x3
  y13 <- y1 - y3
  x23 <- x2 - x3
  y23 <- y2 - y3
  dot.13.23 <- x13*x23 + y13*y23
  mod13 <- sqrt(x13*x13 + y13*y13)
  mod23 <- sqrt(x23*x23 + y23*y23)
  cos.gamma <- dot.13.23/(mod13*mod23)
  if(cos.gamma > 1){
    cos.gamma <- 1
  }
  if(cos.gamma < -1){
    cos.gamma <- -1
  }
  gamma <- acos(cos.gamma)
  return((alpha+gamma) > pi)
}

#'@title Implementation of the EDG subroutine
#'@description returns which edge of I is adjacent to J
#'
#'@param e List of adjacent triangles
#'@param I index of triangle I
#'@param J index of triangle J
#'@export
edg <- function(I,J,e){
  for(i in 1:3){
    if(e[i,I] == J){
      return(i)
    }
  }
  print("unexpected edg output")
  return(-1)
}
