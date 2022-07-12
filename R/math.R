#checks if P is left or right of A->B
left.right <- function(Px,Py,Ax,Ay,Bx,By){
  ABx <- Bx-Ax
  ABy <- By-Ay
  APx <- Px-Ax
  APy <- Py-Ay
  cross_prod <- ABx*APy - APx*ABy
  cross_prod
}


