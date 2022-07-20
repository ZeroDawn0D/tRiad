#'@title Cross Product of AB and AP
#'
#'@description Calculates cross product of AB and AP to check if P is left or right of AB
#'
#'@param Px X component of point P
#'@param Py Y component of point P
#'@param Ax X component of point A
#'@param Ay Y component of point A
#'@param Bx X component of point B
#'@param By Y component of point B
#'
#'@export
left.right <- function(Px,Py,Ax,Ay,Bx,By){
  ABx <- Bx-Ax
  ABy <- By-Ay
  APx <- Px-Ax
  APy <- Py-Ay
  cross_prod <- ABx*APy - APx*ABy
  cross_prod
}
