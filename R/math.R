#' @title Constructor for the vector3D class
#'
#' @description An object of vector3D class has three values: x,y,z.
#' @param x1 The x value
#' @param y1 The y value
#' @param z1 The z value
new_vector2D <- function(x1 = 0, y1 = 0){
  stopifnot(is.numeric(x1) && is.numeric(y1))
  value <- list(x = x1, y = y1)
  class(value) = "vector2D"
  value
}


#' @title Cross Product of two vectors
#'
#' @description Calculates the Cross Product of two vector2D objects and returns the Z value of the resulting vector
#' @param a First vector
#' @param b Second vector
cross_product <- function(a,b){
  stopifnot(class(a)=="vector2D" && class(b)=="vector2D")
  #Z axis of the two vectors are guaranteed to be zero
  #resultant vector will only be along Z axis
  c_z <- (a$x*b$y - b$x*a$y)
  c_z
}

#' @title Checks if a point lies strictly to the left of a line segment
#'
#' @description returns TRUE if point c lies strictly to the left of a->b vector.
#' @param a initial point of a->b vector
#' @param b final point of a->b vector
#' @param c Point whose relative position is to be calculated
is_left_of <- function(a,b,c){
  stopifnot(class(a)=="vector2D" && class(b)=="vector2D" && class(c)=="vector2D")

  #a->b vector
  ab <- new_vector2D(b$x-a$x,b$y-a$y)

  #a->c vector
  ac <- new_vector2D(c$x-a$x,c$y-a$y)

  cross_product(ab,ac) > 0
}


#' @title print() function for vector3D class
#'
#' @description An S3 method using the print() generic
#'
#' @param x A vector3D object
#' @param ... Additional arguments
print.vector3D <- function(x, ...){
  paste("X: ",x$x,", Y: ",x$y,", Z: ",x$z)
}

