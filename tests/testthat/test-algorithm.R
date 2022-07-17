x <- c(0,1,-2,3,0,0,1,1,-1,0)
y <- c(1,-2,-2,2,6,-1,0,2,0,0)
v <- matrix(nrow=3, ncol = 4)
e <- matrix(nrow=3, ncol = 4)
v[1,] <- c(1,1,1,1)
v[2,] <- c(3,2,4,5)
v[3,] <- c(2,4,5,3)
e[1,] <- c(4,1,2,3)
e[2,] <- c(0,0,0,0)
e[3,] <- c(2,3,4,1)

test_that("triloc tests", {
  expect_equal(triloc(6,x,y,v,e), 1)
  expect_equal(triloc(7,x,y,v,e), 2)
  expect_equal(triloc(8,x,y,v,e), 3)
  expect_equal(triloc(9,x,y,v,e), 4)
  expect_equal(triloc(10,x,y,v,e), 1)
})



Bx <- 1
By <- 1
Ax <- -1
Ay <- -1
test_that("left.right tests", {
  expect_equal(left.right(0,0,Ax,Ay,Bx,By), 0)
  expect_equal(left.right(0,1,Ax,Ay,Bx,By), 2)
  expect_equal(left.right(1,0,Ax,Ay,Bx,By), -2)
})
