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
    cat("L: ",L)
    cat(" R:",R,"\n" )
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
  t <- 1
  while(t <= length(v)/3){
    cat("t= ", t, " len= ",length(v)/3,"\n")
    if(v[1,t]>n || v[2,t] > n || v[3,t] > n){
      v <- v[,-t]
    }
    else{
      t <- t+1
    }
  }
  x <- x[c(-n-1,-n-2,-n-3)]
  y <- y[c(-n-1,-n-2,-n-3)]
  print("last plot")
  readline(prompt="Press [enter] to continue")
  print(v)
  triad.obj <- new_triad(x,y,v,e)
  plot(triad.obj)
}


delaun.remove <- function(){

}

delaun.swap <- function(){

}
delaun.triangulate <- function(){

}
