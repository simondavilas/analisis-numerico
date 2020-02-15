#Codigo Realizado por: Gabriel Gomez Corredor
#                      Simon Davila
#                      Juan Pablo Mendez

#PUNTO 5 DOCUMENTO 2

f <- function(x){
    log(x+2)
  }
f(0)
g <- function(x){
    sin(x)
  }
t <-1e-8
q = function(x){
  log(x+2)-sin(x)
  }
teo = uniroot(q, c(-1.8,-1), tol = 1e-9) 



plot(f, xlim = c(-2,2), ylim=c(-2,2), ylab = "f(x)", col = "yellow")
par(new = TRUE)

plot(g, xlim = c(-2,2), ylim=c(-2,2), ylab = "g(x)", col = "purple")

plot(q, xlim = c(-2,2), ylim=c(-2,2), ylab = "f(x) - g(x)", col = "green")
par(new = TRUE)

abline(0,0, col = "blue")

  a <- 5
  b <- 1
  vec <- c(0)
  vec[1] <- a
  vec[2] <- b
  ind <- 3
  em1 <- c() 
  em1_1 <- c()
  teo2<-teo$root
  while(abs(teo2-vec[ind-2]) > t){
    em1[ind-2]=abs(teo2-vec[ind-2])
    vec[ind]= vec[i-1]-((q(vec[ind-1])*(vec[ind-1]-vec[ind-2]))/(q(vec[ind-1])-q(vec[ind-2])))
    ind = ind+1
  }
  
  ind=1
  while(i<length(em1)+1){
    em1_1[i]=esm1[i+1]
    ind = ind+1
  }
  plot(em1,em1_1, type="l")

  n_iteraciones = length(vec)

  vec2 <- c(0)
  em2 <- c(0)
  em2_1 <- c(0)
  vec2[1] <- a
  vec2[2] <- b
  ind <- 3
  while(abs(teo2-vec2[ind]) > t){
    em2[ind-2]=abs(teo2-vec[ind-2])
    vec2[ind]= vec2[ind-1]-((vec2[ind-1]-vec2[ind-2])/(q(vec2[ind-1])-q(vec2[ind-2])))
    ind = ind+1
  }

  ind=1
  while(ind<length(em2)+1){
    em2_1[ind]=em2[ind+1]
    ind = ind+1
  }

  plot(em2,em2_1, type ="l",col ="red")
  
  tabla = data.frame(em1,em1_1,em2,em2_1)
  tabla