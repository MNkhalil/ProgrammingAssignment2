## we will create two function s that allows us to cashe the inverse of a matrix

## this function is going to create a special matrix,by providing a list that can:
## set and get a matrix,set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  Invers<-NULL
  setM<- function(m){
    x<<-m
    Invers<-NULL
  }
  getM<- function() x
  setInverse <-function(inv){
    Invers<<-inv
  }
  getInverse <- function() Invers
  list(setM=setM, getM=getM, setInverse=setInverse, getInverse=getInverse)        
}


## this function looks if the inverse of a matrix is cashed and return it
## otherwise it calculate the inverse of the matrix usin the function solve() 
## and it caches it using the function setInverse()

cacheSolve <- function(x, ...) {
  Invers <- x$getInverse()
  if(!is.null(Invers)){
    message("getting the cashed data")
    return(Invers)
  }
  data <- x$getM()
  Invers <- solve(x,...)
  x$setInverse(Invers)
  Invers
}
