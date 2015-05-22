##Makecachematrix is for making a "special" matrix object that can cache it's inverse. 
##set the value of the vector
##get the value of the vector
##set the inverse value
##get the inverse value

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-function(y) {
    x <<- y
    inverse <<- NULL
  }
  get<- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

##cachesolve computes the inverse of the special "matrix", return by makecachematrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data...")
    return(inverse)
  }
  
  args <- x$get()
  inverse <- solve(args)
  x$setInverse(inverse)
  inverse
}