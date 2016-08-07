## creates a cache object which allows you to cache inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(new) {
    x <<- new
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(get = get, 
       set = set, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Calcualtes or returns cached inverse of a matrix given a cache object. The cache object can be created with makeCacheMatrix() function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  
  inverse
}
