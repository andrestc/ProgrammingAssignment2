## Functions for calculating the inverse of a matrix. Should be used when the the inverse is required 
## multiple times since it has caching capabilities (in a loop, for exemple).
## Usage:
##
## B <- matrix(c(2,3,2,2), nrow=2, ncol=2)
## cm <- makeCacheMatrix(B)
## cacheSolve(cm) # Inverse will be calculated
## cacheSolve(cm) # Inverse gotten from the cache


## Creates a list that represents a matrix with cacheable inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix created with makeCacheMatrix so it can cache its value
## for faster computation (it will be calculated only once).
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
