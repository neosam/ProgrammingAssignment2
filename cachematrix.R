## Matrix inverse cache
##
## Introduce a new object which holds a matrix and optinally
## the inverse of the matrix.  The inverse is only calculated
## once on the first time it is requested.  If the inverse matrix
## is requested again, it will only return the cache.


## An object which holds a matrix and optionally its inverse
## matrix.  Should be used in combination with cacheSolve.
## The cached inverse will be removed once the matrix is replaced
## by the set function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(new.inv) inv <<- new.inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Takes object created by 'makeCacheMatrix' and returns
## its inverse.  It will cache the inverse matrix to improve
## performance.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
