## The purpose of the following functions is to support the caching of
## of the inverse of a matrix, which can be a costly calculation.
## 

## makeCacheMatrix takes a matrix as its input (assumed to be N x N [invertible])
## and creates a special list that makes available get and set functions for that matrix
## as well as get and set functions for the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(mean) inv <<- mean
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve makes use of the special object created by makeCacheMatrix
## it checks to see if the inverse of the matrix has already been calculated
## in which case it avoids the costly computation and returns the cached
## value of the inverse.  If there is no cached value, then it calculates
## the inverse, sets it in the object and returns the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  ## if inv is not null, then we just use the cached version
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise, we compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
