## Creates a cache matrix where the calculation for the inverse of a matrix is cached.
## The matrix must be a square matrix.

## makeCacheMatrix() - Creates a cache matrix from a regular square matrix with the following methods:
## get() - gets the matrix
## set() - sets the matrix
## setinverse - sets the inverse matrix of the matrix
## getinverse - gets the (cached) value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inv_) {
    inv <<- inv_
  } 
  getinverse <- function() {
    inv
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() - Takes a cache matrix and returns the cached value of the inverse matrix
##                if the value exists.  If the value does not exist, calculate
##                the value of the inverse matrix and cache it then return the inverse of matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
