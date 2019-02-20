## This couple of functions make it possible to cache inverse of matrices.
## makeCacheMatrix creates a matrix-like object with "methods" set, get,
## setinverse and getinverse which cache the inverse matrix and
## cacheSolve computes the inverse of a matrix-like object by first
## looking for a cached version of it.

## makeCacheMatrix ----
## This function creates a special "matrix" object that's able to
## cache its inverse.  Returns a list with elements:
## set -- a function to set the (matrix) value of x (won't do any checks)
## get -- a function to get the matrix value of x
## setinv - a function to set the inverse of x 
## getinv - a function to get the inverse of x

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y){
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve ----
## Finds the inverse of matrix--like object formed by makeCacheMatrix
## if this inverse is already computed, it only gets it; otherwise it computes it
## a message indicates if the inverse was cached

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get
  inv <- solve(matrix, ....)
  x$setinv(inv)
  inv
}
