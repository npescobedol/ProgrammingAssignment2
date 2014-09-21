## The first function, makeCachematrix create a special matrix, wich is really 
## a list cointaining a function to
## set the matrix
## get the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  setm <- function(y) {
    x <<- y
    minv <<- NULL
  }
  getm <- function() x
  setminverse <- function(minverse) minv <<- minverse
  getminverse <- function() minv
  list(setm=setm, getm=getm, setminverse=setminverse, getminverse=getminverse)
}

## The function cacheSolve calculates the inverse of a Matrix created with the makeCacheMatrix
## function, first the function checks if the inverse has been calculated using the getminverse function. 
## If so, it gets the inverse from the cache  and skips the computation.

## If the inverse has not been calculated, the function calculates the inverse of the matrix 
## and sets this in the cache using the setminverse function.

cacheSolve <- function(x, ...) {
  minv <- x$getminverse()
  if(!is.null(minv)) {
    message("getting cached data.")
    return(minv)
  }
  data <- x$getm()
  minv <- solve(data)
  x$setminverse(minv)
  minv
}
