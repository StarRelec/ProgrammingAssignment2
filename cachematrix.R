#########################################
####### PROGRAMMING ASSIGNMENT 2 ########
#########################################

## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv<<- NULL
  }
  get = function()x
  setinverse = function(matrix) inv <<- matrix
  getinverse = function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function gets the inverse of a matrix. The first time it is calculated and stored.  Subsequent calls just retrieve the previously calculated inverse.

cacheSolve <- function(x, ...) {
  inv = x$getinverse()
    if (!is.null(inv)){
      message("getting cached data")
      return (inv)
    }
  data = x$get()
  inv = solve (data, ...)
  x$setinverse(inv)
  inv
  
  ## Return a matrix that is the inverse of 'x'
}
