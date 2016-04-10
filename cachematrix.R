## makeCacheMatrix and cacheSolve are functions that store a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    # used '<<- to assign a value to object within different environment
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve will compute the inverse of the matrix created and cached by makeCacheMatrix
cacheSolve <- function(x, ...){
    inv = x$getInv()
    # this loop get the inverse from the cache and skip computing if inverse was calculated
    if(!is.null(inv)){
        message("getting cached data")
      return(inv)
    }
    # calculating inverse if inverse wasn't calculated already
    mat.data = x$get()
    inv = solve(mat.data, ...)
    # setInv is a function that sets the inverse value in the cache
    x$setInv(inv)
    return(inv)
}
