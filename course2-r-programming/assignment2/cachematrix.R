## This file contains two functions that allow the client to cache the
## inverse of a matrix.
##
## Example usage:
##   cache.matrix <- makeCacheMatrix(my.matrix)
##   cacheSolve(cache.matrix) # Computes and returns inverse of my.matrix
##   cacheSolve(cache.matrix) # Returns cached inverse of my.matrix
##   cache.matrix$set(new.matrix) # Update the matrix
##   cacheSolve(cache.matrix) # Computes and returns inverse of the updated matrix

makeCacheMatrix <- function(my.matrix = matrix()) {
  # Create and return a 'cachematrix' object. This object is a list that
  # encapsulates the matrix and its inverse via these utility functions: set,
  # get, setinv, and getinv.

  my.inv <- NULL
  set <- function(new.matrix) {
    my.matrix <<- new.matrix
    my.inv <<- NULL
  }
  get <- function() my.matrix
  setinv <- function(new.inv) my.inv <<- new.inv
  getinv <- function() my.inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(cache.matrix, ...) {
  # Return a matrix that is the inverse of cache.matrix. If the inverse
  # of cache.matrix has already been computed and cached, return this
  # cached inverse.

  inv <- cache.matrix$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- cache.matrix$get()
  inv <- solve(data, ...)
  cache.matrix$setinv(inv)
  inv
}
