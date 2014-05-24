##
## To avoid heavy load on multiple computation of inverse the functions 
## makeCacheMatrix and cacheSolve allow to compute the inverse once and
## reuse multiple times.

#######################################################
# fn: makeCacheMatrix ( matrix() )
# Computes the inverse of an, invertible,given matrix
# and caches the reversed matrix.
#
# Args:
#   x: a matrix, supposed to be invertible.
#
# Returns:
#   a "special" matrix cached as a list of functions 
#   manage matrix's values and the values of the inverse
#   matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(Solve) m <<- Solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

#######################################################
# fn: cacheSolve ( makeCacheMatrix() )
# Computes the inverse of an, invertible, given matrix
# passed to the makeCacheMatrix function if not cached 
# and gets the reversed matrix from cache if yet calculated.
#
# Args:
#   x:  a "special" matrix returned by makeCacheMatrix.
#
# Returns:
#   the inverse of the matrix passed to makeCacheMatrix
#
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
