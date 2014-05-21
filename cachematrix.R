## Author: Bernhard Thoni
## Date: 21.05.2014
##
## This function makeCacheMatrix creates "a special object" which caches the inverse-matrix (im) of a matrix (m) which it receives as a function argument
## it has both getters and setters for the matrix itself, as well as for the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
  im <- NULL
  set <- function(y) {
    m <<- y
    im <<- NULL
  }
  get <- function() m
  setimatrix <- function(imatrix) im <<- imatrix
  getimatrix <- function() im
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)  
}


## this function cacheSolve gets as an argument a matrix, and looks up via makeCacheMatrix, wheter the inverse matrix already was computed.
## if so, the cached version is pulled, otherwise it computes via solve() the inverse matrix, and "writes" it in the cache

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- m$getimatrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- m$get()
  im <- solve(data, ...)
  m$setimatrix(im)
  im
}
