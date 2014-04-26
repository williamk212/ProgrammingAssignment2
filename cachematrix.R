## R Programming class by RPeng
##
## Assignment 2
##
## @author william.keung
## email: williamk212 [at] gmail [dot] com
##

## The makeCacheMatrix function is simply a container that
## holds references to:
## * the original matrix
## * the inverse of the original matrix
## * methods 'getsolve', 'setsolve' to retrieve cache
makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv_matrix <<- solve
  # if called, just return inversed matrix
  getsolve <- function() inv_matrix
  list(set = set, get = get, setsolve = setsolve,
       getsolve = getsolve)
}


## The 'cacheSolve' function gets and sets the cache. 
## If there is no inversed matrix set in the cache, the function
## will solve and calculate the inverse matrix and set the cache.
##
## The argument to this function should be the container 
## function type defined above: 'makeCacheMatrix'
##
## Example:
## > cached_matrix <- makeCacheMatrix(simple_matrix)
## > cacheSolve(cached_matrix)
##
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getsolve()
  if (!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$setsolve(inv_matrix)
  inv_matrix
}
