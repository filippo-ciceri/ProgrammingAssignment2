## The purpouse of these functions is to generate a matrix (makeCacheMatrix)
## and to visualize its inverse by calculation or by caching (cacheSolve)

## makeCacheMatrix generates or re-sets the values of a matrix and provides the functions to
## calculate, store in memory and visualize the inverse of the initial input.

makeCacheMatrix <- function(x = matrix(1:4, nrow=2, ncol=2)) {
  inv <- NULL
  set <- function(y= matrix(4:7, nrow=2, ncol=2)) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  compute_inverse <- function(x) {
    inv <<- solve(x)
    inv
  } 
  get_inverse <- function() inv
  list(set = set, get = get,
       compute_inverse = compute_inverse,
       get_inverse = get_inverse)
}


## makeCacheMatrix retrieve from memory the inverse of the matrix obtained by makeCacheMatrix
## or calculate the inverse using the compute_inverse function.

cacheSolve <- function(x, ...) {
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- x$compute_inverse(data, ...)
  inv
}
