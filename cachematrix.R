## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
