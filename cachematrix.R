# Description: 
#   Make a object that retain a cached version of the inverse of the matrix
# Args: x = must be a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(i) inverse <<- i
  get_inverse <- function() inverse
  list(
    set = set, 
    get = get,
    set_inverse = set_inverse,
    get_inverse = get_inverse)
}


# Description: 
#   If the inverse of the matrix has been cached, return the cached value of the inverse of the matrix x
#   else calculate the inverse, cache the result in x and return the inverse
# Args: x = must be a object create with makeCacheMatrix(x)
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(x$get(), ...)
  x$set_inverse(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
}
