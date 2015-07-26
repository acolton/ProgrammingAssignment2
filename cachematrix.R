# The following functions cache the inverse of a matrix

# list of four functions that set/stor and retrieve a matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set.matrix <- function(y){
    x <<- Y
    inverse <<- NULL
  }
  get.matrix <- function() x
  set.inverse <- function(inverseMatrix) inverse <<- inverseMatrix
  get.inverse <- function() inverse
  list(set.matrix = set.matrix, get.matrix = get.matrix, set.inverse = set.inverse, get.inverse = get.inverse)
}


# calls the get.inverse function, if it is null it calculates the inverse and returns it with a message
# if the matrix has not changed (i.e., the inverse is not null, it retrieves the cached inverse from the get.inverse function)
cacheSolve <- function(x, ...) {
  inverse <- x$get.inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get.matrix()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}
