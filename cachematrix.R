## This is a matrix inverse function.
## So in my first section of code, x will be my input.
## Then x will be validated with cache, if the inverse value is available in cache then it will return inverse of the matrix.
## Otherwise, it will return the inverse matrix(Using Solve function)
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(matt) z <<- matt
  getinvmatrix <- function() z
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}

cacheSolve <- function(x, ...) {
  z <- x$getinvmatrix()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinvmatrix(z)
  z
}