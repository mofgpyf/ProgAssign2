# creates a special "matrix" object that can cache its inverse

# the <<- operator which can be used to assign a value to an object 
# in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  InverseMat <- NULL
  set <- function(y){
      x <<- y
      InverseMat <- NULL
  }
  get <- function() x
  setInverse <- function(inverse) InverseMat <<- inverse
  getInverse <- function() InverseMat
  list (set <- set, get <- get, setInverse <- setInverse, getInverse <- getInverse)
}


# Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  InverseMat <- x$getInverse()
  if(!is.null(InverseMat)){
      message("getting cached data")
      return(InverseMat)
  }
  data <- x$get()
  InverseMat <- solve(data, ...)
  x$setInverse(InverseMat)
  return(InverseMat)
}
