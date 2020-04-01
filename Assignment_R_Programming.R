makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <-  x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

matrix_example <- makeCacheMatrix(matrix(c(5, 3, -7, 0, 3, 1, 5, 4, 0),3,3))
cacheSolve(matrix_example)
