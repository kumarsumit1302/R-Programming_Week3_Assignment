
# Below code is for generating cache
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

#This code is for finding Inverse of a matrix, If the matrix is already in cache then it return cached inverse

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

# examples given here is to chech the accuracy of above code

matrix_example <- makeCacheMatrix(matrix(c(5, 3, -7, 0, 3, 1, 5, 4, 0),3,3))
cacheSolve(matrix_example)
