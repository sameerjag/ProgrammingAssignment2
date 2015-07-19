## Put comments here that give an overall description of what your
## functions do
## These functions will create a wrapper around the matrix which allows its inverse to be stored in the wrapper (in the cache)

## Write a short comment describing this function
## This function creates the matrix and the wrapper around it allowing the user to get/set the matrix and get/set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function
## This function looks inside the matrix wrapper to see if the inverse is cached.  If it is, it returns the value
## If it is not cached, it solves the inverse of the matrix, sets it into the matrix wrapper, and returns the result

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'd <-
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}