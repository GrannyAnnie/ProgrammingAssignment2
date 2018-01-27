## makeCacheMatrix creates a special "vector", which is really a list containing 
## a function to set and get the value of the vector, and set and get the value of the mean. 

## This matrix will be used as the input to the subsequent cacheSolve matrix.

makeCacheMatrix <- function(x = matrix()) {
  ## uses <<- operator to assign a value to an object in an environment 
  ## that is different from the current environment. 
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## cacheSolve matrix uses the matrix created in makeCacheMatrix as its input

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message ("getting cached data")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  return(inv)
}
