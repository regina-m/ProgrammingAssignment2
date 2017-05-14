## These functions will calculate & cache the inverse of a matrix.

## This function will create a null variable which can then be assigned the value
## of the input matrix. Then it will set another variable to store the inverse of
## the matrix and calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) m <<- solve
  
  getinv <- function() m
  
  list("set" = set, "get" = get,
       "setinv" = setinv,
       "getinv" = getinv)
}


## This function will calculate the inverse of the matrix generated in the 
## makeCacheMatrix function. If the matrix is unchanged and the inverse was 
## previously calculateed, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m
}
