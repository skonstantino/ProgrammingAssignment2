## makeCacheMatrix creates a "matrix" object that can cache its inverse
## It is a list containing a function to: set the matrix, get the matrix, set the inverse of a matrix, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      ## inverse is set to NULL as a placeholder
      ## set <- function(y) is defining a new function to set vector 'x' to vector 'y'
      #inv is set to NULL
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}
## get function is returning the vector 'x' 
## setInverse is setting the inverse function
## getInverse returns the inverse of the function

## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated, then cachesolve should retrieve the inverse from the cache, and skip the computation 
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
         inv <- x$getInverse()
  if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
