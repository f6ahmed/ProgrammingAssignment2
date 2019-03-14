## These 2 functions can cache matrix inverse computation to save time

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the matrix inverse
## get the value of the matrix inverse
#
makeCacheMatrix <- function(x = matrix()) {
inverse_var <- NULL
     set <- function(y) {
          x <<- y
          inverse_var <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inverse_var <<- inverse
     getinverse <- function() inverse_var
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}
#
## ## This function calculates the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse was already calculated, then the cachesolve retrieves the 
## inverse from the cache.
#
cacheSolve <- function(x, ...) {
     inverse_var <- x$getinverse()
     if(!is.null(inverse_var)) {
          message("getting cached data")
          return(inverse_var)
     }
     data <- x$get()
     inverse_var <- solve(data, ...)
     x$setinverse(inverse_var)
     inverse_var
}
