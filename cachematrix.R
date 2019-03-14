## Put comments here that give an overall description of what your functions do
## This function takes a matrix and creates a special vector which is a list containing functions
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
## This function caches the inverse variable
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
