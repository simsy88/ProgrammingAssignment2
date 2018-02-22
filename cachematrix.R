## Functions to calculates the inverse of a matrix
## and to cache the result for future use so it doesn't have to be recalculated
## as calculating a matrix inverse can be computationally costly


## makeCacheMatrix sets up the list of functions

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## cacheSolve uses the cache if available, and calculates if not

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     ## Return a matrix that is the inverse of 'x'
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
