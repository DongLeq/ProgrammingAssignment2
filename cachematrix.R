## These two following functions help to cache the inverse of a matrix instead of computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              Inverse <- NULL
              set <- function(y) {
                x <<- y
                Inverse <<- NULL
              }
              get <- function() x
              setsolve <- function(solve) Inverse <<- solve
              getsolve <- function() Inverse
              list(set = set, get = get,
                   setsolve = setsolve,
                   getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by by makeCacheMatrix above or
## retrieve it if the inverse has already been calculated (and the matrix has not changed). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inverse <- x$getsolve()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data, ...)
        x$setsolve(Inverse)
        Inverse
}
