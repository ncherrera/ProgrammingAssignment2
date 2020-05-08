## These functions creates a cache of the inverse of matrix.
## Use makeCacheMatrix to create the special matrix object.
## Call cacheSolve to get the inverse of the matrix, if the
## matrix is already calculated, it will return the inverse from the cache.

## This function creates a new matrix object that can cache its inverse 

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


## This function computes the special "matrix" object returned by the 
## makeCacheMatrix. If the matrix is already calculated, this function will get the 
## inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m  
}
