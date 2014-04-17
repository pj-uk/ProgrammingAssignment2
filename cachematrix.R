## Programming Assignment 2 (peer assessed)
## Provides functions for managing caching of matrix inverses.
## Assumes matrix is invertible.

## makeCacheMatrix returns a cache object which we can use for querying the cache and computing
## new cache entries (ie. inverses).
## usage: 
## cache <- makeCacheMatrix(x)
## cache$setsolve(x)    - sets inverse in cache
## cache$getsolve(x)    - looks in cache for previously computed inverse.

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


## Returns inverse of given matrix, grabbing result from the cache instead of computing if possible

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