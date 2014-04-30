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
    ## Return a list of functions which can then be called using the cache object
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve returns inverse of given matrix, grabbing result from the cache instead
## of computing, if possible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    ## If m is not null then we have successfully fetched result from the cache, so we return it.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If we reach this point, result was not cached so we compute it and add it to cache.
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    ## Return m
    m
}