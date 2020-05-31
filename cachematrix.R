# Title     : Cache Matrix
# Objective : Write function to cache matrix inverse.
# Created by: Steven
# Created on: 5/30/2020

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() {
        inv
    }
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message('getting cached inverse Matrix')
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(x, ...)
    x$setInverse(inverse)
    inverse
}

