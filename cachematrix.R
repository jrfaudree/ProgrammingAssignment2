## this function takes a square, invertible matrix as input
## and outputs a list of functions: set, get, setinverse, and getinverse

## the four functions in the list do the following:
## set: allows user to set (change) the matrix
## get: fetches the (possibly new) matrix
## setinverse: sets the inverse of a function
## getinverse: fetches the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the output of makeCacheMatrix as input
## and outputs the inverse of the matrix x.

## The function will first check to see if the inverse has been
## calculated (and cached).

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
