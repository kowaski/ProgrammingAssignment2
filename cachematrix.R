## Set of functions for a generating a wrapper function over a matrix
## and retrieving it, if its in cache, else computing it and storing in cache

## Generates a *special* variable, which is a list containing 4 operations
## for storing a matrix, getting a matrix, setting inverse, getting inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Takes a *special* variable as input, if its inverse is already calculated, return
## the cached value, else, compute inverse and cache it using setinverse()

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
