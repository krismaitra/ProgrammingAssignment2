## These functions find the inverse of a matrix and cache it.
## The inverse is returned from the cache on subsequent calls
## unless the matrix itself is changed

## This function takes a matrix as input and creates an object
## containing getters and setters for the matrix and its inverse.
## It also caches the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Create a special "matrix" object that can cache its inverse
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function calculates the inverse of the matrix
## updates it in the cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}