#### Pair of functions that cache the inverse of a matrix (assuming that the 
#### matrix supplied is invertible).
#### The first function caches the inverse of the supplied matrix.
#### The second function computes the inverse of the supplied matrix.


## Function that creates a special "matrix" object that can cache its inverse.
## Input should be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinversemx <- function(inversemx) i <<- inversemx
        getinversemx <- function() i
        list(set = set, get = get,
             setinversemx = setinversemx,
             getinversemx = getinversemx)
}


## Function that computes the inverse of the special "matrix" returned by the
## makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve function retrieves the inverse from the cache.
## Input should be the return of the makeCacheMatrix function (or the object 
## where it is stored), plus (if applied) additional arguments passed to the 
## solve function (computes the inverse of a square matrix).

cacheSolve <- function(x, ...) {
        i <- x$getinversemx()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinversemx(i)
        i
}