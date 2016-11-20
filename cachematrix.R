## This pair of functions will cache the inverse of a matrix.

## The first function, makeCacheMatrix, will build a set of functions and
## return those functions within a list to the parent environment.  The list
## will contain four functions: setMatrix(), getMatrix(), setInverse(), and
## getInverse().  It will also contact two data objects, x and m.
## This list will be used as the input for the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}

## This second function, cacheSolve, requires an input argument of type 
## makeCacheMatrix().  It will compute and return a matrix that is the inverse
## of 'x'.  If the inverse has already been calculated (and the matrix has not
## changed), then cacheSolve should simply retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        return(m)
}