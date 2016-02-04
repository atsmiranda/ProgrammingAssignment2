## Caching the Inverse of a Matrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matinv <- NULL
    set <- function(y) {
        x <<- y
        matinv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) matinv <<- inverse
    getInverse <- function() matinv
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matinv <- x$getInverse()
    if (!is.null(matinv)) {
        message("getting cached data")
        return(matinv)
    }
    mat <- x$get()
    matinv <- solve(mat, ...)
    x$setInverse(matinv)
    matinv
}
