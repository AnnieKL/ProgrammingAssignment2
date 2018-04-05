# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse_mat) inverse <<- inverse_mat
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" turned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the
# cacheSolve function should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("retrieving data")
                return(inverse)
        }
        matr <- x$get()
        inverse <- solve(matr, ...)
        x$setInverse(inverse)
        inverse
}
