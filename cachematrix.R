## The following functions were designed to save
## computational time for inverse matrix calculations.

## We expect the first function below to create a
## special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }

	get <- function() x
    setInverse <- function(inverse) inv_mat <<- inverse
    getInverse <- function() inv_mat

	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Below, we expect the next function to compute
## the inverse of the special matrix returned by
## the preceding function (makeCacheMatrix)

## The "cacheSolve" retrieves the inverse from the cache,
## if inverse has already been calculated
## and the matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv_x <- x$getInverse()
        if (!is.null(inv_x)) {
                message("Getting some cached data")
                return(inv_x)
        }
        mat <- x$get()
        inv_x <- solve(mat, ...)
        x$setInverse(inv_x)
        inv_x
}
