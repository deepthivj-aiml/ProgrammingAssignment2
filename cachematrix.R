makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse <<- inverse
        getInverse <- function() inverse 
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse )) {
                message("getting cached data")
                return(inverse )
        }
        matrix1 <- x$get()
        inverse <- solve(matrix1 , ...)
        x$setInverse(inverse )
        inverse 
}