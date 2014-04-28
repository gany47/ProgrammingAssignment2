## makeCacheMatrix will hold and provide the input matrix for inverse computation
## This function will also cache the inverse of the matrix from the cacheSolve
## function which can be retrieved at any point of time saving the computation time.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get used to access the matrix for which the inverse needs to be computed
        get <- function() x
        ##setinverse caches the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## cacheSolve function returns the inverse of the matrix using the solve() function
## if the inverse was not previously calculated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
