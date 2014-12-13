## These are functions to cache and retrieve a matrix and its inverse

## Creates a list which stores a matrix and its inverse in the global environment (cache)
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    # update an existing matrix in the cache
    set <- function(y) {
            x <<- y
            m <<- NULL
        }
    # retrieve a matrix from cache
    get <- function() x
    # cache the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    # retrieve the inverse of the matrix from cache
    getinverse <- function() m
    # list that stores everything
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the inverse of the matrix from cache if it exists, otherwise calculates it
## and sends it to the function that saves it in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
        # if cached version exists return cached inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # if not, calculate and return inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
