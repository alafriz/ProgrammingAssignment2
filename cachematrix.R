## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Method to set a matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Get matrix
    get <- function() x
    
    # Set a solution
    setsolve <- function(solve) m <<- solve
    
    # Get the solution
    getsolve <- function() m
    
    # Print details
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Check if cached, if it is, return it.
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Not cached, get matrix, solve it, set solution, return solution
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
