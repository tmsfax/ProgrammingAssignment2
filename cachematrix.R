# This file defines a special kind of matrix that can cache its
# inverse. You can use it in situations where it is difficult or
# inconvenient to keep track of the inverse and when it might change.


# This function creates an object that can hold both a matrix and its
# cached inverse. The object contains functions to get and set both
# the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {

    # Initialize the cached inverse to NULL.
    inverse <- NULL
    
    # This function lets the caller replace the matrix with a new one.
    # Since the matrix may be different, it also deletes any cached inverse.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # Gets a copy of the matrix.
    get <- function() x
    
    # Sets the cached inverse.
    setinverse <- function(inverse) inverse <<- inverse
    
    # Gets a copy of the cached inverse.
    # Note that it will be NULL if there is no cached inverse.
    getinverse <- function() inverse
    
    # Return a matrix that can cache its inverse.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# This function will return the inverse of a matrix.
# Because computing an inverse can be expensive, it will cache the 
#    inverse for future use.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # First check to see if the inverse is cached.
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # The inverse was not cached, so we now retrieve the matrix,
    # compute its inverse, and save it as the cached inverse.
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    
    # Return the same matrix that was saved as the cached inverse.
    inverse
}
