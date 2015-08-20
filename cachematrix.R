# makeCcheMatrix is a function to set a matrix and get a matrix
# cacheSolve is matrix used to solve the inverse of imput matrix

# makeCacheMatrix contains four functions: set, setinverse, get, getinverse
makeCacheMatrix <- function(x = matrix()) {
    inverse_the_mat  <- NULL
    set = function(y){                                                            # Set matrix
        x <<- y
        inverse_the_mat <<- NULL
    }
    get <- function() x                                                           # Get matrix
    setinverse <- function(inverse) inverse_the_mat <- inverse                    # Set inverse of matrix
    getinverse <- function() inverse_the_mat                                      # Get inverse of matrix
    list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


# cacheSolve function calculate the inverse of matrix

cacheSolve <- function(x, ...) {
    inverse_the_mat <- x$getinverse()                         # Know the property of cache
    if(!is.null(inverse_the_mat)) {                           # check the cache data whether it is empty
        message("getting cached data")
        return(inv)                                       # If it isn't empty, return the cache
    }
    mat <- x$get()                                            # Get the matrix
    inverse_the_mat <- solve(mat)                             # Calculate the inverse matrix
    x$setinverse(inverse_the_mat)                             # Set the inverse matrix
    inverse_the_mat                                           # Output the inverse matrix
}
