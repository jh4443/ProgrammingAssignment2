# makeCcheMatrix is a function to set a matrix and get a matrix
# cacheSolve is matrix used to solve the inverse of imput matrix

# makeCacheMatrix contains four functions: set, setinverse, get, getinverse
makeCacheMatrix <- function(x = matrix()) {
        inverse_of_M  <- NULL 
        set = function(y){                                                            # set matrix
                x <<- y
                inverse_of_M <<- NULL
        }
        get <- function() x                                                           # get matrix                           
        setinverse <- function(inverse) inverse_of_M <- inverse                       # set inverse of matrix
        getinverse <- function() inverse_of_M                                         # get inverse of matrix              
        list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}


# cacheSolve function calculate the inverse of matrix

cacheSolve <- function(x, ...) {
        inverse_of_M <- x$getinverse()                # Know the property of cache
        if(!is.null(inverse_of_M)) {                  # check the cache data whether it is empty
                message("getting cached data")
                return(inverse_of_M)                  # If it isn't empty, return the cache
        }
        mat <- x$get()                                # get the matrix
        inverse_of_M <- solve(mat)                    # calculate the inverse matrix
        x$setinverse(inverse_of_M)                    # set the inverse matrix
        inverse_of_M                                  # output the inverse matrix
}
