## Put comments here that give an overall description of what your
## functions do

## THis function, makeCacheMatrix, is to create a matrix with functions.

makeCacheMatrix <- function(x = matrix()) {
        #initialize a varible to store values
        tempMatrix <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                tempMatrix <<- NULL
        }
        
        # get the value of the matrix
        get <- function() x
        
        # invert the matrix and store in tempMatrix
        setInverse <- function(inverse) tempMatrix <<- inverse
        
        # get the inverted data from tempMatrix
        getInverse <- function() tempMatrix
        
        # return the created values
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## THis function, cacheSolve, is to find a inversed matrix from another inout.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        
}
