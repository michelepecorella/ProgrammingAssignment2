## Return a matrix that is the inverse of x

## This function creates a special "matrix" object that can cache its inverse. It's really a list containing a function to set and to get the value of the matrix and to set and to get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## The following function calculates the inverse matrix with the special "matrix" created with the above function. 


cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}