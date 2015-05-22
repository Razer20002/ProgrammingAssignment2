## This will cache the inverse of a matrix so it doesn't 
## have to be calculated repeatedly

## makeCacheMatrix is a fucntion that creates a list of functions
## and will cache the inverse of a matrix (once defined).
## Note: The inverse is not defined in the function, only stored.
## get() will return the matrix x stored in makeCacheMatrix
## set() will overwrite the matrix x in the main function with a new suppied one
## setinverse and getinverse simply set and store the inverse matrix.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inverse) {
                m <<- inverse
        }
        getinverse <- function() {
                m
        }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Here we check if the inverse of the matrix has been calculated.
## If so we return it and exit the function.
## otherwise it is calculated and stored using x$setinverse(m)
## It is then available for next time

cacheSolve <- function(x, ...) {
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
