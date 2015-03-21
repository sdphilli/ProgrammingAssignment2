## These functions will return the inverse of a matrix, and, if the resource intensive
##      process of inverting a matrix has already been performed, return the prior
##      result instead of doing the same calculations again

## The makeCacheMatrix function will creates a special vector that contains a function to:
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function creates the inverse of the special "matrix" created with the above function.
##      However, it first checks to see if the inverse has already been created.
##      If so, it gets the inverse from the cache and skips the computation. 
##      Otherwise, it creates the inverse of the matrix and sets the value of the
##      inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get ()
        m <- solve(data, ...)
        x$setinverse (m)
        m
}
