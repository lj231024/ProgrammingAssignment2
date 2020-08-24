## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than  
## compute it repeatedly. My purpose is to write a pair of functions 
## that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

## We assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve # Computing the inverse of 
                                                  # a square matrix can be done
                                                  # with the solve function in R.
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)    # Computing the inverse of 
                                 # a square matrix can be done
                                 # with the solve function in R.
        
        x$setinverse(m)
        message("not getting cached data")
        m
}

