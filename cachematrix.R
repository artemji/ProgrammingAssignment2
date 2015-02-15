## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## 
## The functions below can cache the inverse of a matrix.
##
## Note: it is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(mv) {
                m <<- mv
                i <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {	
        i <- m$getinverse()
        if (!is.null(i)) {
                message("getting inverse matrix")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        i
}
