## Pair of fucntions that enable caching the invese of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(mi) minv <<- mi
        getinverse <- function() minv
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
        data <- x$get()
        minv <- solve(data, ...)
        x$setinverse(minv)
        minv
}
