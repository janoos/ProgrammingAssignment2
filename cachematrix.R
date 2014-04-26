## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## The functions in this file can be used to create and cache the inverse of a given matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
			x <<- y
			inverse <<- NULL
		}
		get <-function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if (!is.null(i)){
        	message("getting cache data")
        	return(i)
        }
        mat_data <- x$get()
        i <- solve(mat_data, ...)
        x$setinverse(i)
        i
}
