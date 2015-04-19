## Put comments here that give an overall description of what your
## functions do

## As described in the assignment, 
## "This function creates a special "matrix" object that can cache its inverse."
## Based on example makeVector.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## As described in the assignment,
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
	M <- x$getinverse()
    if(!is.null(M)) {
            message("getting cached data")
            return(M)
    }
    data <- x$get()
    M <- solve(data, ...)
    x$setinverse(M)
    M
}