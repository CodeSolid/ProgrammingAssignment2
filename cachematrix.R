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
            message("Getting cached matrix")
            return(M)
    }
    data <- x$get()
    M <- solve(data, ...)
    x$setinverse(M)
    M
}

## cacheSolveTest
## This function runs the assignment and shows that the second time it is run, the line 
## "getting cached data" is called.
## It also demonstrates that the solution is correct given R's "solve" method.
cacheSolveTest <- function() {
    
    print("Creating a solevable matrix as shown in the solve example at http://www.endmemo.com/program/R/solve.php")
    M <- matrix(c(3,1,2,1),nrow=2,ncol=2)
    
    print("Displaying the matrix")
    print(M)    
    
    print("Making the matrix cacheable")    
    Mcacheable <- makeCacheMatrix(M)
    
    print("Solving inverse the first time, won't be cached")
    Minverse <- cacheSolve(Mcacheable)
    
    print("Printing the inverse returned by cacheSolve")
    print(Mcacheable$getinverse())
    
    print("Solving inverse the second time, WILL be cached")
    MinverseCached <- cacheSolve(Mcacheable)
    
    print("Demonstrating correct result by solving original matrix, next line should be TRUE")
    print(identical(MinverseCached, solve(M)))
    
}