## function makeCacheMatrix creates a special matrix that can cache its inverse

## function cacheSolve computes the inverse of a special matrix created by makeCacheMatrix
## if the inverse has been already calculated and the matrix unchanged, the cached value is returned 


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## define 4 functions of matrix x
        ## setting the matrix x
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## getting the matrix x
        get <- function() x
        ## setting the inverse of matrix x
        setinverse <- function() i <<- solve(x)
        ## getting the inverse of matrix x
        getinverse <- function() i
        
        ## returns a list of 4 functions
        list(set = set, get = get
             , setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        
        ## the function takes a special matrix x, 
        ## created by makeCacheMatrix, as an argument
        
        ## pull the inverse i from the special matrix x
        i <- x$getinverse()
        
        ## if inverse i exists, return cached data
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## i does not yet exist: calculate and set inverse of matrix x
        i <- solve(x$get())
        x$setinverse(i)
        
        ## returns a matrix that is the inverse of 'x'
        i
}