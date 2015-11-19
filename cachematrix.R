 
## Author: Bruno Jacobs - October 19, 2015

## -- Functions
## -- 1) makeCacheMatrix : takes a square invertible matrix as input and stores it in the parent environment (cache)
## -- 2) cacheSolve : returns the inverse of the matrix by computation or directly from the cache if it already exists

makeCacheMatrix <- function(x = matrix()) {
    
    ## -- makeCacheMatrix:
    ## -- Takes a square invertible matrix as argument and stores it in the parent environment (cache)
    ## -- Returns a list of 4 functions that act on the matrix and it's inverse
    ## ----- set the matrix
    ## ----- get the matrix
    ## ----- set the inverse
    ## ----- get the inverse
    ## -- These functions are used in the cacheSolve function
    
    inv <- NULL
    
    ## setter function stores matrix in parent environment
    ## variable inv to store the inverse of the matrix is initialized to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## returns the matrix
    get <- function() x
    
    ## store inverse in cache
    setinv <- function(inverse) inv <<- inverse 
    
    ## returns the inverse
    getinv <- function() inv
    
    ## return list of functions
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}

cacheSolve <- function(x, ...) {
    
    ## -- cacheSolve:
    ## -- Returns the inverse from the square matrix stored in the cache (parent environment)
    ## -- If the inverse doesn't exist yet, it is computed and stored in the cache
    ## -- Next time the function is called, the inverse is retrieved from the cache
    
    ## get inverse from cache 
    inv <- x$getinv()
    
    ## check if inverse exists
    if (!is.null(inv)){
        ## inverse exists in cache
        message("got cached data")
    } else {
        ## inverse does not exist, calculate and store in cache
        message("calculate inverse and store in cache")
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
    }
    
    inv
    
}

