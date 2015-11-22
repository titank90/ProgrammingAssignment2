 
## Author: Bruno Jacobs - October 19, 2015

## -- Functions
## -- 1) makeCacheMatrix : takes a square invertible matrix as input and returns a list of 4 functions
## -- 2) cacheSolve : returns the inverse of the matrix by computation or directly from the cache if it already exists

makeCacheMatrix <- function(x = matrix()) {
    
    ## -- makeCacheMatrix:
    ## -- Takes a square invertible matrix as argument and returns a list of 4 functions attached to the matrix object
    ## -- These functions act on the matrix and it's inverse which are both stored in the parent environment (cache)
    ## ----- set: set the matrix
    ## ----- get: get the matrix
    ## ----- setinv: set the inverse
    ## ----- getinv: get the inverse
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

