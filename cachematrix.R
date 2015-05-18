## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse value to NULL
    inverse <- NULL
    
    ## set the matrix value
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }   
    
    ## get the matrix value
    get <- function() x
    
    ## set the inverse value
    setinverse <- function(inv_) inverse <<- inv_
    
    ## get the inverse value
    getinverse <- function() inverse
    
    ## return a list of four functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## check the inverse value
    inv_ <- x$getinverse()
    if(!is.null(inv_)) {
        message("getting cached data")
        return(inv_)
    }
    
    ## if the inverse value is NULL, compute the value by solve() and return the value to x
    data <- x$get()
    inv_ <- solve(data, ...)
    x$setinverse(inv_)
    inv_
}
