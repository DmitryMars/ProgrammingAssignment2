## makeCacheMatrix and cacheSolve functions allows to calculate inverse matrix
## for given matrix x and cache inverse in special matrix object.

## makeCacheMatrix creates and returns a wrapper for matrix x. It stores 
## cache of inverse matrix for the matrix x. To calculate and cache inverse 
## matrix of x use cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y # variables x and inv are closed inside makeCacheMatrix scope  
        inv <<- NULL 
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    return(list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse))
}


## cacheSolve calculates, caches and returns inverse for matrix x. 
## x shold be a matrix wrapper, created by makeCacheMatrix function.
## An exceptions occurs if: a) x is nott a matrix wrapper object
##                          b) matrix is not invertible (not square  
## or near-singular)                           

cacheSolve <- function(x, ...) {
    inversecache<-x$getinverse()
    
    if(!is.null(inversecache)){
        message("retrieving inverse from cache...")
        return(inversecache)
    }
    
    inverse<-solve(x$get())
    x$setinverse(inverse, ...)
    
    ## Return a matrix that is the inverse of 'x'
    return(inverse)
}
