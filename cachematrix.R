
##
## Caching the Inverse of a Matrix
##
## The following functions can be used to cache the inverse of
## a matrix instead of calculating it repeatedly
##



## 
##  makeCacheMatrix
##
##  Makes a matrix object that can cache its inverse
##  Input x:    The actual matrix 
##
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize cache to NULL
    cache <- NULL
    
    
    # Member fuctions
    
    # set, resets the matrix and cahce
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    # get, returns the original matrix
    get <- function() x
    
    # setinverse, resets the cache (new inverse value)
    setinverse <- function(inverse) cache <<- inverse
    
    # getinverse, returns the inverse value from cache
    getinverse <- function() cache
    
    
    # Return a list of "member functions" 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 
##  cacheSolve
##
##  Calculate inverse for matrix in x, use cached value if exists
##  Input x:    Cached matrix object (created with makeCacheMatrix)
##  Input ...:  Function parameters for solve -function
##
cacheSolve <- function(x, ...) {
    
    # Get the inverse value from the cache
    i <- x$getinverse()
    
    # Check if cached value is already calculated
    if(is.null(i)) {
        
        # No inverse in cache, calculate inverse for matrix in x
        data <- x$get()
        i <- solve(data, ...)
        
        # Store calculated inverse to cache 
        x$setinverse(i)
    }

    # Return a matrix that is the inverse of 'x'
    i
}

