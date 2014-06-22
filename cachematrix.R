## Programming Assignment #2 by Securonomics
## These two function implement a special "matrix" object as a list with 
## embedded functions that can set and get the matrix and the inverse of
## the matrix is previously cached

## The function, `makeCacheMatrix` creates a list that caches a special 
## "matrix" object to store the original matrix and the inverse of that
## matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) { ## set the matrix and the inverse
            x <<- y
            i <<- NULL
        }
        
        ## add functions to the object to return the matrix, set the inverse
        ## or get the inverse.
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        ## create the list that encompasses the matrix data, the inverse data
        ## and the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve( x, ... ): This function computes the inverse of x, a special
## 'matrix' object returned by the `makeCacheMatrix` function listed above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve() retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    # first get the cached value of the inverse, i
    i <- x$getinverse()
    
    ## check to see if the inverse has already been calculated. If so, return
    ## the cached inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## if inverse has not been calculated, then solve the inverse, set the 
    ## cache to hold the computed inverse, and return the inverse
    data <- x$get()
    i <- solve(data, ... )
    x$setinverse(i)
    i
}