## The following function creates a special matrix object called makeCacheMatrix. 
## makeCacheMatrix is a list of functions that sets and gets the value of a given matrix
## as well as sets and gets the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL        
        
        ## The following function sets the global value of x and initializes i.
        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        ## The following piece of code returns the value of x, sets and returns the global value of i.
        
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        
        ## The following piece of code creates a list including set, get, setinv and getinv as defined above
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function computes the inverse of the special matrix returned by 
## makecachematrix above. If the inverse has already been calculated
## and the matrix has not changed then cachesolve should retrieve the inverse from the cache

cachesolve <- function(x, ...) {

        ## The following piece of code returns the inverse of matrix x from the cache.
        
        i <- x$getinv()
        
        ## The following piece of code returns the message "inverse from cache" if the value exists in cache.        
        
        if(!is.null(i)){
                message("inverse from cache")
                return(i)
        }
        
        ## The following piece of code calculates the inverse for the first time and also populates the cache
        
        z <- x$get()
        i <- solve(z)
        x$setinv(i)
        i
}
