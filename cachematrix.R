## The following function creates a special matrix object called makecachematrix. 
## makecachematrix is a list of functions that sets and gets the value of a given matrix
## as well as sets and gets the inverse of that matrix.

makecachematrix <- function(x = matrix()) {
        
        i <- NULL        
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function computes the inverse of the special matrix returned by 
## makecachematrix above. If the inverse has already been calculated
## and the matrix has not changed then cachesolve should retrieve the inverse from the cache

cachesolve <- function(x, ...) {
        
        i <- x$getinv()
        
        if(!is.null(i)){
                message("inverse from cache")
                return(i)
        }
        
        z <- x$get()
        i <- solve(z)
        x$setinv(i)
        i
}
