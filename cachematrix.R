## The two functions are used to compute and cache the inverse of a matrix

## The function creates a list that can function to 
## set the value of a matrix
## get the value of a matrix
## setinverse of a matrix
## getinverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of a matrix. 
## In case it is already calculated returns from the cache otherwise computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("checking cached data and retrieving it")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
