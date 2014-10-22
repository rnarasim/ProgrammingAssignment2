## Basic function to calcuate the inverse of a matrix and cache it.
## Cached value of inverse matrix is returned, if cached inverse is not null
## If cached inverse is null, inverse is calculated and cached
##

## Write a short comment describing this function
## function to cache matrix and  inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        ## when we value of matrix is set, we reset the inverse of the matrix to null
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s <<- inverse
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to take a Matrix as an input and find the inverse and cach

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        s <- x$getinverse()
        
        ## if the inverse of the matrix is not null, implies it is cached
        ## retrieve from cache and return cached value
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        ## if we get this far means we don't have the inverse cached
        ## find the inverse and cache it
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
