## Funxtions to allow caching result of matrix inverse operation

## makeCacheMatrix function contains functions and fields to store cached inverted matrix 
## along with the original matrix

makeCacheMatrix <- function(x = matrix()) { ## x - field containing matrix to be inverted
    i <- NULL ## field that will contain cached inverted matrix
    set <- function(y) { ## function to set matrix to be inverted
        x <<- y ## setting the value
        i <<- NULL ## resetting inverted matrix to be NULL, so that it will be recalculated on next call to cacheSolve
    }
    get <- function() x ## returning matrix to be inverted
    setInverse <- function(inv) i <<- inv ## setting value of inverted matrix
    getInverse <- function() i ## return inverted matrix
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse) ## returning as a list of named created functions
}


## Function cacheSolve calculates the inverse of the matrix on the first run and stores it in the cache
## On the second run the cached result is returned

cacheSolve <- function(x, ...) {
    i <- x$getInverse() ## Geting the value of the cahced matrix
    if(!is.null(i)) { ## Checking is the value is not null, meaning that there is a cached inverted matrix
        message("getting cached data") ## printing message to the console
        return(i) ## returning cached inverted matrix
    }
    ## there is no cached invered matrix, so we need to calculate it
    data <- x$get() ## getting the original matrix
    i <- solve(data) ## calculating inverted matrix
    x$setInverse(i) ## storing inverted matrix in makeCacheMatrix
    i ## returning result
}
