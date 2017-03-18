## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # inversed value
        inverse <- NULL
        # setter
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        # getter
        get <- function() x
        # setter for inverse
        setinverse <- function(i) inverse <<- i
        # getter for inverse
        getinversed <- function() inverse
        
        list(set = set, get = get, setinverse = setinverse, getinversed = getinversed)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinversed()
        if (!is.null(i)) {
                return(i)
        }
        mtx <- x$get()
        i <- solve(mtx, ...)
        x$setinverse(i)
        i
}
