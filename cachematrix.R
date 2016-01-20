## Taking the inverse of a complicated matrix is very complicated and time-consuming, especially if it has to be computed repeatedly 
## (e.g. in a loop). If the contents of a matrix are not changing, it may make sense to cache the value of the mean so that when we 
## need it again, it can be looked up in the cache rather than recomputed. 
## The following function is basically doing the thing described above.

## This function creates a special "matrix" object that can cache its inverse.
## It's a list containing a function to: set the value of the matrix; get the value of the matrix; set the value of the inverse of the 
## matrix; get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
}
 get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
