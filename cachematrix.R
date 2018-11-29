## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(above) inverse <<- above
        getinverse <- function() inverse
        list(get=get, set=set,
             getinverse=getinverse,
             setinverse=setinverse)
}


## This function computes the inverse of the special "matrix" returned by
## `makeCacheMatrix` above.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}
