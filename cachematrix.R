## functions makeCacheMatrix() and cacheSolve() will return the inverse of
## a matrix and will cache result to avoid repeated computation
## assume the matrix supplied is always invertible


## makeChcheMatrix will create a new object which contained the cached matrix
## and return a list of functions

makeCacheMatrix <- function(x = matrix()) {
        inv  <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
} 



## cacheSolve will first try to retrive the cached inverse. 
## If no cached inverse, compute the inverse matrix and return the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


