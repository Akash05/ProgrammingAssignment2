## For the purpose of easy and fast matrix computations the following functions
## can help in caching the inverse of a matrix. 

## The function makeCacheMatrix is used to create a "special" type of matrix
## that can cache its inverse

makeCacheMatrix <- function(X = matrix()) {

	  inv <- NULL
        set <- function(Y) {
                X <<- Y
                inv <<- NULL
        }
        get <- function() {X}
        setinverse <- function(inverse) {inv <<- inverse}
        getinverse <- function() {inv}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve is used to cache the inverse of a matrix 
## if the matrix has not changed and calculate the inverse if the matrix has changed

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'X'

	  inv <- X$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- X$get()
        inv <- solve(data, ...)
        X$setinv(inv)
        inv

}
