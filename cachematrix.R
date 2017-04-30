##Creates a special matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inversem) i <<- inversem
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


##Retrieves the inverse of x if it has already been cached, otherwise computes and returns it 
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    ##Check if the inverse of 'x' is cached and return it if so
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ##if the inverse of 'x' is not cached, compute and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
