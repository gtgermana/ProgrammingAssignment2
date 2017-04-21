## These functions compute and cache the inverse of a matrix for later use. 

## Function makeCacheMtrix creates an object that can be cached.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(xinverse) m <<- xinverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve computes, caches and returns the matrix inverse of 'x'  
## if not previously computed. Otherwise, it returns the cached matrix inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
