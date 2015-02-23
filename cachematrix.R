## calculat the inverse of a matrix and saves it to a cache.
## The saved value is returned when next time recalled

## create a special 'matrix' object.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y 
        m <<- NULL
    }
    get <- function() x 
    setinverse <- function(inverse) m <<- inverse 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## caculate the inverse of the matrix

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
