## These two functions create a special matrix and its inverse and cache, then retrieve it from 
## if it already exists

## Creates a function list for matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setIn <- function(solve) m <<- solve 
    getIn <- function() m
    list(set = set, get = get, setIn = setIn, getIn = getIn)
}


## Gets the inverse from cache and retrieves it, or creates it, puts it in cached and retrieves it

cacheSolve <- function(x, ...) {
    m <- x$getIn()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setIn(m)
    m        ## Return a matrix that is the inverse of 'x'
}
