## The following functions are used to create a special matrix object
## that is able to cache it's inverse matrix calculation and is also
## able to retrieve or performe that operation


## makeCacheMatrix  creates an special matrix object that's able to set 
## and get a matrix provided by the user, and is also able to set
## or get a inverse matrix provided by the user.
## It receives the matrix to be converted to the special object as parameter.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list(set = set, get = get,
         setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve receives a cacheMatrix object returned by makeCacheMatrix and 
## returns the inverse matrix, or the result of applying solve(), by either
## retrieving the cached result in x, or calling solve on the matrix stored in x.
## In the latter case, the result is cached in x and returned.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
