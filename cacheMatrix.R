makeCacheMatrix <- function(x = matrix()) {    # #makeCacheMatrix function will create a special matrix object that can cache          the inverse of a matrix
    inv <- NULL
    set <- function(y) {
        x <<- y # Assigning Value to object x, when environment is different
        inv <<- NULL # Assigning Value to object inv, when environment is different
    }
    get <- function() x # gets the value of the matrix
    setInverse <- function(inverse) inv <<- inverse # sets the inverse of the matrix
    getInverse <- function() inv 
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve function will fetch the inverse of the matrix from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data") # if the inverse matrix value is not available, get it from cached data
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
