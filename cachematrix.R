## Computing the inverse of a matrix and storing it for later use to save 
## comutational effort and time.


## makeCacheMatrix - Creates a list of functions to set and get a matrix and set
## the inverse to a matrix and get the cached value of the inverse, if already
## computed

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
    set <- function(y) {
        x <<- y
        i <<- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
}


## cacheSolve - Gets the inverse of amtrix using the list from makeCacheMatrix.
## It first checks if inverse is already computed in which case the cached value
## of the iverse is called. Else, inverse is computed and stores that value in 
## cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!anyNA(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}