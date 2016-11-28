## Functions that create & invert a matrix. 
## The matrix inversion is cached so it is not recalculated.

## makeCacheMatrix provides functions to 
## 1. Set the matrix & initialize its inverse.
## 2. Get the matrix.
## 3. Set the inverted matrix.
## 4. Get the inverted matrix.

## To test the functions:
## Create a makeCacheMatrix object called m
##m <- makeCacheMatrix(matrix(c(4,2,7,6), nrow = 2, ncol = 2))
## Execute to calculate & return inverted matrix w/o cache message.
##cacheSolve(m)
## Execute to calculate & return inverted matrix w cache message.
##cacheSolve(m)
## Place a different matrix into m.
##m$setMatrix(matrix(c(4,1,3,8,7,11,13,21,17), nrow = 3, ncol = 3))
## Execute. No cache message.
##cacheSolve(m)
## Execute. Cache message.
##cacheSolve(m)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setMatrix <- function(y) { 
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(mt) m <<- mt
    getInverse <- function() m
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of matrix x.
## If the inverse has already been calculated, it returns the cached value

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mi <- x$getInverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$getMatrix()
    mi <- solve(data)
    x$setInverse(mi)
    mi
}
