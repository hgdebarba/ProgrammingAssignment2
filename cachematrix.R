## The computation of a matrix inverse has high processing demand.
## Thus, this assignment proposes the implementation of a structure that stores 
## the inverse matrix when it is computed, so that it can be read when needed. 
## The cached value is kept for as long as the matrix values were not modified.

## For the sake of clarity, more meaningful variable names were used here


## makeCacheMatrix - Creates the "special" matrix containing functions that 
## get/set the matrix and its inverse

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(newMat) {
        mat <<- newMat
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(newInv) inv <<- newInv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve - Check if the cached inverse matrix variable is not null,
## If not null, it returns the cached value. Otherwise, it computes the 
## inverse, set it on the "special" matrix, and return this value

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}


## This additional piece of code tests the functions above
## creates a 2D rotation matrix, 90 degrees counter clockwise
rot90DegCCW <- matrix(c(0, 1, -1, 0), 2, 2)
## a rotation matrix is always invertible, and its inverse is = to its transpose
rot90DegCCW

## generates the "special" matrix 
rot90DegCCWSpecial <- makeCacheMatrix(rot90DegCCW)
## run cacheSolve twice to check if cached value is being kept
## inverse is computed
cacheSolve(rot90DegCCWSpecial)
## and cached inverse is recovered
cacheSolve(rot90DegCCWSpecial)

## set the matrix value to check if the inverse is being set to NULL (it should)
rot90DegCCWSpecial$set(matrix(c(0, 1, -1, 0), 2, 2))
## if so, inverse is computed
cacheSolve(rot90DegCCWSpecial)
## and cached value is recovered
cacheSolve(rot90DegCCWSpecial)


# this is the original example provided
# makeVector <- function(x = numeric()) {
#     m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }