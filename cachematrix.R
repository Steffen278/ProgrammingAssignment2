## Author: S.Menne
## Date created: August 20th 2014

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix 
## 

makeCacheMatrix <- function(x = matrix()) {
    ## the inverse of x has not been calculated
    inverseX <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        inverseX <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) inverseX <<- inverseMatrix
    getInverse <- function() inverseX
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Assumption as per assignment: The matrix provided is always invertible, so the function will not check for this

cacheSolve <- function(x, ...) {
        inverseX <- x$getInverse()
        if(!is.null(inverseX)) {
            message("Retrieving stored data")
            return(inverseX)
        }
        m <- x$get()
        x$setInverse(solve(m))
        x$getInverse()
}
