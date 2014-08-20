## Author: S.Menne
## Date created: August 20th 2014

## The function makeCacheMatrix creates a list of functions which can manipulate the given matrix x and its inverse
## cacheSolve uses a list created by makeCacheMatrix to retrieve or set and retrieve the inverse of matrix x,
## without computing it again if x has not been changed

## makeCacheMatrix creates a list of functions which are used to set and get the matrix x (given as parameter)
## and to get and set the Inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
    ## the inverse of x has not yet been calculated
    inverseX <- NULL
    ## assign to matrix object x a new matrix new_matrix, and reset the inverse
    ## <<- is used to assign a value to x in the parent environment (function makeCacheMatrix)
    set <- function(new_matrix) {
        x <<- new_matrix
        inverseX <<- NULL
    }
    ## get returns the matrix x
    get <- function() x
    ## setInverse assigns the inverse Matrix (given as parameter) to inverseX
    setInverse <- function(inverseMatrix) inverseX <<- inverseMatrix
    ## getInverse returns the inverse Matrix (if set, otherwise NULL)
    getInverse <- function() inverseX
    ## return the list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix created by makeCacheMatrix
## It retrieves the inverse of the matrix via x$getInverse(). If null is returned the inverse has not been calculated.
## In that case the inverse is calculated and assigned to x. Otherwise the precalculated inverse is retrieved and
## returned.
## Assumption as per assignment: The matrix provided is always invertible, so the function will not check for this

cacheSolve <- function(x, ...) {
    ## retrieve the inverse from x
    inverseX <- x$getInverse()
    ## if something other than null is returned, the inverse was already calculated and can be returned
    if(!is.null(inverseX)) {
        message("Retrieving stored data")
        return(inverseX)
    }
    ## otherwise retrieve the matrix from list x, calculate the inverse via solve() and set it via setInverse,
    ## then return it
    m <- x$get()
    x$setInverse(solve(m))
    x$getInverse()
}
