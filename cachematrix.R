## R Programming COURSE. Programming Assignment 2:
## By fgonzalezalonso@outlook.es - lanthano
## COURSE NUMBER: rprog-011
## IMPORTANT NOTE: I'm not a native English speaker, so excuse me for the mistakes I will commit.
## Please be polite.
## FUNCTION makeCacheMatrix DESCRIPTION:
## It create the cache matrix object to calculate the inverse matrix.
## INPUT: matrix element
## OUTPUT: list element
## OUTPUT ELEMENT DESCRIPTION:
## Length Class Mode
## set 1 -none- function
## get 1 -none- function
## setsolve 1 -none- function
## getsolve 1 -none- function
## EXAMPLES OF USES:
##
## source("cachematrix.R")
## c=rbind(c(1, -1/4), c(-1/4, 1))
## cm <- makeCacheMatrix(c)
## cm$get() ## return the originial matrix
## cm$getsolve() ## return the inverse matrix
## cm$set(rbind(c(1, -1/2), c(-1/2, 1))) ## set the another matrix value
## cm$setSolve(solve(rbind(c(1, -1/2), c(-1/2, 1)))) ## set the another inverse matrix value
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## SET FUNCTION sets the value of X MATRIX stored in the object.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## GET FUNCTION returns the value of x MATRIX stored in the object.
    get <- function() x
    ## SETSOLVE FUNCTION sets the value of INVERSE X MATRIX stored in the object
    setsolve <- function(solve) m <<- solve
    ## GETSOLVE FUNCTION returns the value of INVERSE X MATRIX stored in the object
    getsolve <- function() m
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
## FUNCTION cacheSolve DESCRIPTION:
## RETURNs the inverse matrix of x, but x must be a makeCacheMatrix Object (list).
## INPUT: makeCacheMatrix Object (list)
## OUTPUT: MATRIX
## OUTPUT ELEMENT DESCRIPTION: the inverse matrix of makeCacheMatrix Object (list)
## EXAMPLES OF USE:
##
## source("cachematrix.R")
## c=rbind(c(1, -1/4), c(-1/4, 1))
## cm <- makeCacheMatrix(c)
## cacheSolve(cm)
## The Result must be: setting cached data ... (PRINT INVERSEMATRIX)
## cacheSolve(cm)
## The Result must be: getting cached data ... (PRINT INVERSEMATRIX)
cacheSolve <- function(x, ...) {
    ## gets the inverse matrix value stored in the x object
    inverseMatrix <- x$getsolve()
    ## If inverseMatrix is not NULL means that the inverse Matrix was calculated
    if(!is.null(inverseMatrix)) {
        ## Then returns the inverse Matrix and prints his state.
        message("getting cached data")
    }else{
        ## Then calculates the inverse Matrix and prints his state.
        ## Obtains the X MATRIX value.
        data <- x$get()
        ## Obtains the INVERSE X MATRIX value.
        inverseMatrix <- solve(data, ...)
        ## Stores the INVERSE X MATRIX value
        x$setsolve(inverseMatrix)
        message("setting cached data")
    }
    ## Returns the INVERSE X MATRIX value
    return(inverseMatrix)
}
