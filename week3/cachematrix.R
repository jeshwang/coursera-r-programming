## COursera: Week 3, Assignment 1
## For Week Ending: Nov. 12, 2017
## User: jeshwang

#########################################################################
## Write a pair of functions that cache the inverse of a matrix.
#########################################################################

## Clear Memory

rm(list=ls())
gc(reset=TRUE)

## Functions
makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its inverse.
        ## Return a list with the following functions:
        ##              1. Set the matrix
        ##              2. Get the matrix
        ##              3. Set the inverse
        ##              4. Get the inverse
        ## This list is used as the input to cacheSolve().
        
        ## inv will hold the inverted matrix. 
        inv <- NULL
        
        ## set function assigns a new matrix in parent environment.
        ## If there's a new matrix, reset inv to NULL. 
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        
        ## get function returns value of the matrix argument.
        get <- function() x  
        
        ## setinv function assigns value of inv in parent environment.
        setinv <- function(inverse) inv <<- inverse
        
        ##getinv function gets the value of inv.
        getinv <- function() inv
        
        list(set = set, get = get,
             setmean = setinv,
             getmean = getinv)
}


## cacheSolve() computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
## changed), the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        ## If the matrix has already been inverted, retrieve inverse from cache.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## Else, calculate the inverse using the solve function.
        data <- x$get()
        inv <- solve(data, ...)
        
        ## Set the value of the inverse into cache.
        x$setinv(inv)
        
        return(inv)
        
}