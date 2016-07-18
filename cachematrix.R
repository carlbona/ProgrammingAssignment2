## This file contains two functions to cache results after inversion of a square matrix,
## exploiting the lexical scoping rules in R. 

## This is a function to create a special matrix object that can cache
## the results produced by the cacheSolve function below. Takes a matrix as an argument.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, 
             setSolve = setSolve,
             getSolve = getSolve)
        
}


## Invert a square matrix, using cached results if available. If no results are available,
## it uses the solve() function to calculate 
## Takes an object created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}