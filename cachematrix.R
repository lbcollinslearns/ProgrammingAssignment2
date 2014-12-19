# This code defines two functions: makeCacheMatrix and cacheSolve

# makeCacheMatrix: 
# Takes matrix as input and defines helper functions (methods)
# that allow repeated calls to solve (invert) the matrix.
# Returns a list object with input, result (intially NULL), and functions

# cacheSolve: 
# Calculates the inverse of a matrix, but only if not
# already calculated (and cached) in the makeCacheMatrix object.

# makeCacheMatrix(x)
####################
## Arguments: 
## x: an invertible, square matrix (defaults to an empty matrix)

## Defined Methods (functions):
## set(y):      reset the matrix x using matrix y using superassignment
## get():       returns the currently set matrix
## setFun(FUN): superassign the function called from cacheSolve to act on x
## getResult(): returns the calculated result of FUN acting on x 
##              or returns NULL if cacheSolve has not yet been called

## Return Value: a list of all methods: set, get, setFun, getResult

makeCacheMatrix <- function(x = matrix()) {
	    m <- NULL
	    # reset the matrix x using matrix y using superassignment
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setFun <- function(FUN) m <<- FUN
        getResult <- function() m
        list(set = set, get = get,
             setFun = setFun,
             getResult = getResult)

}

# cacheSolve(x, ...)
####################
## Arguments:
## x: an object created by makeCacheMatrix 
## ...: optional additional arguments to the function that will act on x

## Algorithm Description:
## getResult from object x
##    If the result is not NULL, return the cached result and exit
##    If the result is NULL
##        Get the data (matrix)
##        Calculate the result (invert the matrix)
##        Set the function for object x to be "solve"
##        Return the result and exit

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getResult()
        if(!is.null(m)) {
                message("cacheSolve() is providing a cached result")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setFun(m)
        m
}

