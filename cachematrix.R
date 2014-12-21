## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions and the input will be a matrix

makeCacheMatrix <- function(x = matrix()) {
    mtrx <- NULL
    set <- function(y){
        x <<- y
        mtrx <<- NULL
    }
    get <- function () x ## The get function retrieve the original matrix.
    
    setSolve <- function(solve) mtrx <<- solve ## The setSolve function applies the solve to the original matrix.
    
    getSolve <- function() mtrx ## The getSolve function retrieve the inverse matrix.
    
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## Function that return the inverse of the original matrix 
## and if the matrix does't change shows the cached inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtrx <- x$getSolve() 
    if(!is.null(mtrx)) { ## If the matrix doesn't change returns the inverse matrix from cache in console

        message("getting cached data")
        return(mtrx)

    }
    data <- x$get()             ## Obtains matrix
    mtrx <- solve(data, ...)    ## Applies the solve function
    x$setSolve(mtrx)            ## Assign the inverse matrix
    mtrx                        ## Return the inverse matrix in console
}
