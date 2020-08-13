## Assignment: Caching the Inverse of a Matrix
## The folowing two functions cache the inverse of a matrix.
##
## makeCacheMatrix - creates a special type of matrix which is 
## really a list containing a function to get and set matrix 
## and inverse matrix
##
## cacheSolve - calculates the inverse of the special "matrix" 
## created with the above function, first checking to see if 
## the inverse matrix is already cached

## This function creates a matrix object which has an attribute im
## which can store the solved inverse of that matrix. 
## both the matrix and inverse matrix data (if calculated) are cached
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinversematrix <- function(inversematrix) im <<- inversematrix
    getinversematrix <- function() im
    list(set = set, get = get,
         setinversematrix = setinversematrix,
         getinversematrix = getinversematrix)
}

## Creates an inverse matrix for a given makeCacheMatrix Object.
## If the inverse martix has already been 'solved' it uses a cached solution
## NOTE: it is assumed that the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinversematrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    # need inverse calculation here
    im <- solve(data, ...)
    x$setinversematrix(im)
    im
}




