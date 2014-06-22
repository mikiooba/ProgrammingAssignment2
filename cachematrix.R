###############################################################################
##
## makeCacheMatrix 
##
## Description
##
## This function creates a special matrix object that can cache its inverse.
## It returns an object containing the following functions:
## get()       returns the original matrix
## set(y)      sets the matrix y to the object
## getsolve()  returns the inverse matrix of the original matrix
## setsolve(inv)  sets the inverse matrix inv to the object and cache it
## 
## This function assumes that the matrix is always invertible, 
## hence no checking is done within the function.
##
## Usage
##
## makeCacheMatrix(a)
##
## Arguments
##
## a   an invertible matrix
## 
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialise the variable to store (cache) the inverse matrix 
    inv <- NULL
    
    # Set the matrix and initialise the cache
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # Get the original matrix
    get <- function() x
    
    # Set the inverse matrix and cache it
    setsolve <- function(invmat) inv <<- invmat

    # Get the inverse matrix
    getsolve <- function() inv
    
    # Return the list of functions above 
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


###############################################################################
##
## cacheSolve
##
## Description
##
## This function takes the object returned by the makeCacheMatrix function 
## and returns the inverted matrix of the original matrix in the object. 
## The inverted matrix is returned from the cache, if it is already cached 
## in the object.
##
## Usage
##
## makeCacheMatrix(a, ...)
##
## Arguments
## 
## a   a matrix object created by the function makeCacheMatrix
## 
###############################################################################
cacheSolve <- function(x, ...) {

    # Get the cached inverse matrix
    inv <- x$getsolve()

    # If the inverse matrix is already cached, return the cache 
    # and show a message informing that the cache is returned
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    # Get the original matrix
    data <- x$get()
    
    # Calculate the inverse matrix
    inv <- solve(data, ...)
    
    # Cache the inverse matrix to the object
    x$setsolve(inv)
    
    # Return the inverse matrix
    inv
}
