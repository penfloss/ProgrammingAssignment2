## Assignment 2, R Programming
## Caching inverse of a matrix


## Function to create a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # Set inverse object to NULL during initialisation
    invM <- NULL
    
    # Replace the matrix and reset inverse
    set <- function(y) {
        x <<- y
        invM <<- NULL
    }

    # Return the current value of matrix x
    get <- function() x
    
    # Set the current value of inverse invM
    setInv <- function(inv) invM <<- inv
    
    # Return the current inverse
    getInv <- function() invM
    
    # Return the list of functions created for 
    # the new makeCacheMatric object
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
}


## Computes the inverse of the matrix returned by makeCacheMatrix
## Takes a makeCacheMatrix object as it's argument

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Retrieve the current value of inv
    inv <- x$getInv()
    
    # If inv already has a cached value, return this
    if(!is.null(inv)) {
        message("Retrieving cached inverse matrix")
        return(inv)
    }
    
    # If nothing was returned, retrieve the original matrix
    # and calculate the inverse
    m <- x$get()
    inv <- solve(m)
    
    # Cache the inverse just calculated
    x$setInv(inv)
    
    # Return the inverse
    inv
}
