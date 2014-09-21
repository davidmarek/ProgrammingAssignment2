# Functions for storing a matrix and computing (and caching) its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a matrix object to store the matrix with its inverse.
    #
    # Args:
    #   x: Initial value of the matrix.
    #
    # Methods:
    #   SetMatrix(x): Sets the matrix and recomputes its inverse.
    #   GetMatrix(): Returns the matrix.
    #   SetInverse(im): Sets the matrix's inverse.
    #   GetInverse(): Returns the matrix's inverse.
    
    # Stored matrix
    matrix <- x
    # Cached inverse
    inverse <- NULL
    
    SetMatrix <- function(x) {
        matrix <<- x
        # Invalidate the cached inverse.
        inverse <<- NULL
    }
    
    GetMatrix <- function() matrix
    
    SetInverse <- function(im) inverse <<- im
    
    GetInverse <- function() inverse
    
    list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, 
         SetInverse = SetInverse, GetInverse = GetInverse)
}


cacheSolve <- function(x, ...) {
    # Computes an inverse of a matrix.
    #
    # Args:
    #   x: Matrix object created by makeCacheMatrix.
    # Returns:
    #   The inverse of x.
    
    inverse <- x$GetInverse();
    # Check if the inverse has been computed.
    if (is.null(inverse)) {
        # Compute the inverse.
        inverse <- solve(x$GetMatrix(), ...)
        # Cache the inverse.
        x$SetInverse(inverse)
    }
    inverse
}
