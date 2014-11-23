# This file contains functionality for computing the 
# the inverse of a matrix. Caching the inverse instead
# of computing it repeatedly.

#  Creates a special matrix object
makeCacheMatrix <- function(x = matrix()) {
    inverse<-NULL
    
    setMatrix<-function(y){
        x <<- y
        inverse <<- NULL
    }
    
    getMatrix<-function() x
    
    setInverse<-function(solve) inverse <<- solve 
    
    getInverse<-function() inverse
    
    list(setMatrix=setMatrix, 
         getMatrix=getMatrix,
         setInverse=setInverse,
         getInverse=getInverse)

}

# Returns the inverse of a given matrix
cacheSolve <- function(x, ...) {
    inverse<-x$getInverse()
    
    if(!is.null(inverse)){
        message("getting cached data")
        return(inverse)
    }
    
    matrix <- x$getMatrix()
    
    inverse <- solve(matrix)
    
    x$setInverse(inverse, ...)
    
    inverse # Return the inverse
}
