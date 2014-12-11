## makeCacheMatrix creates a special matrix object that is able to cache its inverse. 
## solveCache calculates the inverse of the matrix so long as it hasn't been 
## calculated before and sets the inverse in the cache, otherwise it returns the 
## cached value of the matrix inverse.

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function (y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function (inv){
        inverse <<- inv
    }
    getInverse <- function (){
        inverse
    }
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve calculates the inverse of the matrix returned by makeCacheMatrix as
## as long as the inverse has not already been calculated. If the inverse has been 
## calculated and the matrix has not been changed, the function retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        
        if(!is.null(inverse)){
            message("getting cached inverse")
            return(inverse)
        }
        
        matrix <- x$get()
        cacheInverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}
