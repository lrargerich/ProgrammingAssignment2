## Set of functions to cache matrix inverses
## useful if you need to invert a matrix multiple times
## 
## by Luis Argerich 2014 
## 
## usage:
##
## initialize:
## mymatrix = matrix(data,nrow=,ncol=1)
## mcached <- makeCacheMatrix(mymatrix)  
##
## get regular matrix:
## matrix <- mcached$get()
## 
## set to new matrix:
## matrix <- mcached$set(newmatrix)
##
## compute inverse (will try to use cache if possible):
## inverse <- cachesolve(mcached)


## Receives a regular matrix and returns a cached matrix object
## which is a list with get and set methods to set and update the
## matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(newm) {
        x <<- newm
        inv <<- NULL 
    }
    
    get <- function() {
        x
    }

    # Internal methods
    setinv <- function(newinv) {
        inv <<- newinv
    }
    
    getinv <- function() {
        inv
    }
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Returns the inverse of x trying to use cached copy of it 
## if available
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    if (!is.null(inv)) {
        message("getting cached inverse")
        inv
    }
    
    matrix <- x$get()
    
    inv <- solve(matrix,...)
    
    x$setinv(inv)
    inv
}
