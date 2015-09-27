## makeCacheMatrix: 
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseValue = NULL
  set = function(y) {
    x <<- y
    inverseValue <<- NULL
  }
  get = function() x
  setinv = function(inverse) inverseValue <<- inverse 
  getinv = function() inverseValue
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseValue = x$getinv()
  
    if (!is.null(inverseValue)){
        message("getting cached data")
        return(inverseValue)
    }
  
    matrixData = x$get()
    inverseValue = solve(matrixData, ...)
  
    x$setinv(inverseValue)
  
    return(inverseValue)
}
