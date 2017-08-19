## These two functions create a matrix (that can be inversed) and caches its inverse.
## If the inverse has not already been solved, the second function will solve it and
## set the value of the inv in the cache.

## Creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     mat.inv = NULL
     set = function(y) {
          x <<- y
          mat.inv <<- NULL
     }
     get = function() x
     setinv = function(inverse) mat.inv <<- inverse
     getinv = function() mat.inv
     list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## If the inverse is already cached, this function will retrieve it.
## Input for this function is the output list from makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     mat.inv = x$getinv()
     
     if(!is.null(mat.inv)) {
          message("Getting cached data")
          return(mat.inv)
     }
     data <- x$get()
     mat.inv <- solve(data, ...)
     x$setinv(mat.inv)
     mat.inv
}
