## A pair of functions that cache the inverse of a matrix. If the inverse has been
## calculated, it can be retrieved from cache rather than recalculates.

## makeCacheMatrix function creates a special matrix. This function returns four functions
## that can set/get the matrix and set/get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      invmtx <- NULL
      set <- function(y){
        x <<- y
        invmtx <<- NULL
      }
      get <- function() x
      setinvmtx <-function(inversemtx) invmtx <<- inversemtx
      getinvmtx <-function() invmtx
      list(set = set, get = get, 
           setinvmtx = setinvmtx, getinvmtx = getinvmtx)
}


## cacheSlove function computes the inverse of the matrix which is returned by 
## makeCacheMatrix. If the inverse has been calculated, this function retrieves 
## the inverse from cache rather than recomputes.

cacheSolve <- function(x, ...) {
      invmtx <- x$getinvmtx()
      if(!is.null(invmtx)){
        message("Getting cache inverse matrix")
        return(invmtx)
      }
      data <- x$get()
      invmtx <- solve(data)
      x$setinvmtx(invmtx)
      invmtx    ## Return a matrix that is the inverse of 'x'
}
