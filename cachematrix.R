## These two functions are designed to store and retreive the inverse of a 
## provided matrix. IF the matrix has not changed from previously provided, 
## then the functions return the inverse from the cache.  IF the matrix has
## changed from previously provided, then the functions calculate, store, 
## and return the new inverse,


## Create a function that contains four functions (set, get, setinv, getinv) 
## to be used in cachesolve, below.

makeCacheMatrix <- function(x = matrix()) {
            v <- NULL
            set <- function(y) {
                  x <<- y
                  v <<- NULL
            }
            get <- function() x
            setinv <- function(solve) v <<- solve
            getinv <- function() v
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
      }


## Create a function that returns either the cached inverse matrix or 
## calculates the new inverse matrix, if the provided matrix has changed,
## i.e., if makeCacheMatrix or $set has run and changed v to null.

cachesolve <- function(x, ...) {
      v <- x$getinv()
      if(!is.null(v)) {
            message("getting cached inverse matrix")
            return(v)
      }
      data <- x$get()
      v <- solve(data, ...)
      x$setinv(v)
      v
}