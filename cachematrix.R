## These functions work to cache the inverse of a matrix, get the cached inverse if one already exists 
## and, if not, compute the inverse of the matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedinv <- NULL
  set <- function(y) {
    x <<- y
    cachedinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cachedinv <<- inv
  getinv <- function() cachedinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invfunc <- x$getinv()
  if(!is.null(invfunc)) {
    message("getting cached data")
    return(invfunc)
  }
  data <- x$get()
  invfunc <- solve(data)
  x$setinv(invfunc)
  invfunc
}
