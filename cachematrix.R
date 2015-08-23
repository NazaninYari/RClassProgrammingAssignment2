## Use makeCacheMatrix and cacheSolve to cache the inverse of a matrix.
## Example:
##   mdat <- matrix(c(1,2, 11,12), nrow=2, ncol=2)
##   cached_mdat <- makeCacheMatrix(mdat)
##   cacheSolve(cached_mdat)
##   cacheSolve(cached_mdat)   uses the cached value this time

## Creates a CacheMatrix which takes a matrix and stores the matrix
## along with the cached value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of a CacheMatrix and uses a cached value if available

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
