## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse_M <- NULL
	set <- function(y) {
		x <<- y
		inverse_M <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)inverse_M <<- inverse
  getinverse <- function() inverse_M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse_M <- x$getinverse()
	if(!is.null(inverse_M)) {
		message("getting cached data")
		return(inverse_M)
  }
  mat <- x$get()
  inverse_M <- solve(mat, ...)
  x$setinverse(inverse_M)
  inverse_M
}
