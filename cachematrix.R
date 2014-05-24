# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list of 4 functions to
# 1. set the value of the matrix and reinitialize inv so cache is empty
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  # Inititalize the inv so that we will not get an error when we check for NULL or not in CacheSolve function.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  # vector containing all 4 functions
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(mtrx, ...) {
  
  # get inverse.
  
  myinv <- mtrx$getinverse()
  # Check to see if inverse is already in cache.
  # If it is in cache, the value is not NULL and return inverse
  
  if(!is.null(myinv)) {
    message("getting cached data.")
    return(myinv)
  }
  # This is the first call to solve to set the inverse
  data <- mtrx$get()
  myinv <- solve(data)
  mtrx$setinverse(myinv)
  myinv
}