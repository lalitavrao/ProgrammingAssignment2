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
cacheSolve <- function(mx, ...) {
  
  # get inverse.   
  
  inv1 <- mx$getinverse()
  # Check to see if inverse is already in cache.
  #  If it is in cache, the value is not NULL and return inverse
  
  if(!is.null(inv1)) {
    message("getting cached data.")
    return(inv1)
  }
  # This is the first call to solve to set the inverse
  data <- mx$get()
  inv1 <- solve(data)
  mx$setinverse(inv1)
  inv1
}

#Results
# > m <- rbind(c(1,2), c(2,1))
# > m
#      [,1] [,2]
# [1,]    1    2
# [2,]    2    1
# > l = makeCacheMatrix(m)
# > l$get()
#      [,1] [,2]
# [1,]    1    2
# [2,]    2    1
# First time we run no data in cache
# > cacheSolve(l)
#       [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# Second run value is in cache
# > cacheSolve(l)
# getting cached data.
#       [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# Third run value is still in cache
# > cacheSolve(l)
# getting cached data.
#        [,1]       [,2]
# [1,] -0.3333333  0.6666667
# [2,]  0.6666667 -0.3333333
# try with 3x3 matrix
# > z <- rbind(c(1,2,3), c(0,1,4), c(5,6,0))
# > z
# [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# Set the new value to the matrix
# Cache is cleared
# > l$set(z)
# > l$get()
#       [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    0    1    4
# [3,]    5    6    0
# Frist call to get the inverse
# > cacheSolve(l)
#       [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > cacheSolve(l)
# getting cached data.
#       [,1] [,2] [,3]
# [1,]  -24   18    5
# [2,]   20  -15   -4
# [3,]   -5    4    1
# > 
