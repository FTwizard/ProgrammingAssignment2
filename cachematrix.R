## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when the matrix changes
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Compute the inverse of the special "matrix" and cache the result
cacheSolve <- function(cacheMatrix) {
  inv <- cacheMatrix$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  mat <- cacheMatrix$get()
  inv <- solve(mat)
  cacheMatrix$setInverse(inv)
  inv
}
# Create a matrix
mat <- matrix(c(4, 2, 2, 5), nrow = 2)

# Create a cacheMatrix object
cacheMat <- makeCacheMatrix(mat)

# Compute and cache the inverse
inverse <- cacheSolve(cacheMat)

# Retrieve the cached inverse
cached_inverse <- cacheSolve(cacheMat)

##The makeCacheMatrix creates a special matrix object that can cache its inverse, 
##and cacheSolve computes the inverse of the matrix and caches the result. 
##If the inverse has already been computed, cacheSolve retrieves it from the cache.

