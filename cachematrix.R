## This R file contain two functions
## First - makeCacheMatrix: To create a special "matrix" object that can cache its inverse
##Second - cacheSolve: To compute the inverse of the special "matrix" returned by makeCacheMatrix.
##         If the inverse has already been calculated (and the matrix has not changed), 
#          then the cachesolve should retrieve the inverse from the cache.

## --create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  ##get function returns the value of x which is a paramter - matrix in this function
  get <- function() x
  setInverse <- function(inverse) n <<- inverse
  getInverse <- function() n
  ##returns the value of n, NULL at the start
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
    ##getInverse() returns the value of variable n - local scope for makeCacheMatrix
    ## call getInverse() and stored the result in variable n. local to cachesolve
    ## n local to makeCacheMatric copied into n local to cacheSolve

  ##followin code checks if n is NULL, yes at the first call of this function
  ##At the second call, it already will have a value - inverse of a created matrix
  if (!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  ##call get() and store result into a local (to cacheSolve) variable data
  ##see makeCacheMatrix for get()
  ##data - will now contain a matrix passed as a parameter to makeCacheMatrix
  data <- x$get()
  ##solve is a function to solve a system of equation. matrix here
  ##solve function here takes matrix as an input and result will be stored into n
  n <- solve(data, ...)
  ##catch the result stored in n 
  x$setInverse(n)
  ##return n
  n
}
