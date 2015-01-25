## A pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inversion <- NULL
  set <- function(y) {
    x <<- y
    inversion <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inversion <<- solve
  getinverse <- function() inversion
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inversion <- x$getinverse()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$setinverse(inversion)
  inversion
}
