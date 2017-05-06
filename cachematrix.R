## Caching matrix inverse
## the following two functions calculate the inverse of a matrix and
## try to cache this inverse to save a lot of computation time calculating the
## inverse each time specially for large matrices

## the following function creates a matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## this function calculates the inverse of the given matrix but
## in case of caching the inverse matrix, it returns the cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinverse(inv)
  return(inv)
}
