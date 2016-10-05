## This code can find the inverse of the matrix ("solve" the matrix).  This
## process is computationally expensive, so it is beneficial to cache the
## result so if other code later calls for the matrix' inverse, it can be
## recalled without performing the computation a second time.  This code
## implements just such a cacheing scheme.

## The makeCacheMatrix function creates a list of functions that:
## 1. Set the value of a matrix
## 2. Get the value of a matrix
## 3. Set the inverse of a matrix (i.e. solve the matrix)
## 4. Get the inverse of a matrix (i.e. the solution of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## CacheSolve will solve the matrix passed to it.  If it's already been solved
## and cached, it'll skip the calculation and return the solution

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
