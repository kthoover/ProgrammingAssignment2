## These functions take advantage of dynamic scoping to store the inverse of a matrix 
## and avoid recalculation if the matrix doesn't change and the inverse already exists.
## The idea is based on examples for mean of a vector provided by R. Peng.
## Starter code also provdied by R. Peng


# Function uses dynamic scoping to cache a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

# Function checks to see if the invese of a matrix already  exits.  If so uses it, if not creates it. 

cacheSolve <- function(x, ...) {
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
