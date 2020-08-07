## Create matrix with methods to calculate/store/access its inverse

## makeCacheMatrix: creates initial matrix and defines functions to
## get/set itself and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) {
    inv <<- i
  }
  getInv <- function() inv
  list(set = set, get = get, setInv =  setInv, getInv = getInv)
}


## cacheSolve: either calculates or returns the cached value of the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  i <- solve(x$get(), ...)
  x$setInv()
  i
}
