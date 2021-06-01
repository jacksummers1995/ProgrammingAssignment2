
## --- These Functions calculates the inverse of a matrix and saves it output for later, so it does not need to be recalculated

## This Function creates a list object with four functions.
## You can set & get the matrix and also set & get the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  return(
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  )
  
}


## This Function will first check to see if the inverse of x has already been calculated.
## If it has, it will return the value from the cache rather than recalculated it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}
