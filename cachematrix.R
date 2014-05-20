## makeCacheMatrix creates our new cache-able matrix
## cacheSolve (weird name) will pull the result from the cache if available,
## or solve if nothing is cached

makeCacheMatrix <- function(x = matrix()) {
  
  ##create our cached inversed
  inverseOfX <- NULL
  
  ##use the <<- to *set the values in 'the other' environment
  set <- function(y) {
    x <<- y
    inverseOfX <<- NULL
  }
  
  ##create the *get 
  get <- function() x
  
  ##*set the inverse
  setInverse <- function(inverse) inverseOfX <<- inverse
  
  ##*get the inverse
  getInverse <- function() inverseOfX
  
  ##now return a list with these values
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## cacheSolve will take a special cache-able matrix 'x' created with
## makeCacheMatrix, and return the inverse of 'this 'x'. The inverse
## of 'x' will be pulled from the cache, if available, rather than solved
## to increase performance, and to pass the assignment. Mostly the 2nd.

cacheSolve <- function(x, ...) {
  
  ## get the cached inverse
  inverseOfX <- x$getInverse()
  
  ##solve if the cahced inverse is null
  if (is.null(inverseOfX)) {
    inverseOfX <- solve(x$get())
    x$setInverse(inverseOfX)
  } else {
    message("using cached matrix")
  }
  
  return inverseOfX
}
