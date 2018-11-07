## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            cachedInverse <- NULL
            set <- function(y) {
                    x <<- y
                    cachedInverse <<- NULL
            }
            get <- function() x
            setInverse <- function(value) cachedInverse <<- value
            getInverse <- function() cachedInverse
            list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedValue <- x$getInverse()
  if(!is.null(cachedValue)) {
    message("getting cached data")
    return(cachedValue)
  }
  data <- x$get()
  cachedValue <- solve(data);
  x$setInverse(cachedValue)
  cachedValue
}
