## These two functions allow for the caching (storing locally) of the results of inverting
## a matrix. 

## Returns a list of four functions, used to set and get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## get from the cache
  m <- x$getinv()
  ## if the cache is not empty, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the cache is empty, calculate the answer, store and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
