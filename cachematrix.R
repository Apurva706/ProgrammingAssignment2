## Matrix Inverse is a computationally expensive procedure
## For efficient computation, making a function that can 
## take cached inverse value, if available

## First function computes the inverse of matrix and outputs
## a list which has inverse and the environment values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(mean) m <<- solve(x)
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}



## CacheSolve tries to find if a cached value exists
## and outputs value from cache if available
## thus improving efficiency

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
  
}

