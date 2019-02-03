## Matrix inversion require high computation so it is helpful to caching the inverse
## of a matrix rather than compute it again and again.So, for that purpose we are using following two function to 
## cache the inverse of a matrix.

## First function- makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix




makeCacheMatrix <- function(x = matrix()) {
  inverse_1 <- NULL
  set <- function(y) {
    x <<- y
    inverse_1 <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_2) inverse_1 <<- inverse_2
  getinverse <- function() inverse_1
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Second function- cachesolve, it returns the inverse of the matrix. it first 
## if the inverse has already been computed. If yes, it gets the result and skip 
## the computation.If not, it computes the inverse, sets the vale in the cache 
## using setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse_1 <- x$getinverse()
  if(!is.null(inverse_1)) {
    message("getting cached data.")
    return(inverse_1)
  }
  data <- x$get()
  inverse_1 <- solve(data)
  x$setinverse(inverse_1)
  inverse_1
}

