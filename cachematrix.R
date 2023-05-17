## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  # set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  # set the value of the inverse
  setinv <- function(inv) i <<- inv
  # get the value of the inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  # first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}
