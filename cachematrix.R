## makeCacheMatrix and cacheSolve work to return the
## inverse of a matrix in an efficient manner by
## caching the inverse of a matrix.

## makeCacheMatrix is a standard setter/getter function
## that sets and gets the inverse of a supplied matrix

makeCacheMatrix <- function(x = matrix())
{
  ## Note that we set by default the inverse
  ## to be the 0 matrix for easy testing of changes
  ## since we assume the matrix to be invertible
  dm <- dim(x)
  xinv <- matrix(numeric(dm^2, dm, dm))
  
  set <- function(y)
  {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setinv <- function(inv)
  {
    xinv <<- inv
  }
  
  getinv <- function()
  {
    xinv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  ## Use the cached value if the matrix has not changed
  ## Note that we test against a default of a zero matrix
  ## to verify whether a solve should be invoked
  xinv <- x$getinv()
  
  if(!(det(xinv) == 0))
  {
    message("Getting cached data")
    
    return(xinv)
  }
  
  data <- x$get()
  
  minv <- solve(data)
  
  x$setinv(xinv)
  
  xinv
}
