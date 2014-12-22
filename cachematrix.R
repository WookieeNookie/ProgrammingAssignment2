## makeCacheMatrix and cacheSolve work to return the
## inverse of a matrix in an efficient manner by
## caching the inverse of a matrix.

## makeCacheMatrix is a standard setter/getter function
## that sets and gets the inverse of a supplied matrix

makeCacheMatrix <- function(x = matrix())
{
  ## Set the default inverse to just be a NULL value
  ## to serve as a test case for matrix changes
  xinv <- NULL
  
  ## Set the the matrix passed in
  set <- function(y)
  {
    x <<- y
    xinv <<- NULL
  }
  
  ## Get the matrix we are working with
  get <- function()
  {
    x
  }
  
  ## Set the inverse of the matrix we are working with
  setinv <- function(inv)
  {
    xinv <<- inv
  }
  
  ## Get the inverse of the matrix we are working with
  getinv <- function()
  {
    xinv
  }
  
  ## Return the list of relevant values
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the matrix of interest
## but if it has been solved previously, it uses the cached value

cacheSolve <- function(x, ...)
{
  ## Return a matrix that is the inverse of 'x'
  ## Use the cached value if the matrix has not changed
  xinv <- x$getinv()
  
  ## If the inverse is already known, just return it
  if(!is.null(xinv))
  {
    message("Getting cached data")
    
    return(xinv)
  }
  
  ## Otherwise, get the matrix of interest
  data <- x$get()
  
  ## Then solve for the inverse
  xinv <- solve(data)
  
  ## Set the inverse for the matrix of interest
  x$setinv(xinv)
  
  ## Return the new inverse
  xinv
}
