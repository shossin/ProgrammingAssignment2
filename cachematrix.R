## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

y <- function(x = matrix()) {
  
  ## initializing inverse matrix cache variable
  m <- NULL
  
  ## cache matrix variable and reset inverse matrix
  ## in the cache 
  ## every time matrix variable changes
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## get the matrix from cache
  get <- function() x
  
  ## cache the inverse matrix
  setinverse <- function(invmatrix) m <<- invmatrix
  
  ## get the inverse matrix from cache
  getinverse <- function() m
  
  ## returning list of functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## obtaining the inverse matrix.
  ## NULL is returned if not yet cached
  m <- x$getinverse()
  
  ## if inverse matrix is in cache
  ## return cached inverse matrix without computing again
  if(!is.null(m)){
    message("getting cashed data")
    ## return and terminate function
    return(m)
  }
  
  ## compute and cache inverse matrix for
  ## the first time
  
  ## get the matrix from cache
  data <- x$get()
  ## compute inverse matrix
  m <- solve(data, ...)
  ## cache inverse matrix
  x$setinverse(m)
  ## return inverse matrix
  m
}