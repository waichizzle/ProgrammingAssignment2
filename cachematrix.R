## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    # set the value    
    x <<- y
    # clear the cache 
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Get existing cached inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    ## Cache was not empty, return cached inverse
    message("getting cached data")
    return(m)
  }
  ## Calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  ## Set the inverse
  x$setinverse(m)
  ## Return the inverse
  m
}