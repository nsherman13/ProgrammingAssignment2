## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function will create a special matrix. It will return a matrix with
# a list of commands such as setting and getting the matrix, and setting
# and getting the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Cache Solve checks and sees if inverse is already stored in a special matrix
#(made by makeCacheMatrix). If so, it returns cached copy, otherwise it will
# calculate and store in cache the matrix's inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}