## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <- i
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
  }


cacheSolve <- function(x, ...) 
  ## Return a matrix that is the inverse of 'x'
  {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
