## 2 functions that cache the inverse of a matrix
## code based on makeVector, but change "mean" for "solve" and apply to matrix

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  #Set the inverse of the matrix 
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set=set, get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  # check the inversed matrix
  m <- x$getInverse()
  # if matrix has been inversed already, return cached data
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  #if not, get the matrix and inverse values
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
