# makeCacheMatrix creates a special "matrix" object / returns a list containing a function to set the value of the matrix, get the value of the matrix, set its inverse and get the inverse.
# cacheSolve in turn calculates the inverse of the special "matrix" created with the above function but before calculating, it checks to see if the value in store in the cache. 
# If so, it directly returns the cached version without wasting any effort.

## makeCacheMatrix returns a list of four functions wrapped in a list. set assigns the input value to variable x. 
##get returns x. setInverse calculates the inverse and stores in in variable invers. getInverse returns the inversed value. 

makeCacheMatrix <- function(x = matrix()) {

  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invers <<- solve
  getInverse <- function() invers
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}





##This function returns either the cached inverse or freshly calculated inverse of x. 
## It first checks to see whether the inverse has been previously caculated. It so, it return the inverse directly. Otherwise, it calculates and returns the inverse.

cacheSolve <- function(x, ...) {
  
  ##first check if the invers exists.
  invers <- x$getInverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  
  ##if invers does not exist, then calculate using solve. 
  data <- x$get()
  invers <- solve(data, ...)
  x$setInverse(invers)
  invers
}


