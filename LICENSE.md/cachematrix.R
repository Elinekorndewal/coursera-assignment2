# The functions below enable to store the calculated inverse of a matrix. 
# The inverse can be retrieved at a later point of time, so that the inverse does not have to be calculated again.

## The function below sets the environment for the matrix of which you would like to (repeatedly) use the inverse. 
## Enter the matrix of which you would like to calculate the inverse as argument x, assign the result of this function to a separate variable.
## The result a list in global environment, this list is needed in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## This function either calculates the inverse of a matrix or retrieve a previously calculated inverse from the cache.
## Enter the result of the makeCacheMatrix as argument x.
## The result of this function is the inverse of the matrix that you entered in the makeCacheMatrix.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}



