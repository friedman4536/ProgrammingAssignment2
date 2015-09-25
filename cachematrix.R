## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Set the solved version of the matrix to NULL.
  s <- NULL
  # If a new matrix is passed, reset the solved version to NULL.
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  # Get the stored matrix.
  get <- function() x
  # Set the stored solved version of the matrix.
  setsolved <- function(solved) s <<- solved
  # Get the stored solved version of the matrix
  getsolved <- function() s
  # Return a list of the get, set, getsolved, and setsolved functions.
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolved()
  # If there already a stored solved version of the matrix,
  # return it.
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  # Get the stored matrix.
  data <- x$get()
  # Get the solved (inverse?) version of the matrix.
  s <- solve(data, ...)
  # Store the solved version of the matrix.
  x$setsolved(s)
  # Return the solved version of the matrix.
  s
}