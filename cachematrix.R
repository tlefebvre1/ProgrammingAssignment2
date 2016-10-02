## Programming assignment 2
## Caching the inverse of a matrix

## The function makeCacheMatrix creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL # Initialize the cache
  set <- function(y) { # Here y is a "classical" matrix
    x <<- y
    invx <<- NULL
  }
  get <- function() x # Return the matrix
  setinv <- function(inv) invx <<- inv # Store the inverse matrix inv
  getinv <- function() invx # Return the inverse matrix invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) # Return the list of functions
}


## The function cacheSolve computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
  invx <- x$getinv() # Get the cache content
  if(!is.null(invx)) { # If the inverse matrix has been previously computed
    message("getting cached data")
    return(invx) # Return the cached inverse matrix (and exit the function)
  }
  data <- x$get() # Else get the "classical" matrix
  invx <- solve(data, ...) # Compute its inverse
  x$setinv(invx) # Store the result in the cache for future use
  invx # Immediately return the inverse matrix
}
