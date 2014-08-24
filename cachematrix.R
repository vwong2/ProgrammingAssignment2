## The inverse of a matrix will be cached to memory
## to reduce the need to recompute
## 
## Usage example (for input matrix x):
## m <- makeCacheMatrix <- function(x)
## cacheSolve(m)
## 
## The cache matrix object can be operated on as
## m$set(x) 


## Returns the cache matrix which is a list of functions returning or changing
## values of the matrix or its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  
  ## setting the value of x will reset the inverse value
  set <- function(y)
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  ## the value of the invmat can be set using this function
  setinv <- function(inv) invmat <<- inv
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Takes the matrix from the cache matrix created in makeCacheMatrix,
## calculates its inverse, and stores it to memory in the cache matrix.
## x is a cache matrix object created in makeCacheMatrix, not an actual
## matrix object.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  
  ## the inverse will be calculated if the inverse is not NULL
  ## else its current value will be returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  
  ## solve is used to invert the matrix
  m <- solve(data)
  x$setinv(m)
  m
}
