# The makeCacheMatrix function creates a matrix,
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inv <- NULL
  
  ## getter/setter for matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## getter/setter for matrix inverse
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  ## return list of functions for matrix
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

# The cacheSolve function calculates the inverse of the matrix
# the matrix which created with the makeCacheMatrix function.
# It first checks to see if the inverse has already been computed.
# If so, it skips the computation by getting the inverse from the cache
# Else it computes the inverse of the matrix and sets the value of the inverse
# in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}
