# Programming assignment 2 ------------------------------------------------

## Add matrix into cache

makeCacheMatrix <- function(x = matrix()) {
  
  ## Check if the matrix is square
  if (dim(x)[1] != dim(x)[2]) {
    message('Input square matrix')
  }
  
  ## Initialize a variable that will hold the value of invert matrix
  invM <- NULL
  
  ## Define a function that will assign new matrix in parent environment
  setM <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  ## Get the value of the matrix
  getM <- function() x
  
  ## Set the value of inverse matrix
  setInvM <- function(solve) invM <<- solve
  
  ## Get the value of inverse matrix
  getInvM <- function() invM
  list(
    setM = setM, getM = getM,
    setInvM = setInvM, getInvM = getInvM
  )
}


## Solve inverse of square matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getInvM()
  
  ## For solved matrix return the value from cache
  if(!is.null(invM)){
    message('Using cached data')
    return(invM)
  }
  
  ## If the matrix is new solve inverse new matrix
  data <- x$getM()
  invM <- solve(data, ...)
  x$setInvM(invM)
  invM
}