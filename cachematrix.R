## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  
  #set the value of the Matrix
  setMatrix <- function(y) {
    x <<- y
    iMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) iMatrix <<- inverse     #set the value of the inverematrix
  getInverse <- function() iMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrix()

  #if inverse matrix is not NULL, return inverse matrix
  if(!is.null(m)) {
    message("getting cached Inverse Marix")
    return(m)
  }
  
  data <- x$getMatrix()     #get the original Matrix 
  m <- solve(data, ...)     #use solve function to inverse the matrix
  x$setInverse(m)           #set the invere matrix 
  m
}