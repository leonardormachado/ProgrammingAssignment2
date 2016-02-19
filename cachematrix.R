## Assignment by LEONARDO MACHADO for COURSERA - R PROGRAMMING -s on Feb-18
## The purpose of this assignment is to create functions that can help saving
## processing time when calculating the inverse of matrices

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  matrixInverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    matrixInverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) matrixInverse <<- inverse
  getInverse <- function() matrixInverse
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  matrixInverse <- x$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data of the inversed matrix")
    return(matrixInverse)
  }
  data <- x$getMatrix()
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}


## This commands were used to test the functions

## Creating a matrix and making sure it is invertable
set.seed(1027)
testMatrix <- matrix( rnorm(400), nrow=20, ncol=20 )
testMatrix
solve(testMatrix)

## Testing the functions
specialMatrix <- makeCacheMatrix()
specialMatrix$setMatrix(testMatrix)
cacheSolve(specialMatrix)
cacheSolve(specialMatrix)








