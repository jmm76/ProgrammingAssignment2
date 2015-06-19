## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## from the assignment: This function/method creates a special "vector" which is list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3.set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCachedInverseMatrix <- function(inverse) m <<- inverse
  getCachedInverseMatrix <- function() m
  list(set = set, get = get,
       setCachedInverseMatrix = setCachedInverseMatrix,
       getCachedInverseMatrix = getCachedInverseMatrix)  
  
}


## cacheSolve returns a cached inversematrix
## the Assignment was only cache the inverse of the matrix, not the matrix itself.
## this method uses makeCacheMatrix-vector to check if inverse has already been calculated. 
cacheSolve <- function(x, ...) {
  
    ## Check if we already have the inverse matrix
    m <- x$getCachedInverseMatrix()
    if(!is.null(m)) {
      message("getting cached inverse matrix")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setCachedInverseMatrix(m)
    ## Return a matrix that is the inverse of 'x'
    m  
}
