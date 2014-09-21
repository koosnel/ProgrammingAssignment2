## 
## Programming Assignment 2
## R Programming on Coursera
##

#
# Sample usage:
#
# > source("cachematrix.R")
# > x<-matrix(c(rnorm(16)),4,4)
# > m<-makeCacheMatrix(x)
# > i<-cacheSolve(m)
# > i<-cacheSolve(m)
# getting cached matrix..
#

makeCacheMatrix <- function(x = matrix()) {
  # 
  # This function create the structures to store a 
  # matrix and cached inverse.
  #
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() { 
    x
  }
  
  set_inverse <- function(inverse) {
    i <<- inverse
  }
  
  get_inverse <- function() { 
    i
  }
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(x, ...) {
  #
  # This function calculate the inverse of a matrix and store it 
  # for optimisation purposes.
  #
  # Return a matrix that is the inverse of 'x'
  #
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("getting cached matrix..")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
