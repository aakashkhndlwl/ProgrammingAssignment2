## There are 2 main functions involved in thsi set of code. The
## function makeCacheMatrix is used to create a special "matrix"
## object which is really a list containing functions to set the
## value of the matrix, get the value of the matrix, set the value
## of the inverse and get the value of the inverse respectively.
## m is the object which stores the inverse calculated using the
## solve function.
## The second function cacheSolve calculates the inverse of the
## matrix created with the above function. However, it first checks
## to see if the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the computation. It
## does print a message "getting cached data" along with the object
## returned. Otherwise, it calculates the inverse of the data using 
## the solve function and sets the value of the inverse in the cache
## via the setInverse function.
## To use these function in the console, I prefer to store the funtion
## makeCacheMatrixin a variable, say a. Then I run a$set(matrix) to
## store a matrix. Now, if I run cacheSolve(a), it returns the inverse
## of the given matrix.


## This function creates a special "matrix" object that can cache
## it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  setMatrix<-function(y){
    x<<-y
    m<<-NULL
  }
  getMatrix<-function() x
  setInverse<-function(solve) m<<- solve
  getInverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$getMatrix()
  m<-solve(matrix, ...)
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}