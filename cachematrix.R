## This file caches a matrix and caches its inverse
#makeCacheMatrix contains 4 functions which sets the value of a matrix and its inverse and returns (gets) them.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #Function 1, sets the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  #Function 2, returns the original matrix
  get <- function() x
  #Function 3, used in conjunction with cacheSolve, stores the value of its inverse
  setInverse <- function(solve) m <<- solve
  #Function 4, returns the inverse 
  getInverse <- function() m
  #function returns a list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the matrix
## however first of all it checks to see if the inverse is already calulated
## if it is it skips the computation and returns the inverse
## else it performs the calculation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Check to see if the inverse is already calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #Calculates the inverse
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

#Basic example of how to use it shown below
#creates matrix
a <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
#assigns first function to b
b <- makeCacheMatrix()
#caches the first vector
b$set(a)
b$get()

#calculates the inverse
cacheSolve(b) 

#stores the inverse
b$setInverse <- makeCacheMatrix(cacheSolve(b))
b$getInverse()

#See if the check is working
cacheSolve(b)
