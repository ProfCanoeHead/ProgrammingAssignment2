## makeCacheMatrix returns a named list containing 4 members
##  set, get, setInverse, getInverse
##  the value of each member is assigned to the variables of the same name
##  assigned within makeCacheMatrix
## cacheSolve returns the inverse of the matrix scoped value (<<- solve) 
##  if it exists in the returned value of makeCacheMatrix,
##  otherwise references x$setInverse, as setInverse's value is a function that
##  returns an inverse matrix of the created matrix

## makeCacheMatrix assigns a list to a variable and takes a square matrix as its
##  parameter e.g. x <- matrix( c(5, 1, 0,
##                      3,-1, 2,
##                      4, 0,-1), nrow=3, byrow=TRUE)
##  the <<- encloses or wraps the setInverse member's assigned function
##  essetially creating a private or child function that returns the value of the
##  solve function which is passed the x matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- tryCatch(function(solve) invM <<- solve)
  
  
  getInverse <- function() invM
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes as its parameter the list returned by makeCacheMatrix
##  the value of the list's getInverse member is checked to see if it is NULL
##  if itis not NULL then the wrapped value of setInverse is returned with the
##  indication that the cached data is being returned.
##  otherwise the inverse of the matrix is created and assigned within the list.
## Note: if the matrix used by makeCacheMatrix is constructed as:
##      x <- matrix(1:9, 3, byrow = TRUE)
## solve will return a stop error : 
##    system is computationally singular: reciprocal condition number = 2.59052e-18
## the elipse (...) parameter of cacheDSolve allows a parameter to be passed to 
##  setInverse reference to solve() "...to increase the tolerence of dectecting
##  linear dependencies in the coloumns"
##     cacheSolve(y, tol = 2.59052e-18)
##  with the value of toll being set to the reciprocal number returned in the error
##  message

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  data <- x$get()
  invM <- solve(data, ...)
  x$setInverse(invM)
  invM
}
