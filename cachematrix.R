## Receives aMatrix and constructs a .
##  sequence of functions that can be used to 
##  access the matrix and its (cached) inverse

makeCacheMatrix <- function(aMatrix = matrix()) {
  aCachedInverse <- NULL
  set <- function(originalMatrix) {
    aMatrix <<- originalMatrix
    aCachedInverse <<- NULL
  }
  get <- function() aMatrix
  setinverse <- function(theInverse) aCachedInverse <<- theInverse
  getinverse <- function() aCachedInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of the passed matrix
##   and, if cached, returns from cache rather than computing

cacheSolve <- function(solveable, ...) {
        ## Return a matrix that is the inverse of 'x' 
  inverseOfSolveable <- solveable$getinverse()
  if(!is.null(inverseOfSolveable)) {
    message("getting cached data")
    return(inverseOfSolveable)
  }
  data <- solveable$get()
  inverseOfSolveable <- solve(data)
  solveable$setinverse(inverseOfSolveable)
  inverseOfSolveable
}
