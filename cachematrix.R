## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
