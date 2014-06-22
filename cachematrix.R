## This function is to cache the inverse of a matrix, so we do not
##    need to repeat a costly computation if can be avoided.

## Create a special 'matrix' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL                                     ## initialise
  set <- function(y) {                                  ## sets the matrix
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x                                   ## get the matrix
  setInverse <- function(solve) invMatrix <<- solve     ## set inverse into invMatrix
  getInverse <- function() invMatrix                    ## get the inverse result
  list (set=set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Computes the inverse of the special matrix returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
##    then cacheSolve should retrieve the inverse from the cache 

cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix) && x== x$get()){ 
    message ("getting cached data")                     ## if cache exists non-empty, we have a cache value
    return (invMatrix)                                  ## exits the function after fetching cached (no else needed after)
  }                                                     ## if it was not calculated we go on
  matrix <- x$get()
  invMatrix <- solve(matrix)                            ## inverse the matrix               
  x$setInverse(invMatrix)                               ## set inverse result in cache
  return(invMatrix)
}
