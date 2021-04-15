##These Two Functions makeCacheMatrix 
##Uses set, get, setInverse, getInverse.
makeCacheMatrix <- function(x = matrix()) {
  Vulcan <- NULL
  set <- function(y){
    x <<- y
    Vulcan <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) Vulcan <<- inverse
  getInverse <- function() Vulcan 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##These Funtions are used to return the inverse to matrix
##If inverse wasn't calculated, this gets the outcome and leaps the
##calculation step. Otherwise, it solves for the inverse, sets value
##in cache via the function setInverse.
 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Vulcan <- x$getInverse()
  if(!is.null(Vulcan)){
    message("getting cached data")
    return(Vulcan)
  }
  Premo <- x$get()
  Vulcan <- solve(Premo,...)
  x$setInverse(Vulcan)
  Vulcan
}
