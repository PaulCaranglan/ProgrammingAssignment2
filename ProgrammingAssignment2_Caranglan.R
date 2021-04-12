##makeCacheMatrix and makeCacheMatrix
##makeCacheMatrix contains set,get,setinverse,getiverse
makeCacheMatrix <- function (x = matrix()) { 
    P <- NULL 
    set <- function (y) {
      x <<- y 
      P <<- NULL
      
      cacheSolve <- function (x,...){
        P <- x$getInverse()
        if(!is.null(P)) {
          message ("geting cached data!")
          return(P)
        }
        mat<- x$get()
        P <- solve (matrix, ...)
        x$setO(P)
        P
      }
      