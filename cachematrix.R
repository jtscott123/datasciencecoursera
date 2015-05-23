makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse
  ## This list is used as the input to cacheSolve()
  m<-NULL
  set<-function(y){
  # <<- Assigns a value to an object in a different environment 
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    ## This function computes the inverse of the special matrix returned by makeCacheMatrix
    m<-x$getmatrix()
    # If the inverse has already been calculated
    if(!is.null(m)){
      # Get it from the cache
      message("getting cached data")
      return(m)
    }
    # Calculate and return the inverse 
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    return(m)
}
