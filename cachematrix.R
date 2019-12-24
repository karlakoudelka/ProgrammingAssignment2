## This pair of functions cache the inverse of a matrix

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<-function(y){
    x<<-y
    m<<- NULL
  }
  get<-function() x
  setinverse<-function(solve) m<<-solve
  getinverse<-function() m
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve is a function that creates the inverse of the special "matrix" returned by the
## makeCacheMatrix function above. If the inverse was already calcuated, then the function will retrieve
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinverse(m)
  m
}
