## The following two functions can cache the inverse of a given matrix

## Firstly, makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  I<-NULL
  set<-function(y){
    x<<-y
    I<--NULL
  }
  get<-function() x
  setinv<-function(solve) I<<-solve
  getinv<-function() I
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## The second one computes the inverse of the "matrix" given by
## makeCacheMatrix above. Note that, if the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve will show the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I<-x$getinv()
  if(!is.null(I)){
    message("getting cached data")
    return(I)
  }
  data<-x$get()
  I<-solve(data,...)
  x$setinv(I)
  I
}