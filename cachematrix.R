## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##  makeVector creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    I<-NULL
    set<-function(y){
      x<<-y
      I<<-NULL
    }
    get<-function() m
    setinverse<-function(inverse) I<<-inverse
    getinverse<-function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## it checks to see if the inverse exist then it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of a matrix and sets the value of the inverse in the cache via
## the setmean function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if (!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    mat <- x$get()
    I <- solve(mat, ...)
    x$setinverse(I)
    I
}
