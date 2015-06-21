## Functions to cache the inverse of a matrix to avoid recomputation
## of inverse of the same matcrix again and again

## Take a input matrix and returns a list with functions to get
## and set the matrix as well as it inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## This function takes input as the list returned by the makeCacheMatrix function
## and retruns the inverse of the matrix returned by the get function 
## on the list. If it does not find a cached inverse matrix it computes
## the matrix inverse and stores it in the cache else returns the cached
## inverse value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<- x$getinverse()
  if(!is.null(i)){
    message("Getting cached data")
    return (i)
  }
  y<- x$get()
  
  i<- solve(y,...)
  x$setinverse(i) 
  i
}
