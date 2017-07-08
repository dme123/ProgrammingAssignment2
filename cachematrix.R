## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that
##can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get <-function() x
  setInverse<- function(inverse) inv <<-inverse
  getInverse() inv
  list(set = set, get = get,
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv < -x$getInverse
        if(!is.null(inv)) { ## if the inverse has already been calculated
          message("getting cached data")
          return(inv) ## get it from cached data
        }
        ## otherwise, calculate inverse
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv  ## Returns a matrix that is the inverse of 'x'
}
