# The combination of functions below compute the inverse of an invertible matrix,
# but they only actually do the computations if the functions haven't been applied to this particular
# matrix before.

# The first part, makeCacheMatrix, takes a matrix and gives back a list of 4 functions: 
# 1) set: puts the input matrix in the cache 2) get: returns the input matrix 
# 3) setinverse: puts the inverse, once computed, in the cache 4) getinverse: returns the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set,get=get,setinverse = setinverse,getinverse=getinverse)
}

## The second part, cacheSolve, first checks if the inverse of the matrix has been computed 
# before (that is, if cacheSolve has been applied to this matrix before). If so, it takes
# the inverse from the cache. Otherwise, it computes the inverse with the regular 'solve' function. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

# This is how I tested if my functions worked.
A <- matrix(1:4,nrow=2,ncol=2)
A2 <- makeCacheMatrix(A)
cacheSolve(A2)
cacheSolve(A2)