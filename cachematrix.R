## This function creates a matrix that has the ability to cache the matrix inverse. 

makeCacheMatrix <- function(x = matrix()) {
## @x: a square invertible matrix
## return: a list containing functions
  inv = NULL
  set = function(y) {

  x <<- y
  inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This fuction computes the inverse that is created by makeCacheMatrix().

cacheSolve <- function(x, ...) {
## @x: output of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
       
  inv = x$getinv()
  if (!is.null(inv)){
  message("getting cached data")
  return(inv)
}
         
  mat.data = x$get()
  inv = solve(mat.data, ...)
           
  x$setinv(inv)
           
  return(inv)
}
