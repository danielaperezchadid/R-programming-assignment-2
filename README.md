# R-programming-assignment-2
## the following code will apply inverse of matrix, first we create cache empty variabel and on next code stablish matrix value.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    d <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) d <<- inverse
  getInverse <- function() d 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

##this function is to obtain inverse in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  d <- x$getInverse()
  if(!is.null(d)){
    message("getting cached data")
    return(d)
  }
  mat <- x$get()
  d <- solve(mat,...)
  x$setInverse(d)
  d
}
