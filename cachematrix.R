# Use makeCacheMatrix to make a matrix
makeCacheMatrix <- function (x=matrix())
{
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
    
  }
  get <-function() x
  setInverse <-function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get= get, setInverse=setInverse, getInverse= getInverse)
  
}
# cacheSolve function is used to get the inverse of the matrix
cacheSolve <-function(x,...)
{
  inv <- x$getInverse()
  # Check to see if the inverse of the matrix is already calculated
  if(!is.null(inv))
  {
    message("getting cached data")
    return (inv)
  }
  matr <- x$get()
  inv <- solve(matr,...)
  x$setInverse(inv)
  inv
}




