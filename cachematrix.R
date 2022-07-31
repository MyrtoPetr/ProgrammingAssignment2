## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function is used to create a matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ##inv has a starting value of NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x ##getting matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
##The following function calculates and returns the inversed x matrix from above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) { ##checking if inversion is NULL
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...) ##calculations to invert x
  x$setInverse(inv)
  inv ##returns of the reversed x matrix
}
