## The following functions can compute the inverse of a matrix if it has not been computed and cache the inverse of the matrix if it has already been computed.

## The function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Sets inv to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } ## Sets matrix
  get <- function() x ## Gets matrix.
  setinverse <- function(inverse) inv <<- inverse ## Sets inverse matrix.
  getinverse <- function() inv ## Gets inverse matrix.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## Creates a list of 4 functions.
}


## The function below compute the inverse matrix of the matrix returned by the above function. 
## If the inverse matrix has already been calculated, it will return the retrieve the inverse matrix from the cache and will not compute the inverse matrix again.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ## Print "getting cached data" and return the cached inverse matrix of x if it has already been calculated.
  }
  ## If the inverse matrix has not been calculated:
  data <- x$get()
  inv <- solve(data, ...) ## Calculate the inverse matrix of x using "solve".
  x$setinverse(inv) ## Sets the value of the inverse matrix in the cache.
  inv
}
