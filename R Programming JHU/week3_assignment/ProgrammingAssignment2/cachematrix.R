## `makeCacheMatrix(x=matrix())` takes in a matrix object and returns a list of special setters and getters
## `cacheSolve(x, ...)` takes in a special matrix objecg and returns the calculated (new or from cache) inverse matrix

## Returns a list of:
## 1) setter for the matrix object
## 2) getter for the matrix object
## 3) setter for calculating Inverse
## 4) getter for calculating Inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()
    x

  setInverse <- function(inverse)
    inv <<- inverse
  getInverse <- function()
    inv

  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Returns a matrix that is the inverse of 'x'
## initialize a matrix
## check if inverse value of that matrix object already exists
## if not calculate the inverse matrix and set the object value
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
