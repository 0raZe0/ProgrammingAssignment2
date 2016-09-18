## Below are two functions that are used to create a special "matrix" object
## that stores an invertible matrix vector and cache's its inverse

## makeCacheMatrix creates a list containing functions to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of inverse of the matrix
## 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}

