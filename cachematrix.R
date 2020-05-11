## The aim of this assignemnet is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix
## The fUnction namely makeCacheMatrix is to create a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## ----------example to check the function----------
## testmat <- makeCacheMatrix()
## testmat$set(matrix(data = (1:20), nrow = 4, ncol = 5))
## testmat$get()
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    1    5    9   13   17
## [2,]    2    6   10   14   18
## [3,]    3    7   11   15   19
## [4,]    4    8   12   16   20

## This fUnction named cacheSolve is to conpute the inverse of the matrix 
## ceated by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


