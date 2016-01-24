## Put comments here that give an overall description of what your
## functions do

2 functions.  The first function creates a special matrix object that supports caching its inverse.  The second function produces the inverse to the matrix object, using the object's cache if set, and otherwise computing the result.

## Write a short comment describing this function
Function returns a matrix object that supports caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolution <- function(solution) m <<- solution
  getsolution <- function() m
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}

## Write a short comment describing this function
Function retrieves the inverse of matrix X, using the cache, and otherwise performs the solve method and then caches the result.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getsolution()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolution(m)
  m
}

## Test
tryme <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
cacheSolve(tryme)
cacheSolve(tryme)

## Test
# > tryme <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# > cacheSolve(tryme)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(tryme)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#
