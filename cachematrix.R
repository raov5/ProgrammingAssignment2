#@author: Venky Rao venkatesh.rao@gmail.com
#@last update: 11 Feb 2018
#Coursera Data Science Specialization
#R Programming
#Programming Assignment 2: cachematrix.R

## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## Here are a pair of functions that cache the inverse of a matrix.

## The first function is "makeCacheMatrix".
## This function creates a special "matrix" object
##    that can cache its inverse.
## Assumption: the matrix is always invertible.
## This function makeCacheMatrix creates a special "matrix",
##    which is really a list containing a function to:
## * set the value of the matrix
## * get the value of the matrix
## * set the inverse of the matrix
## * get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function is "cacheSolve".
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cachesolve retrieves the inverse from the cache.
## Computing the inverse of a square matrix can be done with the
## solve function in R. For example, if X is a square invertible
## matrix, then solve(X) returns its inverse.
## Assumption: the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

## Sample run:
x <- rbind(c(1, -1/2), c(-1/2, 1))
m <- makeCacheMatrix(x)
m$get()

##       [,1]  [,2]
## [1,]  1.0 -0.5
## [2,] -0.5   1.0

## No cache in the first run
n <- cacheSolve(m)
n
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## Retrieving from the cache in the second run
cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333
