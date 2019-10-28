## Put comments here that give an overall description of what your
## functions do


## The makeCacheMatrix function creates a R object that 
## stores a matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(mean) m <<- mean
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## the cacheSolve function requires an argument that is the
## result of makeCacheMatrix function, if the inverse of the 
## cached value has stored in makeCacheMatrix(), the cacheSolve()
## will return to the inverse directly and skip the computation,
## Otherwise, it will compute the inverse.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
