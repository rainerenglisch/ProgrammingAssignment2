## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix returns a list containing functions to set and get
## the matrix (setsolve and getsolve). 
## Additionally it has two functions to store and retrieve
## the inverse of the matrix (set- and getsolve)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ## s stores the inverse of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL ## restets the inverse of the matrix when storing a new matrix
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the special matrix 
## if the inverse of the matrix already cached then
## the inverse if retrieved from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
