## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function() m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=makeCacheMatrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setmatrix(m)
  return(m)
}

## After doing this assignment, I've tried to run it, and even when it should be working, it doesn't.
## Also, I tried to run the example (Caching the Mean of a Vector), and it doesn't work either.
## I looked for help in Stackoverflow, but it didn't help.
## Pd.: I'm running Rstudio with the R version 3.0.2 in a x86_64-pc-linux-gnu.