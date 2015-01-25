## My functions create unique environments (with lexical scoping) to :-
## i. cache the inverse values of matrices
## ii. compute the inverse values of matrices
## iii. check in the cache for inverse values that have been computed before and return those quickly 
## iv. compute and return inverse values that have not been computed before

## This first function (makeCacheMatrix) creates a special matrix object - a list - that can set, get and cache the inverse
## values of matrices. It uses the <<- operator that is used to assign values to variables globally.
## The function that sets the inverse of matrices is solve().

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This second function (cacheMatrix) returns the inverse of a matrix passed through the function above (makeCacheMatrix).
## If the inverse value has already been computed before, it checks in the cache and returns the value quickly.
## If not, it computes and returns the inverse value of the matrix passed through makeCacheMatrix with a normal time delay.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
