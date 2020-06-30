## Two function to calculate iverse matrix with posibility take matrix from cashe

## makeCacheMatrix makes a cpecific structure, which save itself
## deafault matrix (with methods get and set for this), and inverse matrix
## if it has calculate yet
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculate inverse matrix and set it in stucture makeCacheMatrix,
## if it hasn`t calculate yet, or puts and return inverse matrix, which 
## we have calculate yet

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}