## R Programming: Week 3 Programming Assignment
## Goal: Use lexical scoping within functions to solve/invert a matrix, cache it, then retrieve it. 

## Function to hold / make the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(solve) m <<- solve
  getCache <- function() m
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}

## Function to solve(aka invert) a matrix
cacheSolve <- function(x, ...) {
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m
}

## Sample matrix from discussion board
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

## Place the above matrix into the first function
aMatrix <- makeCacheMatrix(m1)

## Solve(invert) matrix and cache it
cacheSolve(aMatrix)

## Retrieve the cached matrix
cacheSolve(aMatrix)