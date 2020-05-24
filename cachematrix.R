
## In this Programming Assignment will take advantage of the scoping rules of 
## the R language and how they can be manipulated to preserve state inside of 
## an R object.


## Basically we take the example code and replace "mean" with inverse/solve fun


## makeCacheMatrix - This function creates a special "matrix" object
## that can cache its inverse.

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

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


## Checking my code output...

## m <- matrix(rnorm(16),4,4)
## > m1 <- makeCacheMatrix(m)
## > cacheSolve(m1)
## [,1]      [,2]      [,3]       [,4]
## [1,]  0.1120418 -0.247946 0.9664689 -0.3282881
## [2,] -0.2612418 -1.340417 0.2954361  1.8862929
## [3,] -0.2664026  0.589900 0.6192944 -0.4517884
## [4,] -0.1919612  0.510074 1.8581911  1.6820573

## It outputs. Woohoo.