## I wrote two functions to find the inverse of a invertible matrix. Once the inverse matrix has been stored to cache,
## I can retrieve it directly and save the time to recalculate it.


############# First Function #####################################
## Input a matrix, and return a list of functions that can:
## 1. set the value of the matrix.
## 2. get the value of the matrix.
## 3. set the value of the inverse matrix.
## 4. get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

############# Second Function ###################################
## Find the inverse matrix of 'x', if its inverse matrix has been stored in cache, function just retrieves the value;
## otherwise, function reclaculates its inverse matrix by using the 'solve' built-in function.

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
