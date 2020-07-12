## Function 1 and 2 will cache the inverse of a matrix.

## Function 1:  Creates a special "matrix" object that can cache its inverse.

makeCasheMatrix <- function(x = matrix) {
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



## Function 2: Computes the inverse of the special "matrix" returned by Function 1 (makeCacheMatrix). If the inverse has already been calculated, then it should retrieve the inverse from the cache.

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
