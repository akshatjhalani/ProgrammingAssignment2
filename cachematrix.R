## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix function creates a matrix object and cache its inverse 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this function computes inverse of the matrix through following ways:-
## 1) if inverse already present in cache then no computation is done and the cache value is returned
## 2) in case of new matrix first inverse is computed and then cahced for future use.

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

