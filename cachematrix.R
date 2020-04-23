## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The purpose of the first function is to create a list that further
## has functions to set the matrix and get the matrix and then set the
## inverse and get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The second function gets the inverse from the cache if it has 
## already been calculated otherwise it calculates the inverse and
## then sets it in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
