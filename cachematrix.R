## This R file contains two functions for caching the inverse of Matrix 
##submitted as part of coursera R-programming Assignment 2


#This function, makeCacheMatrix creates a special matrix, which is really a matrix containing a function to
#set the value of the matrix
#get the value of the matrix
#setinverse the value of the inverse
#getinverse the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
          if(!is.null(i)) {
            message("getting cached data")
            return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
}
