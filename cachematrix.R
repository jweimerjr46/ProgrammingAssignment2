# The makeCacheMatrix function creates a special "matrix",
# which is really a list containing a function to:
#1. set the value of the matrix,
#2. get the value of the matrix,
#3. set the value of the inverse,
#4. and get the value of the inverse.

makeCacheMatrix <- function(x=matrix()) {
      inverse <- NULL
      set <- function(y) {
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inverse <<- solve
      getinverse <- function() inverse
      list(set = set , get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

# cacheSolve calculates the inverse of the special "matrix" 
# created by the makeCacheMatrix function. It first checks 
# to see if the inverse has already been calculated. If so, 
# it gets the inverse from the cache and skips the 
# computation. Otherwise it calculates the inverse of the 
# matrix, and sets the inverse of the matrix via the 
# setinverse function.

cacheSolve <- function(x, ...){
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data, ...)
      x$setinverse(inverse)
      inverse
}