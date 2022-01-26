
# Part 1:
#makeCacheMatrix: This function creates a special "matrix" object that can cache 
#its inverse. 
#1.set the value of matrix
#2.get the value of matrix
#3.setInverse sets the value of matrix
#4.getInverse returns the value of matrix
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the value of i
  i <- NULL
  # Method to set the matrix
  set <- function( matrix ) { m <<- matrix
  i <<- NULL
  }
  # Method the get the matrix
  get <- function() {m}
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) {i <<- inverse}
  # Method to get the inverse of the matrix
  getInverse <- function() {i}
  # Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
# Part 2: 
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  # Return an inverse matrix 
  m <- x$getInverse()
  # Returns the inverse if set already
  if(!is.null(m)) { message("getting cached data")
    return(m)
  }
  # Get the matrix 
  cacheData <- x$get()
  # Calculate the inverse matrix by using multiplication matrix
  m <- solve(cacheData) %*% cacheData
  # Set the inverse to the object
  x$setInverse(m)
  # Return the matrix
  m
}