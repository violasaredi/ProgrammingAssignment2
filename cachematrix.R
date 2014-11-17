## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( m = matrix() ) {
  
  ## Inverse
  inv <- NULL
  
  ## Set the value of the matrix
  set <- function(matrix) {
    m <<- matrix
    inv <<- NULL
  }
  
  ##Get the value of the matrix
  get <- function() m
  
  ##Set the inverse of the matrix
  setInv <- function(inverse) {
    inv <<- inverse
  }
  
  ## Get the inverse of the matrix
  getInv <- function() inv
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
  
  ## Just return the inverse if it has already been calculated
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse by means of solve function
  inv <- solve(data)
  
  ## Set the inverse to the object
  x$setInv(inv)
  
  ## Return the inverse matrix
  inv
}