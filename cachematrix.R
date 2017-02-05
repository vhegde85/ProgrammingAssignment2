## Function to perform caching the inverse of a matrix

# The first function, makeCacheMatrix creates a 
# special "vector", which is really a list containing a function to

#1. set the value of the matrix. Assumes that matrix is invertible. 
    # No error handling for a non-invertible matrix
#2. get the values in the matrix
#3. set the matrix inverse values
#4. get the matrix inverse values


makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y = matrix()) {
    x <<- y
    invrs <<- NULL
  }  
  get <- function() x  
  setinverse <- function(solve) invrs <<- solve  
  getinverse <- function() invrs
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The below function returns the inverse of the matrix. Cached values are returned if 
# the inverse was previously computed 
# Else the inverse is newly computed and is cached for future retrieval

cacheSolve <- function(x = matrix(), ...) {
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }  
  
  data <- x$get()
  invrs <- solve(data)
  x$setinverse(invrs)
  invrs
}

# Use a command like the below to test 
# cacheSolve(makeCacheMatrix(3*diag(3)))
