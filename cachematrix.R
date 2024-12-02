# makeCacheMatrix is a function that creates an object that 
# consits of a square matrix and list of functions for 
# calculating, storing and retrieving the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL # set the inverse to NULL
  
  # if a new matrix is passed, update the the matrix x in the 
  # environment of makeCacheMatrix and reset the inverse to NULL
  set <- function(mat){ 
    x <<- mat 
    mat_inv <<- NULL
  }
  
  # return x
  get <- function() x
  
  # update the inverse after re-calculating it
  setinverse <- function(inverse) mat_inv <<- inverse
  
  # return the inverse
  getinverse <- function() mat_inv
  
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  # use the getinverse() function to retrieve the inverse
  # and check if it has been already calculated
  mat_inv <- x$getinverse()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  # if the mean is NULL, update the value of 
  # the matrix with the get() function
  data <- x$get() 
  
  # calculate the inverse
  mat_inv <- solve(data, ...)
  
  # update mat_inv for future use
  x$setinverse(mat_inv)
  
  # return the inverse
  mat_inv
  
}
