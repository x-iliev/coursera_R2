# makeCacheMatrix is a function that creates an object that 
# consits of a matrix and a list of functions for calculating, 
# storing and retrieving it`s inverse 

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



# cacheSolve is a function that takes the matrix and the functions 
# to work with it. It then calculates the inverse and stores it in 
# a variable that can be accessed using methods defined in 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  # use the getinverse() function to retrieve the inverse
  # and check if it has been already calculated
  mat_inv <- x$getinverse()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  # if the inverse is NULL (not yet calculated = new matrix), 
  # update the value of the matrix with the get() function
  data <- x$get() 
  
  # calculate the inverse
  mat_inv <- solve(data, ...)
  
  # update mat_inv for future use
  x$setinverse(mat_inv)
  
  # return the inverse
  mat_inv
  
}
