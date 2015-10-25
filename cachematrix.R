## Put comments here that give an overall description of what your
## funct ions do

## Write a short comment describing this function

# This function will create a list of four functions to 
# manage the matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  # when the function is first called, make sure 
  # the function variable inv is null
  inv <- NULL
  
  # the set function takes an input and puts it into
  # the x variable, and sets the function variable inv 
  # to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get returns the value of x, 
  get <- function() x
  
  # setinv puts the value of inverse into the 
  # function variable inverse
  setinv <- function(inverse) inv <<- inverse
  
  #getinv returns the value of inv
  
  getinv <- function() inv
  
  # return the four functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# the following function will look for the inverse of a matrix
# in the caching object returned from the function "makeCacheMatrix"
# if it's found, then it'll return it...if it's not there yet, then
# the solve function is called to invert the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # first use the getinv function to retrieve the inverse of the matrix
  m <- x$getinv()
  
  # check to see if anything was returned
  if(!is.null(m)) {
    #if something is there then return it
    message("getting cached data")
    return(m)
  }
  
  # this code is only run if nothing was returned
  # get the matrix
  data <- x$get()
  # invert the matrix
  m <- solve(data, ...)
  # cache the inverse of the matrix
  x$setinv(m)
  #return the inverse of the matrix
  m
}
