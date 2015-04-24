## This file contains two functions which calculate the inverse of a matrix quickly by 
# returning a previously calculated value if it is available. 
#
# The two functions it contains are:
# makeCacheMatrix - called with an invertable matrix as an arguement, 
#     returns a list of functions which allow access to the matrix and its inverse
# cacheSolve - called with a list created by makecacheSolve, 
#     returns the inverse of the matrix


## The makeCacheMatrix function takes a matrix and returns a list containing the matrix and
# other required functions for use in the cacheSolve function. The list contains
# set - function which assigns the input argument as the matrix to calculate the inverse of
# get - function which returns the stored matrix (specified in the input argument)
# setinverse - function which calculates the inverse and stores it in i
# getinverse - function which returns the value of i

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # initiate the stored value to NULL (no inverse calcuated)
  set <- function(y) { # store a matrix and reset the stored inverse to NULL (no inverse calculated)
    x <<- y
    i <<- NULL
  }
  get <- function() x # return the value of the matrix
  setinverse <- function(inverse) i <<- inverse # set the value of the inverse
  getinverse <- function() i # return the value of the inverse
  list(set = set, get = get, # return the list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## The CacheSolve function operates on objects created by the makeCacheMatrix function.
#
# It returns the inverse of the matrix (on the assumption it exists). IT does this by
# returning the stored value if available, or by calculation otherwise. If by calcuation 
# then it sets this as the stored value for future use.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  
  # get any stored value (might be NULL)
  i<- x$getinverse()
  
  if(!is.null(i)) {
    # the inverse has been previously calculated, so return this with a message
    message("Inverse previously calculated - returning this value")
    return(i)
  }
  
  # the inverse has not been previously calculated, so calculate it and
  # store it for potential future use, print a message and return it
  data <- x$get() # get the matrix
  i <- solve(data, ...) # calcuate the inverse
  x$setinverse(i) # store the value
  message("Inverse was stored") # return an message
  i 
}
