## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {           ## creates a function named makeCacheMatrix which accepts a matrix as its variable
      m <- NULL      				            ## creates an varible named "m" which hold a value of NULL
      set <- function(y) {				      ## creates a function named set which accepts the value put into the makeCacheMatix function
            x <<- y					      ## sets the value accepted to a variable "x" in the parent environment
            m <<- NULL				            ## creates a variable "m" in the parent environment with a NULL value
      }
      get <- function() x				      ## creates a function named get which looks for the variable "x"
      setinverse <- function(inverse) m <<- inverse	## creates a function named setinverse takes the calculated value and puts it into a variable "m" which lives in the parent environment 
      getinverse <- function() m			      ## creates a function which retrieves the value residing in the variable "m" in the parent environment
      list(set = set, get =inverse,			      ## creates a list of the four functions created within this function giving each of them the
           getinverse = getinverse)			      ## same name within the list as the name of the function
}


## Write a short comment describing this function


cacheSolve <- function(x, ...) {      		      ## creates a function which will either retrieve the solved value or calculate and return the value
      m <- x$getinverse()				      ## retrieves the value of variable "m" which was created in the makeCacheMatrix function
      if(!is.null(m)) {				            ## tests to see if the value of "m" is not "NULL".  If not "NULL" stays in the if statment code
            message("getting cached data")		## Since a value exists a statement is printed "getting cached data"
            return(m)				            ## The value found in "m" of the parent environment is printed to the screen and the code stops
      }
      data <- x$get()					      ## There is no value for "m" and it must be calculated.  Data is retrieved from the "x" variable								## residing in the parent environment and placed into the variable "data"
      m <- solve(data, ...)				      ## the value of the inverse of the square matrix is determined and placed in the variable "m"
      x$setinverse(m)					      ## the value of "m" is placed in the variable "m" residing in the parent environment
      m						            ## the value of "m" is printed to the screen
}