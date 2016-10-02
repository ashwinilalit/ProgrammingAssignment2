## Overall Description of Assignment: Caching the Inverse of a Matrix
## This assignment is to write a pair of functions that cache the inverse of a matrix.
## Computing the inverse of a square matrix can be done with the solve function in R
## This assumes square invertible matrix will be passed as argument for making special cached Object
## and calculating its inverse.

##
## The first function, makeCacheMatrix creates a special "matrix".
## This function creates a special "matrix" object that can cache its inverse.
## This function is really a list containing a function to -
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
##

makeCacheMatrix <- function(x = matrix()) {
m <- NULL   ## sets the default value of m to NULL when cacheSolve not called
  y <- NULL ## sets the default value of y to NULL when cacheSolve not called
  
  set <- function(y) { 
    x <<- y    ## caches the inputted matrix 
    m <<- NULL ## sets the value of m to NULL
  }
  
  get <- function() x ## returns the matrix
  setinverse <- function(inverse) m <<- inverse ## sets the inverse of matrix
  getinverse <- function() m ## retruns the inverse
  
  list(set = set, get = get, # creates a list to house the four functions
       setinverse = setinverse,
       getinverse = getinverse)

}

##
## The second function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Else it calculates inverse of the special object using SOLVE function. 
##  if X is a square invertible matrix, then solve(X) returns its inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## Compare the last value of matrix
  m <- x$getinverse() ## get already calculated value of inverse
  
  if(!is.null(m)){ ## if already called
    message("getting cached data")
    return(m)
  }
  y <- x$get() 	##  get the value of the input matrix
  x$set(y) 		## set function on the input matrix to cache it
  m <- solve(y,...) ## compute the value of the inverse of the input matrix
  x$setinverse(m) ## cache the inverse
  m # return the inverse

}
