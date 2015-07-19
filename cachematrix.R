## This file has two functions that create a special matrix and compute the inverse of a matrix.
## The computed inverse matrix is cached to save computation time for future inversions of the same matrix. 

## Function: makeCacheMatrix()
## This function creates a special matrix that is cached for reduced processing time. 
## It is useful if the same operation is repeated multiple times in a program.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL   # create a variable
                                          
  set <- function(y) {                     # set function to create cached variable
    x <<- y
    inv <<- NULL
  }
  get <- function() x                      # get the cached variable
  setinv <- function(solve) inv <<- solve  # assign the solved inverse matrix to cached variable
  getinv <- function() inv                 # get cached inverse matrix
  list(set = set, get = get,               # return special matrix 
       setinv = setinv,
       getinv = getinv)
}


## Function: cacheSolve
## This function solves for the inverse of the special matrix created using makeCacheMatrix() and stores
## the computed value.
## If the inverse has been previously computed then the function simply returns the stored value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                 # check if the inverse matrix has been cached
  if(!is.null(inv)) {               # if getting the stored matrix
    message("getting cached data")  # print info message for user confirming cached matrix was used
    return(inv)                     # return inverse matrix
  }
  data <- x$get()                   # otherwise, get cached variable
  inv <- solve(data, ...)           # compute the inverse of the matrix
  x$setinv(inv)                     # set the result in the cached variable
  inv                               # return result
}
