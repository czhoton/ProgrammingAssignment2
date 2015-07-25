## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  ## setter the value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## getter the value of matrix
  get <- function() x
  
  ##setter the inverse of matrix
  setInv <- function(solve) inv <<- solve
  
  ##getter the inverse of matrix
  getInv <- function() inv
  
  ## function to store above 4 functions in the makeCacheMatrix
  list(set = set, 
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## getter inverse of given matrix
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## if not present, getter inverse of given matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

