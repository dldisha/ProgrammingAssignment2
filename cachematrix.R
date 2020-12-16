makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse property
  inv <- NULL
  
  ## set the matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ##get the matrix
  get <- function() {x}
  ##set the inverse of the matrix
  setInv <- function(inverse) {inv <<- inverse}
  ##get the inverse of the matrix
  getInv <- function() {inv}
  
  ## Return a list of the processes facts
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}

## Compute the inverse of the matrix above 
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  
  ## Return the inverse if its already set
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix from the data 
  data <- x$get()
  ## Use matrix multiplication to get the inverse
  inv <- solve(data, ...)
  ## Set the inverse
  x$setInv(inv)
  
  ##print inverse matrix
  inv
}