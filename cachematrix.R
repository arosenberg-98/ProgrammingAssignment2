# The following functions stores a matrix and cache's its inverse.

# makeCacheMatrix creates a list of functions that stores a matrix that can be cached. 
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y           
    m <<- NULL        
  }

  get <- function() x                       
  setInv <- function(solve) m <<- solve     
  getInv <- function() m                    
  
  list(set = set, get = get,
            setInv = setInv, 
            getInv = getInv)
  
}

# cacheSolve calculates the inverse of a matrix, if it hasn't already been calculated.
# If the inverse already exists, it gets the inverse from cache.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInv(m)
  m

}
