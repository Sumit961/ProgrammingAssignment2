## This function calculates the inverse of a matrix and if the inverse had been
## calculated already, it retrieves from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialise m
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse(solve)
  setsolve <- function(solve) m <<- solve
  
  ## get the value  of the inverse(solve)
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), the cachesolve retrieves the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse from the makeCacheMatrix
  m <- x$getsolve()
  
  ## checks if data is cached or not
  if(!is.null(m)) {
    
    ## data is already present(cached)
    
    message("getting cached data")
    return(m)
  }
  
  ## get the data into the variable data(if not cached)
  data <- x$get()
  
  ## solve the data(solve inverse)
  m <- solve(data, ...)
  
  ## set the solve value into m
  x$setsolve(m)
  m
}
