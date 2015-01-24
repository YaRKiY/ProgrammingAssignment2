## This function takes any matrix that we asked and
##pushes it into another environment

makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## First, we give the function object
##which we did in the previous function. 
##Then, this function checks whether the inverse matrix 
##in the cache. If it finds one, it returns stored data 
## If not, invert the matrix, and stores it 
##in the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}     
