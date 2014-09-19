############################################################
# Name: makeMatrix.R                                       #
# Function: makeCacheMatrix                                #
# Parm: Matrix                                             #
# Date: Sept 19,2014                                       #
# Comment: Cloned from makeCacheMean Sample                #
############################################################

makeCacheMatrix <- function(x = matrix()) {
  # Initialize local variable
  m <- NULL
  
  # set function used to replace cached maatrix 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get function returns matrix
  get <- function() x
  
  # setSolve fun ction applies inverse (solve)
  setSolve <- function(solve) m <<- solve
  
  # getSolve function that returns local variable
  getSolve <- function() m
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


############################################################
# Name: cachesolve.R                                       #
# Function: cachesolve                                     #
# Parm: Matrix from makeCacheMatrix Function               #
# Date: Sept 19,2014                                       #
# Comment: Cloned from cacheMean Sample                    #
############################################################

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  # If cached matrix exist return the cached matrix
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Else get the matrix and apply inverse function (solve)
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  # Return inverse matrix
  m
}