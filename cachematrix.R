## a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     #create a local variable called 'm' and set it's value to NULL
  # create a function called 'set' that will save the value of the matrix specified as an input argument to memory
  # (eg: to cache it)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # assign the value of the matrix specified as an input argument to a local variable named 'get'
  get <- function() x
  # create a function called 'setInverse' which calculates the inverse of the matrix specified in
  # the input argument and save the result to memory (eg: cache it)
  setInverse <- function(solve) m <<- solve
  # crete a function called 'getInverse' which returns the value of of the free variable 'm'. If
  # 'setInverse' has been run previously and the result stored to memory/cache, then m will have the
  # value of the inverted matrix. If not, m will have a NULL value
  getInverse <- function() m
  # return a list of the 4 functions created within makeCacheMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the return value of makeCacheMatrix (a matrix) and computes its inverse. If the
## inverse has already been calculated (and the matrix has not changed), then cachesolve retrieves the
## inverse from the cache instead of re-computing it

cacheSolve <- function(x, ...) {
  # create a local variable, 'm' and assign the return value of makeCacheMatrix$getinverse to it.
  m <- x$getInverse()
  # check to see if m has a value or is NULL
  if(!is.null(m)) {
    # if m has a value, then its inverse has already been calculated and cached. no need to re-calculate
    message("getting cached data")
    # inform the user that the previously cached value is being returned and return m (the previously cached
    # value of the inverted matrix) and exit the function
    return(m)
  }
  # if the value of m is null, create a local variable called 'data' and assign the value of makeCacheMatrix$get
  # (a matrix) to it
  data <- x$get()
  # calculate the inverse of the matrix and assign the value (another matrix) to the variablel 'm'
  m <- solve(data)
  # call makeCacheMatrix$setinverse, to save the value of the inverted matrix to memory (eg: to cache it)
  x$setInverse(m)
  # return the value of m (the inverted matrix) and exit the function
  m
}
