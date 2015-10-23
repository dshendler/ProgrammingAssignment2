# the f(x makeCacheMatrix creates the inverse of a matrix using the solve f(x); it sets it to the 'm', which we use to check in the secondary f(x), cacheSolve
#Note: I use the shorthand 'f(x)' to mean 'function' in the notes

makeCacheMatrix <- function(x = matrix()) {
  # m is the inverse of the matrix x - it's calcuated here in makeCacheMatrix and retrieved in cacheSolve
  # first zero-out (make NULL) the inverse
  m = NULL
  set = function(y) {
    # using '<<-' to assign value to an object in an environment that is different from the current environment
    x <<- y
    m <<- NULL
  }
  get = function() x
  #use the solve f(x) to get the inverse of the matrix x
  
  set_inv = function(solve) m <<- solve 
  get_inv = function() m
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}

cacheSolve <- function(x, ...) {
  # this f(x) checks if inverse (m) exists; if not, create it and cache it
  
  m = x$get_inv()
  
  # check if the inverse has already been calculated
  if (!is.null(m)){
    # if it's there, return it
    message("Now getting cached data.")
    return(m)
  }
  
  #if inverse 'm' not present, calculate it
  matrix = x$get()
  m = solve(matrix, ...)
  
  # store m in cache
  x$set_inv(m)
  
  return(m)
}
