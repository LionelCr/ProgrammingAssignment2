makeCacheMatrix <- function(x = matrix()) {
  ## @x: is a dim(m,m) invertible matrix
  ## provides a list of functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         these are used in cacheSolve() as inputs.
  
  inv = NULL
  set = function(y) {
    # `<<-` assign a value to something in an environment
    #different from the current environment, as I understand that :),  not 100% sure, but it works, is'n it ? 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse # here I define the setinv function, will be used in the cacheSolve function
  getinv = function() inv                    # Define the getinv function to get the inverse matrix from the cache
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv) # not sure 100% why it's there, could the reviewer explain :), ? 
}

cacheSolve <- function(x, ...) {
  ## inverse the matrix x 
  
  inv = x$getinv()
  
  # if the inverse matrix exist:
  if (!is.null(inv)){
    #just return the matrix with a nice message 
    message("here's your matrix from the cache")
    return(inv)
  }
  
  # else, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse matrix via the setinv function defined above.
  x$setinv(inv)
  
  return(inv)
}