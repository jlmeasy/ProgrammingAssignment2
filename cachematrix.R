## makeCacheMatrix function will cache the matrix object
## this function has an argument of type matrix
makeCacheMatrix <- function(x = matrix()) {
  ## this will initialize the variable m to NULL
  m <- NULL
  ## this setter function has an argument Y which is expected to be a type of matrix
  set <- function (y) {
    ##this will assign the value of y to the parent variable named x
    x <<- y
    ##this will also initialize the value of m into NULL so that it will be clear
    ##prior to the execution of cachesolve function
    m <<- NULL
  }
  ##the value of x from the parent environment
  get <- function() x
  ##assign the input argument to the value of m in the parent environment
  setsolve <- function(solve) m <<- solve(x)
  ##retrieve the value of m from parent environment
  getsolve <- function() m
  ##assign this functions as an element within a list and returns it to the parent
  ##environment
  list(set = set, get = get,
       setsolve = setsolve, 
       getsolve = getsolve)
}


##this function will retrieve the cache matrix object that is the inverse of x
cacheSolve <- function(x, ...) {
  ##retrieve a matrix
  m <- x$getsolve()
  ##check whether the result is null
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##if the result is null, cacheSolve gets the matrix from the input object 
  ##perform the solve function, uses the setsolve function and returns the
  ##value of the solve to the parent environment by printing the solve object
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
