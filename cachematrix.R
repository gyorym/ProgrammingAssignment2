## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#create the makeCacheMatrix function and pass a matrix as the object 'x'
makeCacheMatrix <- function(x = matrix()) {
  #create a null variable to be used in cacheMatrix
  m <- NULL
  set <- function(y) {
    #assigns y to x so it can be used in difference environment, i.e. cacheMatrix
    x <<- y
    #ensures m is NULL when used in cacheMatrix
    m <<- NULL
  }
  
  #sets the value of get to the function of x, returning x
  get <- function() x
  #sets the value of setinv to call the solve function using m and allowing it to be referenced
  #in other environments
  setinv <- function(solve) m <<- solve
  #sets the value of getinv to m 
  getinv <- function() m
  
  #output a list of saved Inverse values
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}

## Write a short comment describing this function
#cacheSolve takes a passed list x and either uses the stored inverse value or calculates one
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #from a list, x, that may or may not be the result of makeCacheMatrix, set m to its getinv value
  m <- x$getinv()
  #test to see if m is null, if m is not null, return the stored inverse value and exit the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if 
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
