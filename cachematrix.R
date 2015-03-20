# Function makeCacheMatrix initializes cache and defines functions to read and write inverse values
makeCacheMatrix <- function(x = matrix())
{
  # initialize cache
  i <- NULL
  
  set <- function(m)
  {
    x <<- m
    i <<- NULL
  }
  
  get <- function()
  {
    x
  }
  
  setInverse <- function(inverse)
  {
    i <<- inverse
  }
  
  getInverse <- function()
  {
    i
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
# Function cacheSolve returns inverse of matrix x: 1) fetches inverse from cache or 2) calculates inverse if not in cache
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  # Determine if inverse has been saved in cache
  if(!is.null(i))
  {
     # inverse is in cache so return with value
     message("getting cached inverse matrix")
     #print(i)
     return(i)
  }
  else {
     # inverse is calculated using solve function and written into cache
     print("inverse calculated")
     m <- x$get()
     i <- solve(m, ...)
     x$setInverse(i)
     #print(i)
     i
  }
}