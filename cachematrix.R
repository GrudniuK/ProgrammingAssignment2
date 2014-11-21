#Functions that are used to create a special object that stores a numeric matrix and cache's its inverse.

#makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverted matrix
#get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

#cacheSolve calculates the inverse of the special "matrix" created with the above function. 
#However, it first checks to see if the inverted matrix has already been calculated. 
#If so, it gets the inverted matrix from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and sets the inverted matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}