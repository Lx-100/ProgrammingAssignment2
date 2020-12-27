## These two functions allow users to cache and retrieve the inverse of 
## matrices. 
## Start by passing matrices to makeCacheMatrix. The function
## allows users to set, get, compute inverse, and get inverse of matrices.
## the cacheMatrix function REQUIRES a makeCacheMatrix argument

## the makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
  
  set <- function (y = matrix()) {
    
    x <<- y
    i <<- NULL
    
  }
  
  get <- function () x
  
  setinverse <- function (solve) i <<- solve 
  
  getinverse <- function () i
  
  list (set = set, get = get, 
        setinverse = setinverse, getinverse= getinverse)
  }


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
  ## cacheSolve requires an input argument of type makeCacheMatrix()
    
  i <- x$getinverse ()
  if (!is.null (i)) {
    message ("getting cached data")
    return(i)
  }

  data <- x$get ()
  i <- solve (data)
  x$setinverse(i)
  i
  
  }
