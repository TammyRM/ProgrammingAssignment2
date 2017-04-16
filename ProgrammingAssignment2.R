
## Creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
## let's set up and initialize
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
  
   get <- function() x
   setinv <- function(inverse) m <<- inverse
   getinv <- function() m
   list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
  }

## Compute the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the matrix has already been calculated and the matrix has not changed
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
  
## has the inverse already been calculated?
    if(!is.null(m)) {
       message("getting cached data")
       return(m)
  }
## No.  Let's calculated the inverse.
    mat.data <- x$get()
    m <- solve(mat.data, ...)
  
    x$setinv(m)
 
## Return the matrix.
    
    return(m)
  }
