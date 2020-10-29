## The first function, makeCacheMatrix, creates a special 'matrix' object that can cache its inverse.
##Here I use the the <<- operator to assign a value to an object in an environment that is different from the current environment.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The second function, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve will retrieve the inverse from the cache. 
## Computing the inverse of a square matrix can be done with the solve function in R.

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

testing <- matrix(c(1:10),2,2)
testing

testing2 <- makeCacheMatrix(testing)
cacheSolve(testing2)