## To avoid calculating the inverse of a matrix again and agian you can store it in a 
## retrievable cache which changes only if the matrix called changes.
## The makeCacheMatrix function is a constructor function that creates functions to set and get the matrix and
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setInverse = setinverse,
       getInverse = getinverse)
}

## The cacheSolve function takes a cacheable matrix as an input to retrieve the inverse of a matrix. 
## If the matrix is unchanged and the inverse has already been calculated, it retrieves the matrix inverse from
## the cache. If not, it calculates the matrix inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
## Returns the matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
