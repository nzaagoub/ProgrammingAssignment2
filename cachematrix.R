## Computing the inverse of a matric is a time-consuming computations.
## The code below implements the <<- operator with two functions used to create 
## a sepcial object that stores a matrix and cashes its inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) n <<- inverse
  getinverse <- function() n
  list( set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  if(!is.null(n)) {
          message("getting cached data")
          return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}
