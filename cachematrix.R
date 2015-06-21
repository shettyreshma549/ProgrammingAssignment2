
## The following functions cache the inverse of a matrix so that it does not have to be calculated each time we need it.

## This function creates a special "matrix" object that can cache its inverse
## It contains a list of functions to do this

makeCacheMatrix <- function(x = numeric(matrix())) {
  
  inv <- NULL
  
# set is a function that changes the vector stored in the main function  
  set <- function(y) {
      x <<- y
      inv <<- NULL
    }

# get is a function that returns the vector x stored in the main function
  get <- function() x
  
# setinverse stores the value of the input in a variable inv into the main function 
  setinverse <- function(inverse) inv <<- inverse

# get inverse returns the value of inv
  getinverse <- function() inv

# storing the 4 functions in the function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x) {

# verify if the inverse of matrix exists already in the cache
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }

# Else calculate the value and store its value in the object of makeCacheMatrix
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}