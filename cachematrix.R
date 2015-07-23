## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ##set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## Get the inverse of the matrix 
  getinverse <- function() inv
  
  ## Create a list of set,get,setinverse and getinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Call the getinverse function from the environment in which x was set and store it in object inv
  inv <- x$getinverse()
  
  ##if inv is not empty i.e contains the inverse,then get it from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix in data using the get() function in the environment where x was set to NULL
  data <- x$get()
  
  ## Calculate the inverse using solve function
  inv <- solve(data,diag(1,nrow=dim(data)[1]),ncol=dim(data)[1])
  
  ## Set inv variable 
  x$setinverse(inv)
  
  ## Return the inverse matrix
  inv
}
