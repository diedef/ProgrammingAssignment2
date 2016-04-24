## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatric() creates a specal matrix object that can cache its inverse
## It involves four steps: setting the matrix, getting the matrix,
## setting the inverse, and getting the inverse. This output is used for cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  list(set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## Write a short comment describing this function
## cacheSolve() computes the inverse of the matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and the matrix hasn't changed, 
## it will retrieve the inverse from the cache directly.

cacheSolve <- function(x=matrix(), ...){
  
  m <- x$getMatrix()
  
  if (!is.null(m)){
    
    message("getting cached data")
    
    return(m)
  }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  
  x$setMatrix(m)
  
  return(m)
}