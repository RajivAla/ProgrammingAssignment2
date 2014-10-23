## The makeCacheMatrix creates a special matrix that can cache its inverse. The Function makes use of Global Vs Local variable assignments (lexical scoping rules). 

## This function can perform the following tasks..

##  set the value of the matrix 
##  get the value of the matrix
##  set the value of the inverse
##  get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {  
  
  m <- NULL
  
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


##  The "cacheSolve" function calculates the inverse of the matrix created by the "makeCacheMatrix" function. 
##  If the inverse is already calculaed, it skips the computation. 
##  If not, it calculates the inverse and passes the inverse value to "makeCacheMatrix"  using the setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
