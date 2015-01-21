## The functions are used to create an object that stores 
## a symmetric matrix and caches the inverse of the matrix.



## makeCacheMatrix creates a matrix object of 'x'. It pre-allocates space 
## for the variable 'm' (inverse matrix) in the cache and contains a 
## list of functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## cacheSolve returns a matrix 'm' that is the inverse 
## of 'x'. Via setSolve() and the deep assignment arrow, 'm' is not stored in 
## the current environment, but in the parent environment, 
## which serves as cache. getsolve() controls if m is not 'null'(stored in 
## the cache / parent environment) If 'm' is found, 
## the result 'm' plus a message is returned. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


## http://www.r-bloggers.com/closures-in-r-a-useful-abstraction/
## http://adv-r.had.co.nz/Environments.html
## discussion forum

## use:
b<- makeCacheMatrix()
> b$set(matrix(1:4, nrow = 2, ncol = 2))
> cacheSolve(b)
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(b)
getting cached data
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> b$getsolve()
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
##