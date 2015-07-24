

## This function is to store the computed inverse matrix.
##i.e. through this function values are available in cache if it was already executed at least once.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y){
    
    x <<- y
    
    i <<- NULL
    
  }
  
  get <- function()x
  
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  
  list(set = set, get = get, setminv =setinv , getminv = getinv )

}


## This function will check for alreday cached inverse matrix,if available then return that value
## if not available then compute the inverse of given matrix, and then return it.
cacheSolve <- function(x, ...) {
  
  i <- x$getminv()
  
  if(!is.null(i)){
    
    message("Get inverse matrix from cache")
    return(i)
  }
  
  inv <- x$get()
  
  i <- solve(inv)
  
  x$setminv(i)
  i
  
}

##below comments will tell us about how to get the inverse of given matrix.

## > x <- matrix(1:4,2,2)
## > x
## if we execuets the x then below matrix will be output
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

##pass the x to function and then store the result in m
## > m <- makeCacheMatrix(x)
## get the values
## > m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## execute the cacheSolve function, it return the inverse matrix 
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## execute again, this time values are returned from cache.
## > cacheSolve(m)

## Get inverse matrix from cache
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

