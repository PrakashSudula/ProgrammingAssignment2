## Put comments here that give an overall description of what your
## functions do

## This is to store the computed inverse matric, means caching the value.
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
        ## Return a matrix that is the inverse of 'x'
  
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



