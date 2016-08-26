###This function will initialize the matrix and set the function.
###The list will include all function that we use in the preceeding steps.


makeCacheMatrix <- function(x = matrix()) {

  inversematrix <- NULL
  set <- function(y)
  {
    x<<-y
    inversematrix <<- NULL
  }
  get <- function()
  {
    x
  }
  setinverse <- function(inverse)
  {
    inversematrix <<- inverse
  }
  getinverse <- function()
  {
    inversematrix
  }
  list(set = set, get = get ,setinverse =setinverse ,getinverse =getinverse)
  
}


### This function computes the inverse of the special "matrix", returned by makeCacheMatrix above.
###If the inverse is already calculated, then the cachesolve will retriv the inverse of the matrix.

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()    
        
        if(!is.null(inversematrix))
        { 
          message("Getting cached matrix value")
          return(inversematrix)
        }
        
        data <-x$get()       
        inversematrix <-solve(data)        
        x$setinverse(inversematrix)
        inversematrix
}