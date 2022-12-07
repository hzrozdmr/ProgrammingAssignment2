

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
  j <- NULL  ##initializing inverse as NULL
  set <- function(y){
    x<<-y
    j<<-NULL
  }
get <- function()x ##function to get matrix x
  setInverse <- function(inverse) j<<- inverse
  getInverse<-function()j ##function to obtain inverse of the matrix
  list(set=set, get=get, 
       setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## gets cache data
  j <- x$getInverse()
  if(!is.null(j)){ ##checking if inverse is NULL
    message("getting cached data")
    return(j)  ## returns inverse value
  }
  mat<- x$get()
  j <- solve(mat,...) ## calculates inverse value
  x$setInverse(j)  
  j
  }
