##Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
  m<-NULL   ## initialize the inverse property
  
  ## set the matrix
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  
  
  ## get the matrix
  get<-function() x
  
  
  ## set the inverse of the matrix
  setInverse<-function(inverse)
    m<<-inverse
  
  
  ## get the inverse of the matrix
  getInverse<- function() m
  
  
  ## return a list of the methods. 
  list(set=set, get=get,setInverse=setInverse, getInverse=getInverse)
  
  
  
  
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  
  
  m<-x$getInverse()
  
  ## check matrix if its already set 
  if(!is.null(m))
  {
    message ("getting cached data ")
    return(m)
  }
  
  ## if not: get the inverse of matrix 
  data<-x$get()
  
  ## calculate the inverse 
  m<-solve(data,...)
  
  ## set the inverse to the matrix
  x$setInverse(m)
  
  ## return the matrix
  m
  
}


