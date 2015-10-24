## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix and cacheSolve will allow to store a matrix, calculate
# its inverse and cache the result so it can be recalled quickly.

# Variables:
# x: matrix to be inversed
# i: inversed matrix
# o: result of the makeCacheMatrix function, saved as a list


## Write a short comment describing this function

# Takes a matrix as the argument and creates list of functions to:
# 1. Store the matrix
# 2. Get the matrix
# 3. Store the inverse of the matrix
# 4. Get the inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   
  
  set <- function (y) {   
    x<<-y                 
    i<<-NULL               
  }                       
                          
  
  get <- function() x     
  
  setinverse <- function(inv) i <<- inv

  getinverse <- function() i    
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function

# Calculates the inverse of the matrix supplied in makeCacheMatrix.
# Afterwards, When cacheSolve is run again and no new value has been
# set in makeCacheMatrix, the function won't do the calculation again but
# will recall the result of the previous calculation from memory.

cacheSolve <- function(o,...) {
  
  i <- o$getinverse()
  
  if(!is.null(i)) {                       
    message("getting cached matrix")      
    return(i)                             
  }
  mat <- o$get()                          
  i <- solve(mat,...)                     
                                          
  
  o$setinverse(i)                        
  return(i)                               
  
  }