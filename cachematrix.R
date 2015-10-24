## Put comments here that give an overall description of what your
## functions do
# User should put a matrix into function makeCacheMatrix and store it as
# a new variable. Afterwards, they will be able to calculate inverse of 
# this matrix with function cacheSolve. When cacheSolve is run a second time
# without modifying the matrix supplied in makeCacheMatrix, cacheSolve won't
# calculate the inverse again but receive the result from previous calculation
# from memory.

## Write a short comment describing this function

# Takes a matrix as the argument and creates list of functions to:
# 1. Store (set) the matrix
# 2. Get the matrix
# 3. Store (set) the inverse of the matrix
# 4. Get the inverse

# Variables:
# x: matrix to be inversed
# i: inversed matrix


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL   # set i to NULL in function env (= reset cached inversed matrix)
  
  set <- function (y) {   # create function set to allow setting i without
    x<<-y                 # having to run makeCacheMatrix again. Function
    i<<-NULL              # will reset i to NULL and set x to supplied value 
  }                       # (y) in containing environment (= makeCacheMatrix
                          # function env).
  
  get <- function() x     # return x as specified in makeCacheMatrix
  
  setinverse <- function(inv) i <<- inv
  # sets i to supplied value (inv) in containing env, effectively 
  # caching the inversed matrix.
  # If superassignment of i is missing, results of function will still be
  # correct, but won't be cached.
  
  
  getinverse <- function() i    # return i
  
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  # result of the function is a list containing the four created functions.
  # This way, they can be called easily.
  
}


## Write a short comment describing this function

cacheSolve <- function(o,...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- o$getinverse()
  # sets i to inverse matrix stored through makeCacheMatrix function
  
  if(!is.null(i)) {                       # if stored inverse matrix is found,
    message("getting cached matrix")      # message is returned and i is
    return(i)                             # displayed
  }
  mat <- o$get()                          # else, (non-inversed) matrix stored
  i <- solve(mat,...)                     # through makeCacheMatrix is retreiv
                                          # and its inverse is calculated.
  
  o$setinverse(i)                         # inversed matrix is then stored
  return(i)                               # and displayed.
  
  }