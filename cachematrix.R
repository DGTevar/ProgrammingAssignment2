## This function creates a special "matrix" object that can cache its inverse.
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse
## 4- get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  iMatx <- NULL
  #operate with the "matrix" (creates and get)
  set <- function(y) {
    x <<- y
    iMatx <<- NULL
  }
  get <- function() x
  
  #operate with inverse of the "matrix" (set and get inverse)
  setInverse <- function(inv) iMatx <<- inv
  getInverse <- function() iMatx
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

## Assume that the matrix supplied is always invertible.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x) {
  #take the inverse of the matrix
  inverseX <- x$getInverse()
  
  #if the matrix already has inverse -> return the stored inverse
  if(!is.null(inverseX)){
    print("No calculation:")
    return (inverseX)
  }
  
  #if the matrix doesn't have inverse -> calculated the inverse, is stored and then we return the value
  else{
    print("Calculation...")
    
    #take the matrix
    mat <- x$get()
    #Inverse of x where x is a square matrix.
    inv <- solve(mat)
    #set the inverse value
    x$setInverse(inv)
    #return the inverse value
    return(inv)
  }
}
