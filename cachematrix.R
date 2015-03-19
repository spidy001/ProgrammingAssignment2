##	This file cachematrix.R contains two functions 
##	1. makeCacheMatrix 
##      Returns a special matrix object that can set & get matrix data
##      and set matrix inverse in cache & get matrix inverse from cache.
##	2. cacheSolve
##       Returns the inverse of the matrix.


## Returns list of functions set,get,setinverse,getinverse to
## set the value of matrix,get the value of the matrix ,
## set the inverse of matrix in cache and 
## get the inverse of matrix from the cache
makeCacheMatrix <- function(x = matrix()) {
  #cache variable to store inverse.initializing the inverse cache variable.
  inverseCache <- NULL
  #set function - sets the matrix x with the passed value y.
  #if x was never set,function returns an 1*1 matrix with value NA 
  set <- function(y) {
    x <<- y
    #resets the inverse cache 
    inverseCache <<- NULL
  }
  #get - returns previously set value for matrix x.
  get <- function() x
  #setinverse - sets the inverse cache with the passed inverse value.
  setInverse <- function(inv) inverseCache <<- inv
  #getinverse - returns the inverse from inverse cache.
  getInverse <- function() inverseCache
  #returns list of functions set,get,setinverse,getinverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Calls makeCacheMatrix function's functions to get the inverse from cache,
## get the matrix data and set the cache with inverse.
## if data exists in the cache, this function reads the inverse from the cache
## else calculates the inverse of a matrix and update the cache with inverse.
cacheSolve <- function(x, ...) {
  #gets the inverse from cache. i.e from inverse cache
  cachedInverse <- x$getInverse() 
  #if cache data is not null then  data is retrieved and returned.
  if (!is.null(cachedInverse)) {
    message("getting cached data")
    #returns cached inverse
    return(cachedInverse)
  }
  #if cache is null then inverse was not calculated before
  #get the matrix data to calculate inverse
  matrix <- x$get()
  #calculates the matrix inverse using R's solve function
  matrixInverse <- solve(matrix)
  #sets the inverse cache  with newly cacluated inverse value.
  x$setInverse(matrixInverse)
  #returns the inverse value of the matrix
  matrixInverse
}
