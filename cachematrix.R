## The first function will cache the inverse of the matrix. The second function computes the inverse of the matrix.
## But if the inverse of the matrix already exists, get it from the cache values.

## function to cache the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y    #assign value of the matrix
    m <<- NULL #clear the previous cache
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse  #function to set inverse
  getInverse <- function() m  #function to set inverse
  
  list(set = set, get = get,  #return list of functions
       setInverse = setInverse,
       getInverse = getInverse)

}


## function to compute inverse of the matrix. If inverse already exists, cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInverse() 
  if(!is.null(m)) { 
    message("getting cached data") #if inverse exists, get it from the cached data
    return(m)
  }.
  data <- x$get()  #if inverse doesnt exist, calculate it
  m <- solve(data) 
  x$setInverse(m)  
  m                
}
