## These two functions calculates the inverse of a matrix. To conserve resources, 
## One function will first stored the matrix and cache the inverse of the matrix,
## while the 2nd function evaluates whether to output the cached inverse or to perform 
## a new inverse computation and stores it on the first function.

## makeCacheMatrix() is a function that stores the value of the matrix inverse
## calculated by the cacheSolve() function. This function is just a storage and it
## doesn't perform any calculation.

makeCacheMatrix <- function(x = matrix()){
  s <- NULL
  set <- function(y){
    x<<-y
    s<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function is to calcuate the inverse of a newly supply matrix or
## return the previously calculated inverse stored in the makeCacheMatrix if no new
## matrix was supplied.

cacheSolve <- function(x,...){
  s<- x$getinverse()
  if(!is.null(s)) {  
    message("getting cached data")
    return(s)  ## if there is already an inverse calcuated, just output the previously
               ## calculation.
  }
  data <- x$get()                        
  s <- solve(data, ...)
  x$setinverse(s)
  s ## else:  calculates the inverse, stores it in the makeCacheMatrix, and output the
    ## new inverse
}