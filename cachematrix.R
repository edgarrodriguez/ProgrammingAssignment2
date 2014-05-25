## Function makeCacheMatrix creates a special Matrix where the 
## inverse of the matrix is calculated the first time and then 
## cached to avoid extra processing time.

## Create special Matrix with cached inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y               #set the value of special matrix that is kept with its inverse
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m  #retrieve the value of special matrix (calculate or retrieve from cache)
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Generate the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv() #get the inverse of the matrix x
  if(!is.null(m)) {
    message("getting cached data")  
    return(m)     #If the variable m is not empty, use cached inverse
  }
  data <- x$get() #If the variable m is empty, calculate the inverse
  m <- solve(data, ...)
  x$setinv(m)
  m
}


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}



