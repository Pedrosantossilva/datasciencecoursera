
#The first function, "makeCacheMatrix" creates a special "matrix", which is
#really a list containing a function to get and set the "matrix", 
#and get and set the inverse of the "matrix".

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinv <- function(inv) z <<- inv
  getinv <- function() z
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## The second function calculates the inversion of the "matrix"
#created above. First, it checks  if the inversion has already been calculated. 
# If so, it `get`s it from the cache and skips the computation. 
#Otherwise, it calculates the inversion of the "matrix" and sets the value 
#with "solve" function

cacheSolve <- function(x, ...) {
  z <- x$getinv()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinv(z)
  z
}
