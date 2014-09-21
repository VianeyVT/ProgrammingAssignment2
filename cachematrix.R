#MakeCacheMatrix creates a special matrix containing a function to: 
#set the value of the matrix
#get the value of the matrix
#set the inverse of the matrix
#get the inverse of the marix

#cacheSolve computes the inverse of the MakeCacheMatrix's result 
#in case the result has already been calculated or the matrix isn't different
#the function retrieve the inverse from the cache


#makeCacheMatrix has a matrix as argument,sets and gets the values of the matrix
#and set and get the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#cacheSolve takes the result of makeCacheMatrix and retrieve the inverse from
#the cache or calculates it.
cacheSolve <- function(x) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}