## Programming Assignment 2 - ADilick
## Store the store and return the inverse of a matrix

## makeCacheMatrix checks to see if a value has been calculated for the matrix inverse, 
## otherwise calculates, stores and returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<-y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s<<- solve
  getInverse <- function() s
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
   s <- x$getInverse()
   if(!is.null(s)){
     message("Getting cached data")
     return(s)
   }
   data <- x$get()
   s <- solve(data, ...)
   x$setInverse(s)
}
