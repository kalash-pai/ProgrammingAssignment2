## Put comments here that give an overall description of what your
## functions do
# This R code caches computations and this helps save time for matrix inverse.
# This is especially useful if the contents of the matrix do not change.
# The inverse is cached so that when needed again, it can be looked up here
# rather than re-compute it.
# We assign a value to object in environment that is different from
# current environment using <<-

## Write a short comment describing this function
# Make a "special" matrix to set value of matrix, get its value,
# set its inverse and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv_x <- NULL
      set <- function(y){
            x <<- y
            inv_x <<- NULL
      }
      get <- function() {x}
      set_inverse <- function(inverse) {inv_x <<- inverse}
      get_inverse <- function() {inv_x}
      list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
      
}


## Write a short comment describing this function
# First check if inverse has been computed before.
# If so, get it from the cache.
# If no, compute it and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv_x <- x$get_inverse()
      if(!is.null(inv_x)){
            message("Getting Cached Data")
            return(inv_x)
      }
      mat <- x$get()
      inv_x <- solve(mat, ...)
      x$set_inverse(inv_x)
      inv_x
}
