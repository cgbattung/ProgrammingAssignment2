## R Programming Week 3 Assignment
## Author: Christian Battung

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## 'makeCacheMatrix' creates a matrix that is to be cached. The function accepts
## the matrix object `x` as it's parameter. An additional object `inv` is created.
## Inside `makeCacheMatrix` are four additional functions:
      #1) `set`: accepts a variable `y`, uses the <<- operand to set `x` = `y` and
               # `inv` = `NULL`
      #2) `get`: returns the matrix variable `x`
      #3) `setinverse`: accepts the variable `inverse`, uses the <<- operand 
                      # to set `inv` = `inverse`
      #4) `getinverse`: returns the value within the `inv` object
## Last, a list is created containing each of the functions for access.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) {
            inv <<- inverse
      }
      getinverse<- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## Write a short comment describing this function

## `cacheSolve` is a function that calculates the inverse of the matrix passed into
## `makeCacheMatrix`. The function accepts the variable `x` as well as an ellipses
## to allow for additional arguments. The object `inv` is set to the value returned
## by`$getinverse()`. A logical test is executed on the `inv` variable. Such
## that if `inv` is not null, it's contents are returned to the user. Otherwise 
## a new object `data` is created and set to the matrix returned by `$get()`. The
## `data` object is entered into the `solve()` function, and it's contents are 
## assigned to `inv`. The inverse of the `x` matrices is set using `inv` and 
## `$setinverse()` to be cached for future access. Lastly the contents of the `inv`
## are printed to the screen.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
