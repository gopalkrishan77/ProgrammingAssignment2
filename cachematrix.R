## this code follows the coding as provided in the example for assignment
## makeCacheMatrix creates a special matrix object

## As mentioned this functions follows the example
## The get and set are for matrix and
## getinverse and setinverse get and set the inverse of matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_x <- NULL  ## This is variable to hold the inverse of the matrix

## set funtion will set the special matrix to one passed in variable to MakeCacheMatrix
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }

## get function returns the Matrix (not inverse) from the special matrix. 
  get <- function() x

## setinverse functions sets the inverse of the matrix inside the special matrix or in other words "Caches" it
  setinverse <- function(inverse) inverse_x <<- inverse

## getinverse retruns the Cached inverse of the matrix
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse )
  
}


## CacheSolve calculates the inverse of the matrix.
## If the inverse was not calculated, then in else part the function will calculate it
##    store it in Cache and then return the calculated value.
## If the matrix inverse has already been calculated, it will return that from Cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("Cached inverse found. Returning from Cache.")
        return(inverse_x)
    } else {
## using the solve function to get inverse as instructed
        message("Cached inverse not found. Calculating Inverse and storing in cache for future.")
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }
}
