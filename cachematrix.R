## this code follows the coding as provided in the example for assignment
## makeCacheMatrix creates a special matrix object

## As mentioned this functions follows the example
## The get and set are for matrix and
## getinverse and setinverse get and set the inverse of matrix. 

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
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
        message("getting cached inverse matrix")
        return(inverse_x)
    } else {
## using the solve function to get inverse as instructed
        inverse_x <- solve(x$get())
        x$setinverse(inverse_x)
        return(inverse_x)
    }
}
