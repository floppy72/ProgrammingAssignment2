## Put comments here that give an overall description of what your
## functions do

## The function "makeCacheMatrix" creates a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of a matrix
## get the value of the inverse of a matrix

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


## The function "cacheSolve" calculates the inverse of the special "matrix"
## created with the function "makeCacheMatrix". 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the `setsolve` function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
        	message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
