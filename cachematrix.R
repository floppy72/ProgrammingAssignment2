## This is a submission for Programming Assignment 2 of R PRogramming course
## The goal of the assignment was to learn lexical scoping and pulling cached 
## objects across different functions.

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. The following functions represent a solution to cache the 
## inverse of a matrix.
## 
## We assume that the matrix supplied is always invertible.

## Some examples of use of the functions:
## 
## > mat <- matrix (c(2,2,3,2), nrow=2, ncol=2)
## > matx <- makeCacheMatrix(mat)
## > cacheSolve(matx)
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(matx)
## getting cached data
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0

## > mat <- matrix (c(1,2,1,1,1,-2,2,2,1), nrow=3, ncol=3)
## > matx <- makeCacheMatrix(mat)
## > cacheSolve(matx)
##      [,1] [,2] [,3]
## [1,]   -1  1.0  0.0
## [2,]    0  0.2 -0.4
## [3,]    1 -0.6  0.2
## > cacheSolve(matx)
## getting cached data
##      [,1] [,2] [,3]
## [1,]   -1  1.0  0.0
## [2,]    0  0.2 -0.4
## [3,]    1 -0.6  0.2


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

        setinverse <- function(solve) m <<- solve

        getinverse <- function() m

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function "cacheSolve" calculates the inverse of the special "matrix"
## created with the function "makeCacheMatrix". 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the `setsolve` function

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()

        if(!is.null(m)) {
        	message("getting cached data")
          return(m)
        }

        data <- x$get()

        m <- solve(data, ...)

        x$setinverse(m)

        m
}
