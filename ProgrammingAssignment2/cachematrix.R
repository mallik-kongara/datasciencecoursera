## Below are two functions that are used to create a special object that stores
## a matrix and cache's its inverse.

## For the purpose of this assignment it is assumed that the matrix supplied
## is a square matrix (dims(n,n))

## makeCacheMatrix creates a special "matrix" that is really a list containing
## a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

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



## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse of
## the matrix has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}

## Example run from interactive R console:
## > source('~/git-lab/ProgrammingAssignment2/cachematrix.R')
## > a <- makeCacheMatrix()
## > a$set(matrix(1:4, 2, 2))
## > cacheSolve(a)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(a)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
