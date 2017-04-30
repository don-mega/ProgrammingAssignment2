## This program contains two functions that cache the inverse of a matrix.
## Matrix inverson can be a costly computation therefore, caching may provide
## some benefits rather than repeated computation.

## The first function, makeCacheMatrix creates a special "matrix" that can cache its inverse through:

## 1. setting the value of the matrix
## 2. getting the value of the matrix
## 3. setting the inverse of the matrix
## 4. getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list (set = set, get = get, 
              setInverse = setInverse,
              getInverse = getInverse)
              
}


## The second function, cacheSolve calculates the inverse matrix from the makeCacheMatrix function.
## It first checks to see if the inverse has already been computed. If it has, it returns the
## inverse from cache rather than computing. If it has not then it computes the inverse and sets the
## the value in cache using the setInverse function.

cacheSolve <- function(x, ...) {
      
        m < x$getInverse()
        if(!is.null(m))
        {
                return(m)
        }
        m <- solve(x$get()))
        x$setInverse(m)
        m
        
}
