
## These two functions work together to implement matrix inverse caching. 
## The first function returns a list that includes the creation of a matrix
## inverse. Now, the inverse can be acquired quickly by calling cacheSolve().
## The second function will force a re-computation of the inverse
## if and only if the first function has been called since the most
## recent invocation of the second function.

## makeCacheMatrix() returns a list that includes the internal operations
## needed to cache the inverse of a matrix (using the "solve" function).
##
## makeCacheMatrix() is called with a matrix argument. The list that it returns
## will be used as input by the function cacheSolve(). The list is not 
## immediately useful for other purposes.

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


## cacheSolve() returns an inversion of a matrix. It acquires the inverted
## matrix from cache unless  there has been
## a call to makeCacheMatrix() between this invocation of cacheSolve()
## and its previous invocation. In this case, cacheSolve() will force
## a recomputation of the inverse.

## Note that the user must know to call this function with the output
## of makeCacheMatrix() as the argument. 

cacheSolve <- function(x, ...) {
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
