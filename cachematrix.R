## Programming Assignment 2: Lexical Scoping
## By Andrés Felipe Niño Acosta 

## It is about applying the advantages of the scope rules of the R language and
## next to this create a special object (matrix). This matrix is invertible and
## the proposed code allows capturing its inverse matrix,which has already been
## calculated and the initial matrix has not changed

## the function makeCacheMatrix allows to elaborate the matrix object and from 
## this matrix it can be stored to capture its inverse

makeCacheMatrix <- function(M = matrix()) {
     N <- NULL
     set <- function(J) {
          M <<- J
          N <<- NULL
     }
     get <- function() M
     setinverted <- function(solve) N <<- solve
     getinverted <- function() N
     list(set = set, get = get,
          setinverted = setinverted,
          getinverted = getinverted)
}

## the cacheSolve function calculates the inverse of the "matrix" returned by 
## makeCacheMatrix. Since the inverse has already been calculated (and the 
## matrix has not changed), the cacheSolve function retrieves the inverse from 
## the cache.

cacheSolve <- function(M, ...) {
     ## Return a matrix that is the inverse of 'M'
     N <- M$getinverted()
     if(!is.null(N)) {
          message("getting cached data")
          return(N)
     }
     data <- M$get()
     N <- solve(data, ...)
     M$setinverted(N)
     N
}
## Thanks for reviewing my assignment 2 