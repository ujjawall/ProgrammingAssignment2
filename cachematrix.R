## Following functions uses lexical scoping in R to cache a time computatational problem (matrix inversion)
 

## The first function, makeCacheMatrix creates a special matrix, which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y){
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(inv) inverse <<- inv
     getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of a matrix created 
## with the above function. However, it first checks to see if inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the matrix and sets the
## value of the inverse in the cache via the solve function.
## This function assumes the given matrix is invertible.
## diag() function return an identity matrix and solve() uses A*B=I to get matrix B
## which is inverse of A.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)){
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     identity<-diag(nrow(data))
     inv <- solve(data,identity,...)
     x$setinverse(inv)
     inv
     ## Return a matrix that is the inverse of 'x'
}


