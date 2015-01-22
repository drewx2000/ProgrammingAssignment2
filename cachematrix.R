## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is derived from the cachemean example,
## with the exception that makeCacheMatrix now attempts to invert the matrix
## instead of the mean.

## define function accepting a matrix argument x = matrix

makeCacheMatrix <- function(x = matrix()) {

## if called for the first time, create a null object "i" in parent env
## set - sets x as the matrix and stores x in parent env. Also clears "i" obj
## get - returns matrix x
## setinverse - solve() matrix x, then store it in "i" object in parent env
## getinverse returns the cached "i" value 

        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i

## returns a list that contains the functions for setting or getting
## a matrix or its inverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## getinverse attempts to get value of i, which is the cached inverse solution
## if NULL, attempt to get, solve() and store data listed in argument.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}