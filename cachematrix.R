## These two functions can be used to set a matrix as input and get its inverse as output.
## If the inverse of the input matrix it's been already calculated (and the matrix is not changed) the inverse is
## read from the cache. Only squared invertible matrices are handled because the solve function is used underneath.

## makeCacheMatrix returns a list with four functions:
## - set		sets the current matrix received as parameter
## - get		gets the current matrix
## - setinverse	sets the inverted matrix
## - getinverse	gets the inverted matrix

## The user can create a special "matrix" object calling the makeCacheMatrix(originalMatrix)

## cacheSolve returns the inverse of the special "matrix" object specified as the input parameter
## reading it from cache if it's been already calculated and the matrix isn't changed 
## or calculating from scratch.

## This function creates a special "matrix" object that can cache its inverse
## and returns a list of functions used to interact with the matrix object and its inverse
makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object returned by the function makeCacheMatrix.
## If the inverse is already been calculated (and the matrix is not changed) then the inverse matrix is retrieved from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
