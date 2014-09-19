## Put comments here that give an overall description of what your
## functions do take an arbitrary matrix, cache its inverse, and compute the inverse as well.
## If a user sets the inverse of the matrix, then computation does not take place later.

## Write a short comment describing this function
## makeCacheMatrix takes a matrix, and caches the inverse of the matrix in it.
## This is a list of several functions.
## Each function sets, gets, sets the inverse of, and gets the inverse of a given matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Write a short comment describing this function
## This function first checks whether the inverse of a matrix has been calculated.
## If so, it returns the already calculated one.
## Otherwise, it uses the solve function and returns the inverse of the matrix, saved in makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
