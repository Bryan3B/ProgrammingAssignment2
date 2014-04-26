## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse matrix. The first function, 
## makeCacheMatrix creates a "matrix", which is really a list containing a 
## function to 
##     1. set the value of the matrix 
##     2. get the value of the matrix 
##     3. set the value of the inverse matrix
##     4. get the value of the inverse matrix


## makeCacheMatrix creates a list "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv_x <<- solve
        getsolve <- function() inv_x
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve computes the inverse of the list "matrix" returned by 
## makeCacheMatrix. If the inverse has previosly been calculated, then it is 
## retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getsolve()
        if(!is.null(inv_x)) {
          message("getting cached data")
          return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setsolve(inv_x)
        inv_x
}
