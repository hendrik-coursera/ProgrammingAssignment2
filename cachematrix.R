## This collection of R functions provide a means to represent special
## matrix objects (represented as a list) that can cache their inverse.
## This caching is done for efficiency reasons:
## When first querying the matrix' inverse, the inverse is computed.
## All subsequent queries for the matrix' inverse will avoid the computation and
## use the cached inverse if the matrix hasn't changed.

## This function returns a list that represents a special 'matrix object'
## which can store its own inverse. Use cacheSolve to access the matrix' inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    my_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        my_inverse <<- NULL
    }
    get <- function() {
        x
    }   
    setsolve <- function(solve) {
        my_inverse <<- solve   
    }
    getsolve <- function() {
        my_inverse
    }
    
    # return a list, which cacheSolve can consult for a cached inverse
    # so that the inverse be only computed once   
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function expects a list created by makeCacheMatrix (so-called special
## 'matrix object') as argument. It returns the matrix' inverse, by either
## using the cached value, or computing it in case there is no chached value.

cacheSolve <- function(x, ...) {
    my_inverse <- x$getsolve()
    if (!is.null(my_inverse)) {
        message("getting cached data")
        return(my_inverse)
    }
    data <- x$get()
    my_inverse <- solve(data)
    x$setsolve(my_inverse)
    
    ## Return a matrix that is the inverse of 'x'
    my_inverse
}