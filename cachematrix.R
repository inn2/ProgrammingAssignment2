## This script contains two functions to cache the inverse of a
## matrix rather than compute it repeatedly

## Function named "makeCacheMatrix" is a list containing a function
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverted matrix (Square matrix only)
## 4. get the value of the inverted matrix (Square matrix only)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(matrix) m <<- matrix
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)
}


## Function named "cacheSolve" calculates the inverse of the matrix
## created with the "makeCacheMatrix" function or retrieves the 
## cached results if the inverse of the matrix has been calculated prior

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmatrix(m)
    m
}
