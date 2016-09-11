## The two functions that I am going to write here help in caching the
## inverse of a matrix.  Matrix inversion is usually
## very computationally intensive - especially for large
## size matrices.


## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
  # 1. set the matrix
  # 2. get the matrix
  # 3. set the inverse of the matrix
  # 4. get the inverse of the matrix





makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {            ## if user wants to reset matrix 
        x <<- y                     ## reassign "new" matrix to x 
        m <<- NULL                  ## reinitialize m to NULL
    }
    get <- function() x
    setInvmatrix <- function(InvMatrix) m <<- InvMatrix
    getInvmatrix <- function() m
    list(set = set, get = get,
         setInvmatrix = setInvmatrix,
         getInvmatrix = getInvmatrix)

}



## Once We create this matrix, We use the cacheSolve
## function to compute the inverse and cache the result


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
    m <- x$getInvmatrix()              
    if(!is.null(m)) {                             ## if user had calculated the same matrix before
        message("getting cached data")  
        return(m)                                 ## return old result(m) directly 
    }
    data <- x$get()                               ## otherwise, get the uncalculated matrix
    m <- solve(data, ...)                         ## calculate the inverse matrix
    x$setInvmatrix(m)                             ## reassign inverse matrix 
    m                                             ## print the inverse matrix 
}

