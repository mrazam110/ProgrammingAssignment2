## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # set value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL # matrix has changed, reassign to NULL
    }
    
    # get value of matrix
    get <- function() x
    
    # set inverse of matrix
    setinverse <- function(inverse) inv <<- inverse
    
    # get inverse of matrix
    getinverse <- function() inv
    
    # return a list containing all functions defined above
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # get inverse
    inv <- x$getinverse()
    
    # if inverse exists, check if already cached
    # if yes, return cached inverse
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # if not, get matrix
    data <- x$get()
    
    # compute inverse of matrix
    inv <- solve(data, ...)
    
    # cache inverse of matrix
    x$setinverse(inv)
    
    # return inverse
    inv
        ## Return a matrix that is the inverse of 'x'
}
