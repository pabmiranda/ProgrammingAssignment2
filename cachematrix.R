## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function to create a special Matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initializing the variable that will hold the value of inverse
    inv <- NULL
    
    ## Assign new value of matrix in parent environment
    ## If there is a new matrix, reset inv to NULL
    
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    ## Define the get function - returns value of the matrix argument
    
    get <- function() x
    
    ## Assigns value of inv in parent environment
    
    setinverse <- function(inverse) inv <<- inverse

    ## Gets the value of inv where called
    
    getinverse <- function() inv
    
    ## In order to refer to functions with the $ operator
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}

## This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated
## (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
