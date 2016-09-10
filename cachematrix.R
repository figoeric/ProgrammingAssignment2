## Put comments here that give an overall description of what your
## functions do

## Use 'makeCacheMatrix' function to change a regular matrix 
## into one that is able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Use 'cacheSolve' to return a inverse of a matrix. If there is already 
## a inverse in the cache, use the data in the cache. Otherwise calculate the inverse
## and save it in the cache.

## Noted the matrix must be treated by the 'makeCacheMatrix' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
