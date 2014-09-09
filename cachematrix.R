## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates an object with a matrix in it and
## functions to get and set the inverse of the input matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

## This function uses the outcome of makeCacheMatrix and checks 
## if the inverse is allready stored (i.e. different from NULL)
## otherwise it will calculate the inverse and store it in the 
## makeCacheMatrix object that you created
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
