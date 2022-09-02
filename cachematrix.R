## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function receives a matrix and caches the result

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
            set <- function(y) {
                x <<- y
                inv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}



## Write a short comment describing this function
## This function receives the cached matrix, inverses it and returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
            if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinv(inv)
            inv
}
