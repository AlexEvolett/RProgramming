## Function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
## Function that stores a list of functions.
        inv <- NULL
        set <- function(y) {
        ## 'set' function changes the matrix stored in the main function.
                x <<- y
                inv <<- NULL
        }
        get <- function() x 
        ## 'get' function returns the vector 'x' stored in the main function.
        setinv <- function(inverse) inv <<- inverse 
        ## 'setinv'function stores the value of the input in a variable 'inv' into 
        ## the main function.
        getinv <- function() inv
        ## 'getinv' function returns the value stored in a variable 'inv'.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        ## 'list' function stores the 4 functions in the function makeCacheMatrix.
}


## 'cacheSolve' function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}


