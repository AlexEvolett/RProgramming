## Put comments here that give an overall description of what your
## functions do

## Function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #a function that stores a list of functions.
        inv <- NULL
        set <- function(y) {
        #a function that changes the matrix stored in the main function.
                x <<- y
                inv <<- NULL
        }
        get <- function() x #a function that returns the vector x stored in the main function.
        setinv <- function(inverse) inv <<- inverse 
        #a function that store the value of the input in a variable inv into the main function.
        getinv <- function() inv
        #a function that return the value stored in a variable inv.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        #store the 4 functions in the function makeCacheMatrix
}


## function computes the inverse of the matrix returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.

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


