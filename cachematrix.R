## This pair of functions work together to augment the solve function with
## caching capabilities. This new function can store the result of the solve
## function with the original data so its both easy to retrieve and saves 
## computing the result again.

## Creates a wrapper vector for functions to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse of the matrix stored in the wrapper vector, checking
## first if the inverse has already been stored in cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
