## These functions create a special matrix object that can cache its inverse
## to avoid costly recalculations.

## 1. This function creates a custom "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             # Holds the cached inverse matrix
        
        set <- function(y) {                    # Assigns a new matrix
                x <<- y
                inv <<- NULL                    # Clears the cache if the matrix changes
        }
        
        get <- function() x                     # Returns the raw matrix
        
        setInverse <- function(inverse) inv <<- inverse  # Caches the inverse
        
        getInverse <- function() inv            # Returns the cached inverse
        
        # Returns a list of the internal functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2. This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse is already cached, it retrieves it immediately.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()                   # Checks if an inverse is already cached
        
        if(!is.null(inv)) {                     # If cached data exists, return it
                message("getting cached data")
                return(inv)
        }
        
        # If cache is empty, get the matrix, calculate the inverse, and store it
        data <- x$get()
        inv <- solve(data, ...)                 # solve() calculates the matrix inverse
        x$setInverse(inv)                       # Saves the result back to the cache
        
        inv                                     # Returns the inverse matrix
}
