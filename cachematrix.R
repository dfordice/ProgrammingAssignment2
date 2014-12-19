## Two functions that will compute and cache the inverse of matrix x
## 

## makeCacheMatrix creates a vector containing a list of four functions:

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        ## set(y) when called will assign the value of the matrix to be inverted
        ## and assign to the free variable s the NULL value indicating the cache is empty
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        ## get() returns the assigned value of x
        get <- function() x
        
        ## setsolve(solved) assigns to s the inverted matrix and caches it,
        ## assuming the function being passed to makeCacheMatrix()$setsolve is "solve"
        setsolve <- function(solved) s <<- solved
        
        ## getsolve() returns the assigned value of s (NULL if no matrix is inverted)
        getsolve <- function() s
        
        ## list the list of functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve will return a matrix that is the inverse of 'x'
## If the inverse of 'x' is already computed and cached, it will return the cached
## solution

cacheSolve <- function(x, ...) {

        ## set 's' using makeCachedMatrix$getsolve
        s <- x$getsolve()
        
        ## test to see if the matrix inverse has already been cached
        ## if so, return 's', else get the matrix and compute the inverse
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        
        ## use makeCacheMatrix$setsolve to cache the solution
        x$setsolve(s)
        
        ## print the solution 's'
        s
}
