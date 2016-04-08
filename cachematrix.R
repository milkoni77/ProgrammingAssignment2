## Put comments here that give an overall description of what your
## functions do

##By analogy from the Mean example

## Write a short comment describing this function
## The function creates a list of functions.
## Get member of the list contains the matrix, Set member caches a new matrix.
## Variable sol is used as a cache for inverted matrix.
## Setsolve sets a value for sol, getsolve returns the "cached" value of sol.

makeCacheMatrix <- function(x = matrix()) {
    sol <- NULL
    set <- function(y) {
        x <<- y
        sol <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) sol <<- solve
    getsolve <- function() sol
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Write a short comment describing this function
## Again, this is an analogy of the Mean example.
## The function receives a matrix as a first argument.
## It checks if a cached value for the inverted matrix exists.
## If Yes, it is returned, and a message is written in the console.
## If No, the whole matrix is taken from the cache, the inverted matrix is
## calculated by calling the Solve function, the result is cached by
## calling Setsolve member of the previous list, and finally the inverted 
## matrix is returned as a result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    sol <- x$getsolve()
    if(!is.null(sol)) {
        message("getting cached data")
        return(sol)
    }
    data <- x$get()
    sol <- solve(data, ...)
    x$setsolve(sol)
    sol
}
