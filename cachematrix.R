## This function sets up the matrix and result variables. It also defines how the 
## functions set, get, setsolve and getsolve operate. 
## Variables x and m inside the set function are available to the parent function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function first checks to see if a cached value exists for the solve 
## function, and returns it. Otherwise it retrieves the values by 
## recalculating using the solve function, the sets the cached result for next run.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
