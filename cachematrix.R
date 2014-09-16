

## set: store a matrix
## get: fetch a matrix
## setsolve: store a matrix inverse
## getsolve: fetch a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        # intialize the matrix inverse to NULL
        m <- NULL

        # function to set the data and initialize the matrix inverse 
        # to NULL
        set <- function(y) {
                ## set matrix
                x <<- y
                ## set the matrix inverse to NULL because we changed x
                m <<- NULL
        }
        # function to fetch the matrix
        get <- function() x

        # function to set the inverse
        setsolve <- function(solve) m <<- solve

        # function to fetch the inverse
        getsolve <- function() m
        # store the function handles in a list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Compute and return the inverse of the matrix from the cache if
## data has not changed otherwise compute the matrix inverse and 
## return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## fetch the matrix inverse. If we changed the matrix the
        ## matrix inverse will be NULL
        m <- x$getsolve()

        ## check to see if the inverse has already been computed 
        ## (i.e, not NULL)
        if(!is.null(m)) {
                message("getting cached data")
        
                ## return the matrix inverse if it is cached
                return(m)
        }
        ## fetch the the data
        data <- x$get()
        
        ## compute the inverse
        m <- solve(data, ...)
        
        # cache the inverse
        x$setsolve(m)
        m        
        
}
