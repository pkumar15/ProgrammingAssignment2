## Put comments here that give an overall description of what your
## functions do

## make cache matrix 

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL                                       ##initialize inverse matrix with null value
        set <- function(y) {                            ##define function to set matrix x
                x <<- y
                m <<- NULL
        }
        get <- function() x                             ## returns matrix
        setsolve <- function(solve) m <<- solve         ## sets the inverse of matrix x
        getsolve <- function() m                        ## returns the inverse of matrix x
        list(set = set, get = get,                      ## lists the defined functions
             setsolve = setsolve,
             getsolve = getsolve)
}


## find inverse of cache matrix using solve function

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {                               ## checks if the matrix inverse is stored
                message("getting cached data")
                return(m)                               ## returns the inverse of matrix x
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## end of code
