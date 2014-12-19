## Two comlementary functions for matrix generation and inversion
## The first matrix will generate a matrix and support internal 
## functions; get, set, getinv, and setinv that are used for 
## reading out the intial matrix values, or the inverted matrix
## The data form the inverted matrix is cached so that it only needs
## to be gernated the first time it is accessed

## The makeCacheMatrix is used to create a matrix with the storage for the 
## data being done with location that is initialized to NULL.  If the function
## is called with no data for creatinga matrix, the function creates an empty matrix.
## Thee makeCacheMatrix function includes 4 internally defined functions, "set",
## "get", "setinv", and "getinv" that can be called to access either the initial
## matrix, inverted matrix, cached matrix, or cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  ## create the matrix with the passed data
    m <- NULL             ## create an empty matrix
    set <- function(y){   ## set function definition
        x <<- y
        m <<- NULL
    }
    get <- function() x   ## get function definition
    setinv <- function(solve) m <<- solve      ## setinv function definition
    getinv <- function() m                     ## get function definition
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cachSolve function is used to manage whether to invert the matrix
## or return a cached version of the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()  ## check the cached inverted matrix
    if(!is.null(m)) {  ## if the cache is not empty, return the cached version
        message("getting inverted matrix")
        return(m)
    }
    ## will get to this point if the inverted matrix is not cached
    data <- x$get()
    m <- solve(data, ...)   ## create an inverted matrix
    x$setinv(m)
    m                       ## return the inverted matrix
}
