##  cacheMatrix.R
##  
##  Functions to cache the results of an inverse matrix calculation leveraging
##  lexical scoping to preserve state inside of an R object.
##
##  Directions:
##  --
##  Invoke by calling an object to makeCacheMatrix, as such;
##    objectName <- makecacheMatrix()
##  Populate the Matrices values with (where matrix represents your matrix);
##    objectName$set(matrix)
##  View Matrix Values in objectName;
##    objectName$get()
##  Solve and Inverse Matrix and Cache Results;
##    cacheSolve(objectName)
##  View Inverse Matrix Cache in objectName;
##    objectName$getinverse()

makeCacheMatrix <- function(x = matrix()) {
    # Defines a special matrix object to derive results of an inverse matrix
    #   calculation, cache the results, and return them if called again.
    #
    # Args:
    #   x:  A square matrix object with which to calculate inverse.
    #  
    # Returns:
    #   A special matrix object to hold cached inverse results or return the 
    #   first inverse calculation

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x) {
    # Fetches the previously cached inverse matrix values if present.  If the 
    #   matrix calculation is not cached, computes the inverse matrix and
    #   stores the results into the caching matrix object for future use.
    #
    # Args:
    #   x: caching matrix object with matrix values for calculation
    #
    # Returns:
    #   The inverse matrix values for the cached matrix object values
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

### 
# Execution Sample 
###

## Create Special Matrix Object for Caching
m <- makeCacheMatrix()

## Initialize Matrix with Sample Values
m$set(matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3, byrow=TRUE))

## Retrieve Matrix Values to Confirm
m$get()

## Solve for Inverse Matrix m and Return Results
cacheSolve(m)

## Solve Again to Validate Cached Response
cacheSolve(m)