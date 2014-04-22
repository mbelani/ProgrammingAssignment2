##
## makeCacheMatrix - provides a list of member functions that assist in
##   caching the inverse of a matrix.  (Note: the <<- operator is used to
##   assign a value to an object in an environment different from the
##   current environment).
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## [Re-]sets the matrix, and essentially invalidates the cache (m is set
    ##   to NULL).  This will force a recomputation of the inverse the next
    ##   time.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    ## Set the inverse (in the cache).
    setinv <- function(inv) m <<- inv
    
    ## Get the cached inverse.
    getinv <- function() m
    
    ## Return a list of the member functions.
    list( set = set, get = get, setinv = setinv, getinv = getinv )
}

##
## cacheSolve calculates the inverse of a matrix, using the above function.
##   It first checks to see if the inverse has already been calculated.  If
##   so, it simply "get"s the inverse from the cache and skips the computation.
##   Otherwise, it calculates the inverse, and stores this in the cache for
##   subsequent, quick retrieval.
##
cacheSolve <- function(x, ...) {
    ## Get the inverse.  Note that this may be null (if not yet cached).
    m <- x$getinv()
    
    ## If data is available in the cache, simply return it.
    if( ! is.null(m)) {
        message("getting cached data...")
        return(m)
    }
    
    ## Data isn't availabe in the cache...re-compute.
    data <- x$get()
    m <- solve(data, ...)
    
    ## Set up the cache.
    x$setinv(m)
    
    ## Return the inverse
    m
}
