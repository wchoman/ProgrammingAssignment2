## A method to create a matrix with a cached inverse
##   and a method to solve the inverse in the cache

## Create a matrix object with cache inverse method

makeCacheMatrix <- function(x = matrix()) {
        # Init invCache; invCache is the cache of the inverse of tthe matrix x
        invCache <- NULL
        # Set method:  set x to value passed in parameter y; x is the matrix data.
        # Since the matrix changed, also sets invCache to NULL (i.e.invalidates cache) 
        set <- function(y) {
                x <<- y
                invCache <<- NULL
        }
        # Get method: return the matrix in x
        get <- function() x
        # Set cache method:  set the invCache var (in enclosing env) to the value
        #   passed in the solve parameter
        #  IMO, this method should be called setInvCached!
        setsolve <- function(solve) invCache <<- solve
        # Get cache method:  returns the value in invCache
        getsolve <- function() invCache
        # List method
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Solve the inverse of matrix in x, and store it in x's invCache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the cached inverse (it may be NULL, indicating its not yet cached)
        invCache <- x$getsolve()
        # if not NULL, return the cached value 
        if(!is.null(invCache)) {
                message("getting cached data")
                return(invCache)
        }
        # implied else, invCache must have been NULL, so we need to calculate it
        # first, get the matrix data
        data <- x$get()
        # solve the inverse and store it in a local var
        #    this is the only place where the inverse is actually caclulated!
        invCache <- solve(data, ...)
        # set the inv matrix cache in x to the result of our local solve
        x$setsolve(invCache)
        # return the value of our local solve
        invCache        
}
