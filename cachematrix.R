## Sample usage demo. This is what the functions should do.
## > z <- makeCacheMatrix(mat)
## > cacheSolve(z)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

makeCacheMatrix <- function(x = matrix()) {
    ## takes a matrix as an argument
    ## and returns an object that has methods get, set, getinv & setinv
    ## get & set are used to retrieve & set the given matrix
    ## while getinv & setinv retrieve & set the inverse of the matrix

    ## define our inverse matrix, (assigning actual inverse later)
    inv <- NULL
    ## following sets a new matrix value to x
    set <- function(y) {
        x <<- y
        ## reset our inverse matrix (overwrite/discard previous value)
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x) {
    ## Return the cached inverse of given matrix (with a log message)
    ## If the matrix's inverse is not in the cache,
    ## compute it, cache it & then return it

    ## get the cached inverse
    m <- x$getinv()
    ## check if the inverse is not null (i.e. undefined, yet)
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## get the matrix whose inverse it to be calculated
    data <- x$get()
    ## calculate the inverse of that matrix
    m <- solve(data)
    ## cache the inverse
    x$setinv(m)
    ## return the inverse
    m
}
