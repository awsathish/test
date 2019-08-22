## Creates matrix object to cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize inverse
    i <- NULL

    ## settor
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## gettor
    get <- function() {
    	## Return matrix
    	m
    }

    ## settor
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## gettor
    getInverse <- function() {
        ## Return inverse property
        i
    }

    ## Return a list of methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get matrix from our object
    data <- x$get()

    ## Calculate inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to object
    x$setInverse(m)

    ## Return matrix
    m
}