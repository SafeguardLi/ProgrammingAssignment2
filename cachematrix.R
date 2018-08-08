## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) i <<- matrix ##cache its inverse matrix
        getmatrix <- function() i
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## cacheSolve: This function computes the inverse of the special
##"matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should 
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getmatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setmatrix(i)
        i
}
