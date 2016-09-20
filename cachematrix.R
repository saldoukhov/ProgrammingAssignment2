## A pair of functions to improve performance when 
## solving matrices via caching

## makeCacheMatrix creates a structure (list) that is 
## used to store cached solution of the matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list(set = set, get = get,
         setsolution = setsolution,
         getsolution = getsolution)
}


## cacheSolve uses the result produced by makeCacheMatrix to 
## either solve the matrix or return the cached result if 
## matrix was previousely solved

cacheSolve <- function(x, ...) {
    s <- x$getsolution()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolution(s)
    s    
}
