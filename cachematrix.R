makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x #specifies the nature of get. later used to solve the matrix in cacheSolve
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
        m <- x$getmatrix()                      ##
        if(!is.null(m)) {                       ##This if returns the message when the cached data is being called by the function
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   #Solves the cached matrix
        x$setmatrix(m)          #
        m
}
