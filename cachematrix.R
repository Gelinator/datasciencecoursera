makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y                 ##Value of "x" is set as "y", in a global environment
                m <<- NULL              ##Value of "m" is set as "NULL", in a global environment
        }
        get <- function() x             #specifies the nature of get. later used to solve the matrix in cacheSolve
        setmatrix <- function(solve) m <<- solve  
        getmatrix <- function() m
        list(set = set, get = get,      #Creates a list of equivalences in global environment
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


cacheSolve <- function(x, ...) {
        m <- x$getmatrix()              ##Fetches the matrix in the global environment, specified by "x"
        if(!is.null(m)) {               ##"if" returns the message when the cached data is being called by the function, only if m is not NULL
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)   #Solves the cached matrix in this environment
        x$setmatrix(m)          ##Sets the solved matrix "m" as the matrix in the global environment through "x"
        m                       #returns the solved matrix
}
