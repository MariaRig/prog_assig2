prog_assig2
===========
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    #set the value of matrix
        set <- function(y) {
            x <<- y
            inv <<- NULL
         }
    #get the value of matrix
        get <- function() x
    #set the value of the inverse of the matrix
        setinv <- function(inverse) inv <<- inverse
    #get the value of the inverse of the matrix    
        getinv <- function() inv
        list(set = set, get = get,
            setinv = setinv,
           getinv = getinv)
    }

cacheSolve <- function(x, ...) {
    #check if inverse of the matrix has been previously calculated
        inv <- x$getinv()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
    }
