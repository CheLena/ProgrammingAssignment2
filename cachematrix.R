#Function: makeCacheMatrix
#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#Function: cacheSolve
#This function computes the inverse of the special "matrix"
#returnd by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed), then the
#function should retrieve the inverse from the cache.
cacheSolve <- function (x, ...) {
        inv <- x$getinverse()
        ## If inverse already cached, then return message
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        } else{
                inv <- solve(x$get())
                x$setinverse(inv)
                return(inv)
        }
}
