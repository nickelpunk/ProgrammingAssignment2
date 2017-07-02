## author @nickelpunk
## Given a matrix x, makeCacheMatrix() creates a list of functions that can
## cache the matrix inverse
## cacheSolve() takes the result of makeCacheMatrix() as an input, and checks
## if the inverse of matrix x has already been computed. If it hasn't, then
## it computes that and returns the inverse. If the inverse has already been 
## computed and stored in cache, then it retrieves that inverse and returns 
## that instead

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## finds inverse of matrix or returns cached value for the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
