# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix returns a list with getter and setter functions to get & set
# a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setmatrix <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getmatrix <- function() x
        setmatrixinverse <- function(inverse) inv <<- inverse
        getmatrixinverse <- function() inv
        list(setmatrix=setmatrix, getmatrix=getmatrix, setmatrixinverse=setmatrixinverse, getmatrixinverse=getmatrixinverse)
}


#cacheSolve returns the inverse of a matrix, checking first to see if the matrix is cached.
#if matrix is not cached, it's created and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getmatrixinverse()
        if(!is.null(inv)) {
                message("returning the inversed matrix from cache")
                return(inv)
        }
        data <- x$getmatrix()
        inv <- solve(data)
        x$setmatrixinverse(inv)
        inv
}
