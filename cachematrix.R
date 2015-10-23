## The following pair of functions cache the inverse of a matrix.


#The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#This matrix object is a list containing a function to
#set and get the value of the matrix
#set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(y) {
        x <<- y
        m_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inverse <<- inverse
    getinverse <- function() m_inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


#The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
#If the inverse has already been calculated (and the matrix has not changed), then 
#the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m_inverse <- x$getinverse()
    if(!is.null(m_inverse)) {
        message("getting cached data")
        return(m_inverse)
    }
    data <- x$get()
    m_inverse <- solve(data, ...)
    x$setinverse(m_inverse)
    m_inverse
}