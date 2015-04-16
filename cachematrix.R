## Overall the goal of the following two functions 'makeCacheMatrix' and 'cacheSolve'
# is to allow caching of matrix inverses evaluated using 'solve', thus
# reducing processing resources when matrix inverses evaluated earlier 
# are required to be used.
#
# The first function, `makeCacheMatrix` creates a special "matrix", which is
# really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix
#
# The following function 'cacheSolve' calculates the inverse of the special 
# "matrix" created with the above function. However, it first checks to see 
# if the inverse has already been calculated. If so, it `get`s the inverse 
# from the cache and skips the computation. Otherwise, it calculates the 
# inverse of the matrix and sets the value of the inverse in the cache via 
# the `setmean` function.

## 'makeCacheMatrix': Use to create a special 'matrix' which can cache the
# inverse of itself for future use

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(invMatrix) m <<- invMatrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## 'cacheSolve': Use to perform the caching of the special 'matrix'
# created using the 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
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
