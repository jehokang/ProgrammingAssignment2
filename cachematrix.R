## Put comments here that give an overall description of what your
## functions do

## The makeCaxheMatrix function creates a special "vector" which is
## containing following functions
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the inverse matrix
## 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## Returns inverse matrix from the cached data or calls function
## to get inverse matrix if cached data is NULL

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
