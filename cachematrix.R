## Put comments here that give an overall description of what your
## functions do

## stores the value of the matrix, and creates variable to store the inverse along with their getters and setters

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y){
        x <<- y
        I <<- NULL
    }
  
    get <- function() x
    setInverse <- function(inverse) I <<- inverse
    getInverse <- function() I
    list(set = set, get = get,
         getInverse = getInverse, setInverse = setInverse)

}


## Computes the inverse of the matrix, computes the inverse and caches it on the first call, uses the cached value for subsequent calls

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getInverse()
    if(!is.null(I)){
        message("getting cached data")
        return(I)
    }
    
    data <- x$get()
    size <- ncol(data)
    idMatrix <- diag(size)
    I <- solve(data,idMatrix)
    x$setInverse(I)
    I
}