## makeCacheMatrix creates a list containing a function to  
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse of the matrix
## d) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## casehSolve returns the inverse of a matrix given the matrix is invertible
## If inverse is already computed it retrieves it from cache and returns it else it computes 
## the inverse of the matrix and caches before returning it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("retrieving data from cache.")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
