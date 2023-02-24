## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
    ##Initializing the inverse property
    inv <- NULL
    ## Method to set the matrix
    set <- function(y){
        m <<- y
        inv <<- NULL
    }
    ## Method the get the matrix
    get <- function(){
        ## Return the matrix
        m
    }
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        inv
    }
    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}
