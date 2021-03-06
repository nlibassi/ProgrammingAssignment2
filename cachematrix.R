## Functions calculate inverse of a matrix or returns cached matrix if one exists

<<<<<<< HEAD
## Write a short comment describing this function
=======
>>>>>>> dc0e0c12c78d05ef2f95d6be416e1c0f37587454
## function takes matrix as input and defines a list of functions for setting and getting 
## matrix and setting and getting inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


<<<<<<< HEAD
## Write a short comment describing this function
=======
>>>>>>> dc0e0c12c78d05ef2f95d6be416e1c0f37587454
## function takes a matrix and calculates inverse if non-null value for inverse not already in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinv()
    
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

<<<<<<< HEAD
=======




>>>>>>> dc0e0c12c78d05ef2f95d6be416e1c0f37587454
