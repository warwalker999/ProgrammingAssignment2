## These functions will create a vector that holds the information for a matrix and its inverse, allowing the inverse
## to be cached and retrieved after initial calculation to save future computation time

## makeCacheMatrix returns a vector that holds the get, set, getInverse, and setInverse functions to allow storing
## and retrieving of the matrix X and its inverse. X is a matrix which is assumed to be invertible. 

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL # set the inverse to NULL at the start of the function being called
    set <- function(y) { # when the set function is called, set the matrix x to the value passed (y), and reset the inverse to NULL
        x <<- y 
        inverse <<- NULL
    }
    get <- function() x # when the get function is called, return the matrix x
    setInverse <- function(calcInverse = matrix()) inverse <<-calcInverse # when the setInverse function is called, store the matrix
    # passed (calcInverse) as the inverse of the matrix x
    getInverse <- function() inverse # when the getInverse function is called, return the matrix inverse
    list(set = set, get = get, #at the end of the call, return the functions set, get, setInverse, and getInverse
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve is a version of solve that returns the inverse of a matrix that was created in the makeCacheMatrix function, either 
## through calculating via solve function, or by retrieving the cached solution if the inverse has already been calculated and stored

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse() # set the inverse value to the value returned by x$getInverse()
    if(!is.null(inverse)) { # if the inverse value returned is not null, then print the messsage and return the cached inverse
        message("getting cached data")
        return(inverse)
    }
    data <- x$get() # if execution didn't halt through the return of the cached inverse, then get the matrix value with x$get()
    inverse <- solve(data, ...) # compute the inverse of the matrix with the solve function and store it to the inverse
    x$setInverse(inverse) # store the computed inverse via the x$setInverse() function
    inverse # return the inverse matrix
}
