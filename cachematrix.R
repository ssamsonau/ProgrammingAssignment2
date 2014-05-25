## Functions 'makeCacheMatrix'and 'cacheSolve' provide a way to find an inverse
## of a matrix without recalculating already found value of inverse


## 1) Function 'makeCacheMatrix' takes a matrix as an argument and create
##    a special "matrix" object that can cache its inverse in variable inv_s
## 2) To initialize or change this object function 'set' should be used 
##    (inverse will be set to NULL in both cases)
## 3) To read the matrix use a function 'get'
## 4) To set or read inverse use 'setInv' or 'getInv' respectivelly

makeCacheMatrix <- function(x = matrix()) {
    inv_s <- NULL ##inverse saved
    set <- function(y) {
        x <<- y
        inv_s <<- NULL
    }
    get <- function() x
    setInv <- function(inv_c) inv_s <<- inv_c ##inv_c is inverse calculated
    getInv <- function() inv_s
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}

## Function 'cacheSolve' takes as an argument a "matrix" object created by 
## 'makeCacheMatrix' function and calculates it's inverse
## A cashed value for the inverse is taken if the inverse was already calculated 
## and the matrix was not changed 

cacheSolve <- function(x) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting an inverse for the matrix from cache")
        return(invisible(inv))
    }
    data <- x$get()
    if (is.na(data[1, 1])) return(message("No matrix given to calculate an inverse"))
    inv <- solve(data)
    x$setInv(inv)
    invisible(inv)
}
