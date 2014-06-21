## cachematrix.R - functions to create and handle a special matrix
## capable of caching the value of its inverse rather than 
## compute it repeatedly


## This function creates a special matrix wich actually
## is a list that contains two functions to set and get the value 
## of the matrix and two function to set and get the cached
## value of the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function returns the inverse of the matrix (created
## using makeCacheMatrix) if the inverse has already been 
## calculated the function immediately return the cached
## result, otherwise it will calculate the result and save
## it in the internal cache.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
