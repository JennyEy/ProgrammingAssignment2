## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is a function that can cache its inverse.
## This is very helpful for the performance of very time-consuming computations.

## This function creates a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y) {
                x<<-y
                i<<-NULL
        }
        get=function() x
        seti = function(inverse) i <<- inverse 
        geti = function() i
        list(set=set, get=get, seti=seti, geti=geti)

}


## This function will give us the inverse of makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        m<- x$geti ()
        if(!is.null(m)) {
                message ("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$seti (i)
        return(i)
        ## Return a matrix that is the inverse of 'x'
}
