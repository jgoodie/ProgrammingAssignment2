## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
#  Function to create a special "matrix" object that can cache it's inverse
#  get/set to get and set the original matrix
#  getmatrix/setmatrix to get and set the inverse of the matrix
#

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x
        }
        setmatrix <- function(mtrx) {
                m <<- mtrx
        }
        getmatrix <- function() {
                m
        } 
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
#
#  Function to calculate the inverse of the special "matrix" returned by makeCacheMatrix()
#  If the inverse has alread been calculated (and the matrix has not changed), then 
#  cacheSolve() should return the inverse from the cache
#  cacheSolve() assumes the matrix given is always invertible
#  If the matrix given cannot be inverted solve()/cacheSolve() will generate an error.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m        
}

###########################
# Quick and dirty QA tests
###########################
#
# mx1 <- matrix(c(1,1,4,0,3,1,4,4,0),nrow=3, ncol=3)
# x<-makeCacheMatrix(mx1)
# cacheSolve(x)
#
# re-try with non-invertable matrix
#
# mx1 <- matrix(c(2,4,3,1,5,7),nrow=3, ncol=3)
# x<-makeCacheMatrix(mx1)
# cacheSolve(x)
