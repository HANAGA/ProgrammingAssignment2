## Put comments here that give an overall description of what your
## functions do
#-----------------------------------------------------------------
# with the help of the "makeCacheMatrix" and "caseSolve" functions
# Inverse of matrices can be found efficiently. 
# Inside a Loop, while dealing with several matrices, 
# if Inverse of any matrix is already calculated, 
# these functions do not calculate the Inverse again


## Write a short comment describing this function
#------------------------------------------------
#  makeCacheMatrix  function is to prepare a matrix
#  Ex: matA <- makeCacheMatrix( matrix(1:4,2,2))
#  here matA represents CacheMatrix only but not the Actual matrix
#  Actual matrix data is returned by get() function only
#  The get() function returns the current matrix Ex:  matA$get()
#  The set() function is to alter the value of matrix at any time Ex: matA$set(matrix(20:29, 5,2))

#when matrix is set by set() method, getmatrix() returns NULL value.
# getmatrix() funtion returns Inverse matrix, when Inverse matrix is existed.
# setmatrix() should not be used by the user. It will be used by cacheSolve function. 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(mtx) m <<- mtx
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
#------------------------------------------------
# here "m" represents Inverse matrix
# if Inverse matrix exits, this function does not execute "solve" function
# thereby it saves lot of processing time, 
# especially if we try to find inverse of several matices inside a Loop
#
# Example:   cacheSolve(matA)
#            invA <- matA$getmatrix()
#            invA                         

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached Matrix")
                return(m)
        }
        matA <- x$get()
        m <- try(solve(matA))
        x$setmatrix(m)
        m
}
