## makeCacheMatrix and cacheSolve are pair of functions that cache the inverse 
# of a given (square) matrix in order to save on computation time and resources 
# when given a large matrix. Rather than re-computing the inverse it will check
# to see if a previously cached value exists.

## makeCacheMatrix: This function creates a special "matrix" object that can 
#cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
        # inverse will store the inverse of the input vector
        inverse <- NULL
        
        # set will initialize the input vector x
        set <- function(y) {
            x <<- y
            inverse <<- NULL
        }
        
        # get will return the current state of x
        get <- function() x
        
        # setinverse will compute the inverse of input matrix x and store it in inverse
        setinverse <- function(inverse) inverse <<- inverse
        
        # getinverse will return the current state of inverse
        getinverse <- function() inverse
        
        # this will create/return a list embeding each function within the makeCacheMatrix()
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
        # initialize im with the return value of the special "matrix" created with makeCacheMatrix()
        im <- x$getinverse()
        
        # check to see if m has been initialized or not,
        # if not null (empty) return what was computed
        if(!is.null(im)) {
            message("getting cached data")
            return(im)
        }
        
        # im is null, get the vector to compute the mean on
        data <- x$get()
        
        # compute the inverse
        im <- solve(data, ...)
        
        # set the inverse 
        x$setinverse(im)
        
        # return the last argument of function, in this case im, or the inverse of x.
        im
}
