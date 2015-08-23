## This R File contains two functions - makeCacheMatrix and cacheSolve.
## These functions will help cache inverse of a matrix for future reference.


## makeCacheMatrix function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        ##function for cachine the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ##get function
        get <- function() x
        
        ## function for setting the inverse
        setInvMatrix <- function(invMat) inv <<- invMat
        
        ## get inverse
        getInvMatrix <- function() inv
        
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)

}


## cahceSolve function computes the inverse of the special matrix object returned
## by makeCacheMatrix function. If the inverse has already been
## calclated, then this function just retrieves the inverse from 
## cache
## first argument will be the vector
## second argument obj will be cached special chached object

## instructions for execution of this function
## z<-makeCacheMatrix(a)
## cacheSolve(a, z)


cacheSolve <- function(x, obj, ...) {
        ## Return a matrix that is the inverse of 'x'
        

        ## check the matrix if it has changed or not
        if(!identical(x,obj$get())) 
        {
                obj$set(x)
                message("INFO: Arugment vector is different from cached data in the object")
                
        }
        ## get the inverse from cache if it is not null
        
        m <- obj$getInvMatrix()
        if(!is.null(m)) {
                message("INFO:getting cached data")
                return(m)
        }
        
        ## calculate it if it is null
        data <- obj$get()
        m <- solve(data)
        
        ##set it back in the object
        obj$setInvMatrix(m)
        m
}
