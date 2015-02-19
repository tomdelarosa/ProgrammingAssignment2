## Author: tomdelarosa
## This function create a special object matrix
## wichi is able to cache its inverse

## This creates the special matrix
## Access to matrix should be done through $set() and $get() functions
makeCacheMatrix <- function(x = matrix()) {
    invcached <- NULL

    set <- function(y) {
        x <<- y
        invcached <<- NULL
    }

    get <- function(){
        x
    }

    setinv <- function(inverse){
            invcached <<- inverse
    }
    
    getinv <- function(){
        invcached
    }

    list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## This returns the matrix inverse and caches the computed inverse
## in case it has not been computed yet for the last matrix update 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
        if(!is.null(inv)) {
                
                return(inv)
        }
        matrixdata <- x$get()
        inv <- solve(matrixdata)
        x$setinv(inv)
        inv

}
