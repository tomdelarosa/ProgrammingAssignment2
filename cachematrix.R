## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
        if(!is.null(inv)) {
                #
                return(inv)
        }
        matrixdata <- x$get()
        inv <- solve(matrixdata)
        x$setinv(inv)
        inv

}
