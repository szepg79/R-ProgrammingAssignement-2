## Next pair of function speed up the inverse calculation of matrixes
## No need to execute the time consuming inverse repeatitively,
## but we can check if the inverse was calculated before and if yes 
## we can just take it from cache

## Function creates a special list with four elements
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3     set the inverse
## 4 get the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calculates the inverse of the following matrix. 
## First checks if inverse was already calculated if so, it gets the inverse 
## from cache. Otherwise it calculates the inverse of it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}
