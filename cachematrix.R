## Put comments here that give an overall description of what your
## functions 
##We have two functions: makeCacheMatrix and cacheSolve
## Write a short comment describing this function

##makeCacheMatrix Creates a matrix that can cache its inverse. Is made of set, get, setinv and getinv
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x #gets matrix X
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv 
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# This is the function used to get the cache data

cacheSolve <- function(x, ...) { #Gets cache data
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) { #
                message("getting cached data")
                return(inv) #Returns inverse value
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv #Returns an inverse matrix
}


                        
