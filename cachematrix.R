## Put comments here that give an overall description of what your
## functions do

## The makeCachematrix function creates a special matrix that does four things:
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        
        ### 1.Sets the values of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## 2.Gets the values of the matrix
        get <- function() x
        ## 3.Sets the values of the matrix inverse
        setinverse <- function(solve) m <<- solve
        ## 4.Gets the values of the matrix inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Checks if inverse was already computed, if yes it skips and return cached inverse
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## If  inverse not yet calculated, inverse is solved and then cached     
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Use this to check if our function is accurate
x<- matrix(1:4,nrow = 2,ncol = 2)
solve(x)
y<- makeCachematrix(x)
cacheSolve(y)

cacheSolve(y) == solve(x)


