
## This function takes an invertible matrix and produces a list of functions that sets and gets 
## the matrix  (caching the original matrix) and set and get the inverted matrix (also caching that value of the inverted matrix)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ## defines i, which will be the inverse. Then below the 4 functions are defined
        set <- function(y= matrix()) {
                x <<- y ## Caches x
                i <<- NULL ## Caches y
        }
        get <- function() x ## Calls x
        setinverse <- function(inverse) i <<- inverse ## Caches i with inverse value
        getinverse <- function() i ## Calls i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the makeCacheMatrix(x) function with a matrix x and first checks to see if it has already
## been calculated and cached. If not it calculates the inverse and caches it using a function within makeCacheMatrix()

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse() ## Calls the value of i
        if(!is.null(i)) {
                message("getting cached data")  ## Checks to see if i exists then returns cached data if it exists
                return(i)
        }
        data <- x$get() ## If i doesn't exists it calls the matrix
        i <- solve(data) ## calculates the inverse using solve function
        x$setinverse(i) ## sets the inverse and caches it
        i
}

