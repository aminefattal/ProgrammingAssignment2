##makeCacheMatrix:

#  This function creates a special list object that contain the original matrix and function to get 
# the calculated inverse if Not NULL, and set the calculated inverse if in fact it was NULL.
 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



##cacheSolve:

#This function takes on as argument a list object of the makeCacheMatrix type. It uses the getinverse function
#to retieve any available matrix in the list, and run it through an if function to see if its NULL. 
# If its NULL then it goes on to use the matrix to calculate the invere. Later, it used another function from the argument list,
#setinverse to set the value of m in the parent invironment. 


cacheSolve <- function(x, ...) {
             ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}
