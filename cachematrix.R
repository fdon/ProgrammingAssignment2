
#In R the free variable bindings are resolved by first looking in the environment
#in which the function was created. This is called lexical scope.
#In this exercise, we are taking advantage of this feature of R to create a special matrix object
#that can cache its inverse.


#The first function creates a special "matrix object" as a list containing 4 functions to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the matrix inverse
# 4.  get the value of the matrix inverse
#The environment in which these 4 functions are defined contains the variables x and inverse
#(that respectively point to the matrix and its inverse).
#Because of the lexical scoping in R, the 4 functions have access to the values of these 2 variables.
#These values can then be modified using the "superassignment" operator, "<<-".


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(y) inverse <<- y
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The second function calculates the inverse of the special "matrix object"
# created with the first function. It first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse`
# function.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}


#Test
m1 <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)   #initialize a simple matrix
m1                              #look at it
m2<-makeCacheMatrix(m1)         #create the special matrix object that can "cache its inverse"
m2                              #see what it contains
str(m2)                         #see what it contains (it's a list of 4 elements)
cacheSolve(m2)                  #Find its inverse (by computing it)      
cacheSolve(m2)                  #Find its inverse again (this time by looking at the cache)
