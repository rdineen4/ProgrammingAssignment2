## Put comments here that give an overall description of what your
## functions do

# These functions work together to take an input invertible square matrix,
# calculate the inverse of it, and cache that inverse. That cached value is
# accessable for later use without having to rerun the function that calculated it.
# The first function, makeCacheMatrix, acts as a parent environment for the 4
# functions it creates and thus can store the values of the x and its inverse.
# The 4 functions it creates are used to get and set new values and cached values.
# The second function, cacheSolve, calculates the inverse of the matrix set by
# makeCacheMatrix, caches the inverse, and prints it.

## Write a short comment describing this function

# This creates the set, get, setinv, and getinv functions which are used to
# set a matrix, view the current set matrix, set a cached inverse matrix, and
# view the current cached inverse matrix. These functions are stored as a list.

makeCacheMatrix <- function(x = matrix()) { #set default value of x to be 1x1 matrix with NA value
    inv <- NULL #initializes inv variable
    set <- function(y) { #creates set() which assigns input to parent env. and clears any valid inv if there is one
        x <<- y
        inv <<- NULL
    }
    get <- function() x #creates get() which retrieves x from the parent env
    setinv <- function(inverse) inv <<- inverse #creates setinv() which assigns the inputted inverse to the variable inv
    getinv <- function() inv #creates getinv() which retrieves inv from parent environment
    list(set = set, get = get,
         setinv = setinv, getinv = getinv) #returns the set of functions as a list
}

## Write a short comment describing this function

# This function takes an object of type makeCacheMatrix() and calculates
# the inverse of the given matrix. That inverse is then cached in the 
# makeCacheMatrix() object using the setinv() function. The inverse is also
# printed. If the same matrix is given to it again, the inverse is simply
# printed from the cache, not recalculated.

cacheSolve <- function(x, ...) { #x must be of type makeCacheMatrix()
    inv <- x$getinv() #gets the inv variable from the getinv() func. in x, should be null
    if(!is.null(inv)) { #if inv IS NOT null, inv is simply returned with a message
        message("getting cached data")
        return(inv)
    }
    data <- x$get() #if inv was null, data is retrieved to calculate the inverse
    inv <- solve(data, ...) #calculates inverse of supplied matrix
    x$setinv(inv) #stores inv in the parent env.
    inv #prints the inv object
}

#tests--------------------------------------------------
exmat <- matrix(c(2, 2, 3, 2), 2, 2) #an example square invertible matrix
aMatrix <- makeCacheMatrix(exmat)
aMatrix$get()
aMatrix$getinv()
cacheSolve(aMatrix)
aMatrix$getinv()
cacheSolve(aMatrix) #test when nothing has changed

exmat2 <- matrix(c(-3, 5, 1, 0), 2, 2) #another example square invertible matrix
aMatrix$set(exmat2)#set new matrix
aMatrix$get() #repeating previous tests
aMatrix$getinv()
cacheSolve(aMatrix)
aMatrix$getinv()
cacheSolve(aMatrix) 

