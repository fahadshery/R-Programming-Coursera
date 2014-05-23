## Create a cacheMarix that can be used to calculate the inverse of a matrix if it hasn't been done before.
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to: 

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

## FUNCTION DESCRIPTION:
## create a special "matrix" consists of a list
## 1. check whether the input is a matrix
## 2. set the value of the matrix
## 3. get the value of the matrix
## 4. set the inverse of the matrix
## 5. get the inverse of the matrix


makeCacheMatrix <- function(matrix = matrix()) {
        
        #check if the object passed is a matrix...
        if(!is.matrix(matrix))
        {
                stop("Object passed must be a matrix!")
        }
        
        ## reset/initialise the cache
        inversedCache <- NULL
        
        ##set up the matrix
        set <- function(y) {
                matrix <<- y
                inversedCache <<- NULL
        }
        
        ## getter method to return the Inverse Cached Matrix
        get <- function() matrix
        
        setInverse <- function(inverse) inversedCache <<- inverse
        getInverse <- function() inversedCache
        
        #setup list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of
## the inversed matrix in the cache via the setInverse function.

## 1. check to see if the inverse of x has already been calculated
## 2. if so, return the Inverse in the cache via the getInverse function
## 3. otherwise, calculate the inverse of the matrix
## 4. And set the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## get the current cached Inverse
        m <- x$getInverse()
        #check if its calculated before and not empty
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #else calculate the Inverse and cache it
        data <- x$get()
        results <- solve(data, ...)
        x$setInverse(results)
        results
}

## EXAMPLE USEAGE
myMatrix = matrix(1:4,2,2)
x = makeCacheMatrix(myMatrix)
class(x)
cacheSolve(x)
