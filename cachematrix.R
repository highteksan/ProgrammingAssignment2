## Programming Assignment 2
## Highteksan
## This R program contains two functions and code to test the two functions
## 
## makeCacheMatrix is a function that has the following purpose.
## 1. Take the matrix argument x and assign it to the variable y.
## 2. It returns a a list of functions that are passed to the solveCache 
##    function. The functions are as follows.
##    1. set() puts the matrix to be inverted into y
##    2. get() gets the matrix 
##    3. getinverse() gets the inverse of the matrix when it exists
##    4. setinverse() calculates the inverse of the matrix and cache it
## 3. solveCacheMatrix takes the list of functions created by makeCacheMatrix.  
##    It then uses those functions to check to see if the invers is in cache.  If not
##    It calculates the inverse using solve() and returns it.

## This is the test program code
## Build a 4x4 invertable test matrix
matrixdata <- c(1,2,3,4,3,2,2,4,1,2,4,4,1,7,3,5)
testmatrix <- matrix(matrixdata, 4, 4)

## Create the special matrix and store the invertable 
## matrix in memory using the makeCacheMatrix function
specialMatrix <- makeCacheMatrix(testmatrix)

## Calculate the inverse using cacheSolve and pass the special matrix as 
## arguments.  The loop calls the function 3 times.  The first time this
## is called, the message "Cached data not found" is displayed.  The second 
## two times the message "gettin cached data" is displayed indicating that
## the cached data is available.

for (i in 1:3)
{
  inverse <- cacheSolve(specialMatrix)
}
## end of test program

## Function makeCacheMatrix
## argument is a square invertable matrix
## returns a list of functions that can be used to get, set, getinverse, setinverse
## matrix in cache.  
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ## store the matrix x to be inverted in y
    x <<- y
    m <<- NULL
  }
  get <- function() x ## gets the matrix to be inverted x
  setinverse <- function(inverse) m <<- inverse ## sets the calculated invers in cache
  getinverse <- function() m ## gets the inverse matrix from cache
  ## return the list of functions
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  }
## Function cacheSolve
## Takes as arguments the functions list from makeCacheMatrix and returns the inverse matrix
## If the inverse matrix is not in cache it calculates it using solve(), stores in cache and 
## returns the inverse. If the inverse matrix is in cache it is returned.
## messages are included to let the user know if the inverse came from cache or was 
## calculated for the first time.

cacheSolve <- function(x, ...) {
  m <- x$getinverse() ## get the inverse matrix from cache
  if(!is.null(m)) { ## if it is not null then return the inverse
    message("Getting cached data")
    return(m)
  }
  data <- x$get() ## if the inverse is null then calculate using solve and return.
  m <- solve(data)
  x$setinverse(m)
  message("Cached data not found")
  m
}



        
