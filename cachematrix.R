## The two functions below have the purpose to compute and store Inverse Matrix data in an efficient manner
## The first function makeCacheMatrix creates a special purpose object which wraps the Matrix data with "methods" to access (get) or assign (set) the data
## The second function cacheSolve is the function that actually performs the computation (compute inverse matrix from the user input matrix) and returns the result to the user
## Remark: It is assumed that the matrix data is always invertible


## This function creates a special Matrix object, which is actually a list containing a function that that performs the following four tasks:
## 1) set: Set the matrix data
## 2) get: Get the matrix data
## 3) setminv: Set the inverse matrix data
## 4) getminv: Get the inverse matrix data

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setminv <- function(solve) m <<- solve
    getminv <- function() m
    list(set = set,get = get, setminv = setminv, getminv = getminv)
}


## This function calculates the inverse matrix from the special Matrix object created by the function makeCacheMatrix
## It first checks if the inverse Matrix has been calculated and if so, it returns this data from the cached and here it prints a message that the result output was obtained from cached data
## If the inverse Matrix has not been calculated yet, it calculates the Inverse Matrix and it caches this data via the setminv function

cacheSolve <- function(x, ...) {
    m <- x$getminv()
    if(!is.null(m)) {
        message("Getting Inverse Matrix Cached Data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setminv(m)
    m
}

## Test example running the code and also R Output with included added comments. They are marked by using double hash characters ##
## This is creating a matrix
# > m1 <- matrix(c(1,3,55,22),nrow=2,ncol=2)
## This is creating a special matrix object using makeCacheMatrix
# > n1 <- makeCacheMatrix(m1)
## This is running cacheSolve for the first time
# > cacheSolve(n1)
## This is the output by running cacheSolve(n1) for the first time
#            [,1]         [,2]
#[1,] -0.15384615  0.384615385
#[2,]  0.02097902 -0.006993007
## This is running cacheSolve for the second time
#> cacheSolve(n1)
## This is the output by running cacheSolve(n1) for a second time. Note the message explaning that the below results comes from the cach
#Getting Inverse Matrix Cached Data
#            [,1]         [,2]
#[1,] -0.15384615  0.384615385
#[2,]  0.02097902 -0.006993007