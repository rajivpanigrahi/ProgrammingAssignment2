# Coding two functions to cache inverse of a matrix
# Two functions created are called makeCacheMatrix and CacheSolve function
# makeCacheMatrix is used to create a special matrix to set and get the value and its inverse of the matrix from cache
# cacheSolve function takes as an argument the matrix of the type create by makeCacheMatrix 
# and computes the inverse of the matrix

# Example of how to use the Cache function:
# ----------------------------------------
# a <-makeCacheMatrix()   Creates special matrix a to store matrix value and its inverse in canche
# b <- matrix (1:4, 2,2)  Create a 2X2 square matrix b
# a$setMat(b)             Set the matrix a by using the value b
# c<-solve(b)             Assigns inverse matrix of b to c
# a$setMat(c)             Sets up Inverse of a which is same as c
# cacheSolve(a)           Returns the cached inverse of a
# getting cached matrix inverse data
#       [,1] [,2]
# [1,]  -2    1.5
# [2,]   1    -0.5

# Coding of the makeCacheMatrix function starts here. 
# makeCacheMatrix is used to create a special matrix to set and get the value of the matrix and to set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
{
        matInverse <- NULL
        setMat <- function(y =matrix())
        {
                x <<- y
                matInverse <<- NULL
        }
        getMat <- function() x
        setInverse <- function(m=matrix()) matInverse <<- m
        getInverse <- function() matInverse
        list(setMat = setMat, getMat = getMat,
             setInverse = setInverse,
             getInverse = getInverse)
        
}
# cacheSolve function takes as an argument the matrix of the type create by makeCacheMatrix 
# and computes the inverse of the matrix

cacheSolve <- function(x, ...) 
        {
        matInverse <- x$getInverse()
        if(!is.null(matInverse)) {
                message("getting cached matrix inverse data")
                return(matInverse)
        }
        matdata <- x$getMat()
        matInverse <- solve(matdata, ...)
        x$setInverse(matInverse)
        matInverse
}
