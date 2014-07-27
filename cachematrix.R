# Calculating the inverse of a matrix can be a costly computation, so 
# it can be worthwhile to cache the inverse of a matrix rather than compute it 
# each time it is needed.
# The two functions below cache a matrix inverse.


## makeCacheMatrix: a function that creates a matrix object and caches its inverse
## cacheSolve: computes the inverse of the matrix object returned by makeCacheMatrix


## Creates a list containing a function to
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the value of inverse of the matrix
## 4. Gets the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## Returns the inverse of the matrix. 
## 1. If matrix inverse has already been computed in getmatrix(), take that result and skip the solve() calculation.
## 2. Otherwise, calculate the inverse of the matrix and set,atrix() its value in the cache.
## setinverse function.
cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}

## Sample runs:
## x = rbind(c(1, -1/4), c(-1/4, 1))
## m = makeCacheMatrix(x)
## m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
