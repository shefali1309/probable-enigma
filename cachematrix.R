## The functions below creates a special matrix object, which caches its inverse and returns it.

## This function creates a list which contains the following functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will return the inverse of a matrix. 
## If the inverse of the matrix has already been calculated, 
## then it retrieves it from the cache through the 'getinverse' function.
## If the inverse is not cached, then it calculates it.


cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data<-x$get()
        i<-solve(data,...)
        x$setinverse(i)
        i
}