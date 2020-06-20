## Assignement 2, functions for cache inverse of matrice

## makeCacheMatrix creates a special matrix for cache inverse

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y=matrix()){
        x<<-y
        i<<-NULL
    }
    get<-function()x
    setinverse<-function(inverse)i<<-inverse
    getinverse<-function()i
    
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve check if there is cached inversed matrix, If there is, with get the cache. If there is't, will calculate.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data, ...)
    x$setinverse(i)
    i
}
