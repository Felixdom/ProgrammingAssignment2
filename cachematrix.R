##These functions computes the inverse of a matrix and creates a matrix object than can cache it.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()x
         setinverse<-function(inverse)m<<-inverse
         getinverse<-function()m
         list(set=set, get=get,
                 setinverse=setinverse,
                 getinverse=getinverse)
}


## cacheSolve first checks if the inverse of a matrix has already been calculated. If so, it gets the inverse from the cache. 
## Otherwise, it calculates the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
        ## 
        m<-x$getinverse()
         if (!is.null(m)){
                message("getting cached data")
                return(m)
         }
    data<-x$get()
    m<-solve(data,...)  ##Return a matrix that is the inverse of 'x'
    x$setmean(m)
    m
}
