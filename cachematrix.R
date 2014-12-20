## Define two functions that can calculate cacheable inverse matrix

## This function is assigned to create special matrix object of cacheable inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setSolve<-function(solve) m<<-solve
    getSolve<-function() m
    list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}


## This function is to calculate the inverse matrix of special matrix that function makeCacheMatrix returned.
## If the inverse matrix has been calculated, this function will search the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getSolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setSolve(m)
    m
}
