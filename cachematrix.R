## First function 'makeCacheMatrix' create an list of 4 function:
## 'set' assignes values of matrix to x
## 'get' is there these values are contained
## same principle for 'setsolve' and 'getsolve'

makeCacheMatrix <- function(x = matrix()) {
    n<-NULL
    set<-function(y){
        x<<-y
        n<<-NULL
    }
    get<-function() x
    setsolve<-function(solve) n<<-solve
    getsolve<- function() n
    list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)

}


## This function checks if 'getsolve' has inverted matrix in it, if not
## this function will calculate it and write down.
## It return inverted matrix of and x.

cacheSolve <- function(x, ...) {
    n<-x$getsolve()
    if(!is.null(n)){
        message("using cached data")
        return(n)
    }
    data<-x$get()
    n<-solve(data,...)
    x$setsolve(n)
    n
}
