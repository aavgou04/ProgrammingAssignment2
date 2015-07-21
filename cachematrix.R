## Both function are calculating the inverse of a matrix. Special thing is that we are trying to save time and resources
## saving the calculated inverse matrix and recall them when we need to calculate them again

## Creates a list of information about a matrix and saves the results

makeCacheMatrix <- function(x = matrix()) {
        k<-NULL
        set<-function(y){
                x<<-y
                k<-NULL
        }
        get <- function () x
        setinv<-function(solve) k<<-solve
        getinv<-function() k
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## Checks whether we have calcultated again the inverse of a specific matrix. If yes it returns the result, otherwise
## it calculates first then saving the result for future use and finally returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        k <- x$getinv()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setinv(k)
        k
}
