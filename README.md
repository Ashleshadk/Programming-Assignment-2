Programming-Assignment-2
========================

Cache Inverse Matrix

makeCacheMatrix <- function(x=matrix()) {
    m <- NULL
    get <- function() x
    setImatrix <- function(Imatrix) m <<- Imatrix
    getImatrix <- function() m

    list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}
cacheSolve <- function(x) {
    m <- x$getImatrix()
    if(!is.null(m)){
        message("Found cached data. Done.")
        return(m)
    }
    else {
        message("Cached data not found. Calculating inverse matrix...")
        data <- x$get() 
        m <- solve(data) 
        x$setImatrix(m) 
        message("Done.")
        return(m)
    }
}
