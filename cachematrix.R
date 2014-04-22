##The function cacheSolve calculates the inverse of the matrix from makeCacheMatrix
##it first checks whether the inverse already exists
##if the inverse already exists it skips the computation and reports the value in the cache
##if the inverse does not exist it calculates and sets the value to the cache and reports the value



#Create list of functions to set and get matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        
        #create an object for the inverse and set value to NULL
        
        mx <- NULL
        
        #set the value of the matrix
        #using the <<- operator the matrix is set outside the current environment
        
        set <- function(y) {
                x <<- y
                mx <<- NULL
        }
        
        #get the value of the matrix
        
        get <- function() x
        
        #set the value of the inverse
        
        setinv <- function(inv) mx <<- inv
        
        #get the value of the inverse
        
        getinv <- function() mx
        
        #create list of functions
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



#Check for existence of inverse matrix, report inverse if found
#if not existent then calculate inverse and add to cache and report value

cacheSolve <- function(x, ...) {
        
        #look for inverse in cache
        
        mx <- x$getinv()
        
        #if the inverse is present return message "getting cached data"
        #and report the inverse matrix
        
        if(!is.null(mx)) {
                message("getting cached data")
                return(mx)
        }
        
        #if the inverse was not found then get the matrix
        
        data <- x$get()
        
        #and calculate the inverse
        
        mx <- solve(x)
        
        #set the value of the inverse to the cache
        
        x$setinv(mx)
        
        #and report the inverse
        
        mx
}




