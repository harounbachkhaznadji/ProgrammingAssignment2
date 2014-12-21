## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
I <- NULL
makeCacheMatrix <- function(x = matrix()) { 
        
        ## Assign matrix with solve calculation in Cached variable I, so you can used
        ## in cacheSolve function to determine if we read from cache or calculate again
        I <<- solve(x)
         
 }
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##  solve(x)%*%x
        
        I <<- solve(x)%*%x
        # Retrives the most recent value for the inverse
        if(!is.null(I)){
                message("getting cached data")
                return(I)
                # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value
        }else
        {
        # If the value of Inverse is NULL, then you retrive matrix x and calculate the inverse with the solve() function
        message("newly calculating data")
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        
        # Sets Inverse to the newly calculated value
        return(I) #Returns the new Inverse value
        }
}
