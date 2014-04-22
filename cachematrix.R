## The following functions are responsible of creating a matrix that can cache its inverse. This helps to avoid having to run
##the same expensive computation multiple times. If the inverse has already been calculated it is cached and this is what will be used when the
##same value has to be calculated again. If its not cached yet the function will have to be run.

## The makeCacheMatrix creates a "special matrix" that can cache its inverse.
#It also returns a list that contains functions to 
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix())
  {
    inv<-NULL #initialize the inverse
      set<-function(y)#setter function 
           {   x<<-y 
               inv<<-NULL
            }
    
    get<-function() x #getter function 
    
    setInverse <- function(inverse) inv <<- inverse   #cache inverse value
    
    getInverse <- function() inv
    
    #list of getter and setter functions returned by this function to be called from the cacheSolve function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function that solves for the inverse of a matrix. It first checks if the inverse has not been calculated and cached before
##if its been cached it simply loads the cached value otherwise it computes it
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()#get cached inverse value
  
  #check if value from getInverse is null or populated. if its populated then return it as the inverse otherwise continue to calculate inverse of the function.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)  #return cached inverse value
  }
  data <- x$get()#get cacheable matrix
  
  inv <- solve(data, ...)#call the solve function to compute inverse of square matrix
  x$setInverse(inv)#set the value to be cached.
  inv  #return calculated inverse value.
}
