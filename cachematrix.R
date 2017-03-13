## argument x isa square invertible matrix
## The function returns a list containing four functions which
##    1) set the matrix
##    2) get (return) the matrix
##    3) set the inverse
##    4) get (return) the inverse
## this list of functions is used as an input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL #default value of i (inverse) to NULL
  set<-function(y){
    x<<-y #use <<- operator to assign a value to an object in an environment
    i<<-NULL
    
  }
  #define get, set, getinverse, setinverse functions
  get<-function() x #returns the matrix x
  setinverse<-function(inverse) i<<-inverse # assigns inverse of matrix x to i
  getinverse<-function() i #returns the inverse of matrix X or NULL 
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## argument x is the ourput of makeCacheMatrix()
##The function returns inverse of the original matrix which was an input to makeCacheMatrix

cacheSolve <- function(x, ...) {
  i<-x$getinverse()
  
  # if the inverse has been calculated before i would not be NULL
  if(!is.null(i)){
    message("getting cached data")
    return(i) # end function here if inverse already exists
  }
  
  data<-x$get()
  i<-solve(data,...) #use solve function to calculate the inverse
  x$setinverse(i)
  i
  ## Return i (inverse matrix) that is the inverse of 'x'
}
