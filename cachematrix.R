## The following two functions are to help mitigate
## time-consuming computations by creating a matrix
## that has the potential to cache data

#This function creates a special "matrix" object 
#that can cache its inverse (source: R Programming Course)
#define the makeCacheMatrix function
makeCacheMatrix <- function(m = matrix()){

#intialize inverse variable
inv <- NULL

#set the matrix
set <- function(matrix) {
	x <<- matrix
	inv <<- NULL
	}

#get the matrix
get <- function(matrix) {
	x
	}

#set the inverse of the matrix
setinverse <- function(inverse) {
	inv <<- inverse
	}
	
#get the inverse of the matrix
getinverse<-function() {
	inv
	}
	
#return methods list	 
list (set = set
	 ,get = get
	 ,setinverse = setinverse
	 ,getinverse = getinverse)
	 }

#This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed), 
#then the cacheSolve function should retrieve the inverse 
#from the  (source: R Programming Course)
#define the cacheSolve function
cacheSolve <- function (x, ...) {

	#get the inverse of matrix from makeCacheMatrix function
	inv <- x$getinverse()
	
	#if cache set, return set inverse
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	#if cache not set, get the inverse (compute)
	#get the matrix
	data <- x$get()
	
	#compute the inverse 
	inv <- inverse(data, ...)
	
	#set the matrix with the inverse
	x$setinverse(inv)
	
	#return the inversed matrix
	inv
}