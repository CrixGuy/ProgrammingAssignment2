## The following functions enable the creation of a special 'matrix' object that 
##   can cache its inverse, reducing the need for additional  computation time 
##   which can be costly when dealing with large matrices.


## makeCacheMatrix() creates the special 'matrix' object. The new object is then 
##   accessible in its matrix form by passing the $get() function. This function 
##   also enables a matrix to be 'reset' (set_matrix() function) as specified. 
##   The function to invert the special 'matrix' is also included but is invoked 
##   elsewhere (see cacheSolve() ).

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
  	set_matrix <- function(y){
  		x  <<-  y
    	i  <<-  NULL
  	}
  
  	get         <- function() x
  	set_inverse <- function(solve) i <<- solve
  	get_inverse <- function() i

  	list(set_matrix  = set_matrix, 
       	 get		 = get,
    	 set_inverse = set_inverse,
    	 get_inverse = get_inverse)
}


## cacheSolve() calls the caching function within makeCacheMatrix to create the 
##   cached output of the special 'matrix' object that was created with the 
##   makeCacheMatrix function. It first checks to see if a cached version of the
##   output already exists. It then returns this value if this is the case.

cacheSolve <- function(x, ...) {
	i  <-  x$get_inverse()

	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$set_inverse(i)
	i
}
