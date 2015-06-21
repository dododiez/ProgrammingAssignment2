## The following is a pair of R functions that will calculate and cache the inverse of a given Matrix
##
## This will only calculate the inverse of a matrix once after the matrix data is changed.

## MakeCacheMatrix generates a collection/list of functions that can be used to cache matrix calculations
makeCacheMatrix <- function(x = matrix()) 
{
	# Mark the INVERSE as "Not calculated"
	myInverse <- NULL

	# Define the set matrix function -- Use when you need to change the matrix data
	set <- function(y) 
	{
	     # Replace the matrix with the one given in the input
		x <<- y
		
		# Mark the INVERSE of the matrix a "Not calculated"
		myInverse <<- NULL
	}

	# Define the get matrix function --- use when you just want the matrix data
	get <- function() 
	{
	     # Simply return the active matrix
		x
	}

	# Set Mean -- Use after you have calculated the mean and you want to cache it
	setInverse <- function(inverse) 
	{
	     # Cache the given inverse
		myInverse <<- inverse
	}

	# Get Inverse -- Use when you want to simply get the Inverse to test if it exists or you just want to retrieve it..
	getInverse <- function() 
	{
	     # Return the cached inverse
		myInverse
	}

	# Return the special list of useful functions to aid in our caching
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# CacheSolve will determiine if an  inverse for a matrix has already been calculated and decide to return the pre-calculated Inverse of generated from scratch
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'

     # Extract inverse from "cache matrix"
	myInverse <- x$getInverse()

	# If a valid matrix inverse was obtained, just return it.
	if (!is.null(myInverse)) 
	{
		# The inverse is already cached, just return it...
		message("getting cached data")
		return(myInverse)
	}

	# The Inverse has not been calculated, Calculate it, cache it, and return it
	data <- x$get()
	myInverse <- solve(data, ...)
	x$setInverse(myInverse)
	myInverse        
}

