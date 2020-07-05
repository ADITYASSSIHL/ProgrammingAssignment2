## We aim to create two functions to obatain inverse of a given matrix (yeah only an inversable matrix)
##We create two function called makecacheMatirix which wil essentially get you the matrix and also provide the inverse of the matrix


## makeCachematrix is written with both single arrow assignment operators <- and double arrow assignment operators <<-


makeCacheMatrix <- function(x = matrix()) {
inv<-NULL 
set <-function(y){
    x<<-y
    inv<<-NULL
}
get<-function() {x}
setInverse <- function(inverse) {inv <<- inverse}
getInverse <- function() {inv}
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## We write the cachesolve function which uses the standard solve (inbuild in R to provide the inverse)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
}

##now we test the functions we have written
mymatrix<-makeCacheMatrix(matrix(1:4, nrow=2,ncol=2))
#make a 2X2 inversablwe matrix we call this by 
mymatrix$get()

##we use the cacheSolve function as passs the mymatrix object and call for inverse
cacheSolve(mymatrix)

##call for the getinverse function now
mymatrix$getInverse()


#We check if the function is working by using the inbuld solve function


a<-matrix(runif(4), nrow=2,ncol=2) # a 2x2 matrix  
a_inv<-solve(a) #solve function to obtain inverse

my_a<-makeCacheMatrix(a) #using makecachematrix function
cacheSolve(my_a)         #using cachesolve function
my_a_inv<-my_a$getInverse()   #obtaining inverse from getinverse 


a_inv==my_a_inv  #checking solve function is same as getinverse function which we wrote


