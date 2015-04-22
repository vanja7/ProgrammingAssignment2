## makeCasheMatrix transforms a matrix in such a form that its invers is cached,
## once it's calculated applying the cacheSolve function. The invers is cached until another
## matrix value is introduced (by makeCacheMatrix or makeCacheMatrix$set).  

## makeCacheMatrix function makes a vector of four functions: setinv function sets a value
## of the invers matrix (a new one); getinv function gives the cached value of the invers;
## get function gives the new matrix, set function sets a new value of the source matrix

makeCacheMatrix <- function(x = matrix()) {
              inv<-NULL 
              getinv<-function()inv       ## cached invers will be nullified
                                          ## by every calling of this function
              get<-function() x
              setinv<-function(invers)inv<<-invers ## sets the new invers value
                                                   ## in the global enviroment
              set<-function(y){x<<-y ## introduces the new matrix value
                               ##in the global enviroment
                                inv<<-NULL} ## cached invers will be nullified
                                            ## by every calling of this function
                                            ## thus notifying that the new matrix value
                                            ## was set
              list(setinv=setinv,getinv=getinv,get=get,set=set)}                                    


## cacheSolve function calculates the invers of a matrix already transformed
## by the makeCaheMatrix; if the invers is already calculated,
## it just returns the cached value 

cacheSolve <- function(x,...){if(!is.null(x$getinv())){message("getting cached value")
                              return(x$getinv())}
                              ## if the invers was already calculated,
                              ## the function informs us about that,
                              ## then returns the cached invers and stops here
                              
                matrix<-x$get() ## else, the new matrix value is introduced
                inv<-solve(matrix,...) ## the new invers value is calculated
                x$setinv(inv) ## the new invers value is set in the global envoroment                         
                                                       inv}

