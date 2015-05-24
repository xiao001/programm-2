makeCacheMatrix <- function(x = matrix()) 
{
inv=NULL
set=function(y)
{
x<<-y
inv<<-NULL
}
get=function(){return(x)}
setInverse=function(i){inv<<-i}
getInverse=function(){return(inv)}
output=list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
return(output)
}
cacheSolve <- function(x, ...) 
{
inv=x$getInverse()
if(!is.null(inv))
{
message("getting cached data\n")
return(inv)
}
data=x$get()
inv=solve(data,...)
x$setInverse(inv)
return(inv)

}
