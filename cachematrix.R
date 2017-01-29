makeCacheMatrix<-function(x=matrix())
{
        INV<-NULL
        getmatrix<-function(){
                x
        }
        setinverse<-function(y){
                INV<<-y
        }
        getinverse<-function()INV
        list(getM=getmatrix,setI=setinverse, getI=getinverse)
}       
cacheSolve<-function(x){
        Invr<-x$getI()
        if(!is.null(Invr)){
                print("getting the inverse")
                return(Invr)
        }
        Mat<-x$getM()
        if(det(Mat)==0)
        {
                print("The matrix is not Invertible")
                return()
        }
        else
        {
                inverse<-solve(Mat)
                x$setI(inverse)
                inverse        
        }
        
}