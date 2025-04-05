rm(list=ls()) 
#-------------------------------------------------------------------------------
#Packages
#-------------------------------------------------------------------------------
library(lhs)
library(EzGP)
f=function(l){
#-------------------------------------------------------------------------------
#initial data
#-------------------------------------------------------------------------------
method_name="one-shot"
print(method_name)
load("data/initial.RData")

#-------------------------------------------------------------------------------
#measurements and time
#-------------------------------------------------------------------------------
#M_a
RMSE=c()
  #-------------------------------------------------------------------------------
  #data
  #-------------------------------------------------------------------------------
  X=randomLHS(N,p)

  if(q==1){
    Z=rep(1:m,N)
     id_sample=sample(1:length(Z),N)
    Z=Z[id_sample]
  }else{   
    Z=apply(z_true,2,rep,N)[1:N,]
    id_sample=sample(1:nrow(Z),N)
   Z=Z[id_sample,]
  }  
  XZ=cbind(X,Z)
  y= computer_simulator(XZ)
  n0_dat=cbind(XZ,y)
  #-------------------------------------------------------------------------------
  #Measurements
  #-------------------------------------------------------------------------------    
  for(i in 1:nnew){
    print(i)
   model=EzGP_fit(n0_dat[1:(n0+i),1:(p+q)],n0_dat[1:(n0+i),(p+q+1)],p=p,q=q,m=m,tau=tau) 
    
   pred=EzGP_predict(Data_M_c0[,1:(p+q)], model, MSE_on =0)
   Yhat = pred$Y_hat
   
    RMSE[i]=sqrt(mean(((Yhat-Data_M_c0[,(p+q+1)])^2)))
  }

#-------------------------------------------------------------------------------
#save results
#-------------------------------------------------------------------------------   
filename= paste0("RMSE-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
saveRDS(RMSE,file=filename)
}
l=as.integer(commandArgs(trailingOnly = TRUE))
f(l)
