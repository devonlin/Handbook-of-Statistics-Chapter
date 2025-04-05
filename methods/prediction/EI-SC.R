rm(list=ls()) 
#-------------------------------------------------------------------------------
#Packages
#-------------------------------------------------------------------------------
library(lhs)
library(EzGP)

#serial job
f=function(l){
  #-------------------------------------------------------------------------------
  #initial data
  #-------------------------------------------------------------------------------
  method_name="EI_SC"
  print(method_name)
  load("data/initial.RData")
  #-------------------------------------------------------------------------------
  #measurements and time
  #-------------------------------------------------------------------------------
  #M_a
  RMSE=c()
  time_update=c()
  time_cri=c()
  time_whole=c()
  #-------------------------------------------------------------------------------
  #n0 data
  #-------------------------------------------------------------------------------
  filename= paste0("data/tradata","_",l,".csv",sep="")
  n0_dat=as.matrix(read.csv(file = filename))[,(2:(p+q+2))]
  model =  EzGP_fit(n0_dat[,1:(p+q)],n0_dat[,(p+q+1)],p=p,q=q,m=m,tau=tau) 
  #-------------------------------------------------------------------------------
  #Criterion functions
  #-------------------------------------------------------------------------------
  # Function to calculate equi-spaced contour levels
  a_max_f=function(x,model){
    if(is.matrix(x)!=TRUE) x = matrix(x,nrow=1,ncol=p+q)
    pred= EzGP_predict(x[,1:(p+q)], model, MSE_on =1)
    mse = pred$MSE
    for(ii in 1:length(mse)){ if(mse[ii]<0){
      mse[ii]=0
    }
    }
    a_max=pred$Y_hat[which.max(sqrt(mse))]
    return(list(a_max=a_max,SD_id=which.max(sqrt(mse))))
  }
  
  
  EI_SC_f=function(x,model,a){
    if(is.matrix(x)!=TRUE) x = matrix(x,nrow=1,ncol=p+q)
    pred= EzGP_predict(x[,1:(p+q)], model, MSE_on =1)
    ypred= pred$Y_hat#save
    mse = pred$MSE
    for(ii in 1:length(mse)){ if(mse[ii]<0){
      mse[ii]=0
    }
    }
    sd_pred=sqrt(mse)
    ep = 2* sd_pred
    u1 = (a-ypred-ep)/sd_pred
    u2 = (a-ypred+ep)/sd_pred
    EI_max = which.max(((ep^2-(ypred-a)^2-sd_pred^2)*(pnorm(u2)-pnorm(u1))+(sd_pred^2)*(u2*dnorm(u2)-u1*dnorm(u1))+2*(ypred-a)*sd_pred*(dnorm(u2)-dnorm(u1))))
    return(list(id_EI=EI_max,ypred_max=ypred[EI_max]))
  }
  #-----------------------------------------------------------------------------
  #Adaptive design
  #-----------------------------------------------------------------------------
  for(i in 1:(nnew)){
    print(i)
    
    #------------------------------------------------------
    #Search space
    #------------------------------------------------------
    if(q==1){
      X_pred= randomLHS(npred*prod(m),p)
      Z_pred=rep(1:m,each=npred)
    }else{   
      X_pred= randomLHS(npred*prod(m),p)
      Z_pred=  apply(z_true,2,rep,npred)
    }  
    XZ_pred=cbind(X_pred,Z_pred)
    
    a_set=a_max_f(XZ_pred,model)$a_max
    #------------------------------------------------------
    #Criterion selection
    #------------------------------------------------------    
    start_time_whole <- Sys.time()
    start_time_cri <- Sys.time()
    EI_SC = EI_SC_f(XZ_pred,model,a_set)
    end_time_cri <- Sys.time()
    
    
    new_input=matrix(c(XZ_pred[EI_SC$id_EI,],computer_simulator( matrix(XZ_pred[EI_SC$id_EI,],nrow=1))),nrow=1)
    n0_dat=rbind(n0_dat,new_input)
    start_time_update <- Sys.time()
    model =  EzGP_fit(n0_dat[,1:(p+q)],n0_dat[,(p+q+1)],p=p,q=q,m=m,tau=tau) 
    end_time_update <- Sys.time()
    end_time_whole <- Sys.time()
    
    time_cri[i]=as.numeric(end_time_cri - start_time_cri,unit = "secs")
    time_update[i]=as.numeric(end_time_update - start_time_update,unit = "secs")
    time_whole[i]=as.numeric(end_time_whole - start_time_whole,unit = "secs")
    #predictions  
    pred=EzGP_predict(Data_M_c0[,1:(p+q)], model, MSE_on =0)
    Yhat = pred$Y_hat
    RMSE[i]=sqrt(mean((Yhat-Data_M_c0[,(p+q+1)])^2))
  }
  
  
  #-------------------------------------------------------------------------------
  #save results
  #-------------------------------------------------------------------------------   
  filename= paste0("RMSE-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(RMSE,file=filename)
  filename= paste0("time_whole-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(time_whole,file=filename)
  filename= paste0("time_update-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(time_update,file=filename)
  filename= paste0("time_cri-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(time_cri,file=filename)
}
l=as.integer(commandArgs(trailingOnly = TRUE))
f(l)



