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
method_name="EI_MC"
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
  calculate_contours <- function(y_min, y_max, k) {
    if (k == 1) {
      contours <- c(y_min, y_max)
    } else {
      delta_y <- (y_max - y_min) / (k - 1)
      contours <- seq(y_min, y_max, by = delta_y)
    }
    return(contours)
  }
  EI_MC_f=function(x,model,a_set){
    if(is.matrix(x)!=TRUE) x = matrix(x,nrow=1,ncol=p+q)
    pred= EzGP_predict(x[,1:(p+q)], model, MSE_on =1)
    ypred= pred$Y_hat#save
    mse = pred$MSE
    for(ii in 1:length(mse)){ if(mse[ii]<0){
      mse[ii]=0
    }
    }
    sd_pred=sqrt(mse)
    ep = 2*sd_pred
    EI_MC_total=rep(0,npred)
    for(j in 1:nrow(x)){
      EI_MC=c()
      for(i in 1:length(a_set)){
        u1 = (a_set[i]-ypred[j]-ep[j])/sd_pred[j]
        u2 = (a_set[i]-ypred[j]+ep[j])/sd_pred[j]
        EI_max =(ep[j]^2-(ypred[j]-a_set[i])^2-sd_pred[j]^2)*(pnorm(u2)-pnorm(u1))+(sd_pred[j]^2)*(u2*dnorm(u2)-u1*dnorm(u1))+ 
          2*(ypred[j]-a_set[i])*sd_pred[j]*(dnorm(u2)-dnorm(u1))
        EI_MC=c(EI_MC,EI_max)
      }
      EI_MC_total[j]=sum(EI_MC)
    }
    est=list(id_EI=which.max(EI_MC_total),EI_val=EI_MC_total[which.max(EI_MC_total)],ypred_max=ypred[which.max(EI_MC_total)],mse_max=mse[which.max(EI_MC_total)])
    return(est)
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
    
    pred= EzGP_predict(Data_M_c0[,1:(p+q)], model, MSE_on =0)
    a_set=round(calculate_contours(min(pred$Y_hat),max(pred$Y_hat),k),digits=2)
    #print(a_set)
    #------------------------------------------------------
    #Criterion selection
    #------------------------------------------------------    
    start_time_whole <- Sys.time()
    start_time_cri <- Sys.time()
    EI_MC = EI_MC_f(XZ_pred,model,a_set)
    end_time_cri <- Sys.time()
    

    new_input=matrix(c(XZ_pred[EI_MC$id_EI,],computer_simulator( matrix(XZ_pred[EI_MC$id_EI,],nrow=1))),nrow=1)
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



