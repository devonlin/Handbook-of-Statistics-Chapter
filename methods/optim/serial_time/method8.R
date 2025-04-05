rm(list=ls()) 
#-------------------------------------------------------------------------------
#Packages
#-------------------------------------------------------------------------------
library(lhs)
library(EzGP)
library(laGP)

#serial job
f=function(l){
#-------------------------------------------------------------------------------
#initial data
#-------------------------------------------------------------------------------
method_name="MCTS-EI"
print(method_name)
load("initial.RData")
#-------------------------------------------------------------------------------
#measurements and time
#-------------------------------------------------------------------------------
#M_c0
M_c0_mean=c()
#min|y-a|
y_a=c()
#time
time=c()
#-----------------------------------------------------------------------------
#MCTS
#-----------------------------------------------------------------------------
#Selection policy
#UCB
UCB_pol=function(reward, n_node, N_total){
  C_UCB=2
  UCB=reward+sqrt(C_UCB*((log(N_total+1))/n_node))
  return(UCB)
}

#reward function 
reward_fn=function(response){
  #reward=-abs(response-a)
  return(-response)
}

#choose best leaf

bestleaf=function(reward,n_node,N_total){
  leaf_selection=c()
  reward_history_append=c()
  for(i in 1:(prod(m))){
    if(n_node[i]==0){
      leaf_selection[i]=Inf
      reward_history_append[i]=(reward[i])/(n_node[i]+1)
      # print(reward_history_append[i])
      #print((n_node[i]+1))
    }else{
      reward_history_append[i]=(reward[i])/(n_node[i]+1)
      leaf_selection[i]=UCB_pol(reward_history_append[i],n_node[i],N_total)
      # print(reward_history_append[i])
      #print((n_node[i]+1))
    }
  }
  infinit_UBC=which(leaf_selection==Inf)
  if(length(infinit_UBC)>0){
    node_selected_can=infinit_UBC
    Index_sample=sample(length(node_selected_can),size=1)
    node_selected=node_selected_can[Index_sample]
  }else{
    node_selected=which.max(leaf_selection)
  }
  return(node_selected)
}


  #-------------------------------------------------------------------------------
  #n0 data
  #-------------------------------------------------------------------------------
  filename= paste0("data/tradata","_",l,".csv",sep="")
  n0_dat=as.matrix(read.csv(file = filename))[,(2:(p+q+2))]
  model = EzGP_fit(n0_dat[,1:(p+q)],n0_dat[,(p+q+1)],p=p,q=q,m=m,tau=tau)
  #-----------------------------------------------------------------------------
  #Criterion functions
  #-----------------------------------------------------------------------------
  
  EI_f=function(x, model, f_best){
    if(!is.matrix(x)) x = matrix(x, nrow=1, ncol=p+q)
    pred = EzGP_predict(x[, 1:(p+q)], model, MSE_on = 1)
    ypred = pred$Y_hat
    mse = pred$MSE
    
    for(ii in 1:length(mse)){ if(mse[ii]<0){
      mse[ii]=0
    }
    }
    
    shat = sqrt(mse)
    improvement = f_best - ypred  # Difference from the best observed value
    z = improvement / shat  # Standardized improvement
    
    EI = improvement * pnorm(z) + shat * dnorm(z)  # Expected Improvement formula
    return(-EI)  # Negative for optimization (since many optimizers minimize by default)
  }  
  
  #-----------------------------------------------------------------------------
  #MCTS
  N_total=0
  n_node=rep(0,prod(m))
  reward_history=rep(0,prod(m))
  x_full_next=matrix(NA,nrow=nnew,ncol=(p+q+1))
  
  #MCTS
  for(i in 1:nnew){
    print(i)
    
    #qual
    leaf_selected=bestleaf(reward=reward_history,n_node=n_node,N_total=N_total)
    qual_input=z_true[leaf_selected,]
    #quant
    #-----------------------------------------------------------------------------
    if(q==1){
      xpred= randomLHS(npred,p)
      zpred=rep(qual_input,each=npred)
    }else{   
      xpred=randomLHS(npred,p)
      zpred=apply(matrix(qual_input,ncol=q,nrow=1),2,rep,npred)
    }   
    XZ_pred= cbind(xpred, zpred)
    
    #------------------------------------------------------
    #Criterion selection
    #------------------------------------------------------    
    f_best=min(n0_dat[,1+p+q])
    start_time <- Sys.time()  
    EI = EI_f(XZ_pred[,1:(p+q)],model,f_best)
    XZ_new = matrix(XZ_pred[which.min(EI),],nrow=1)
    end_time <- Sys.time()
    time[i]=as.numeric(end_time - start_time,unit = "secs")
    #------------------------------------------------------
    ##add next point  
    #------------------------------------------------------        
    Y_new=computer_simulator(XZ_new)
    new_input=cbind(XZ_new,Y_new)
    
    x_full_next[i,]=new_input
    reward_history[leaf_selected]=reward_history[leaf_selected]+reward_fn(x_full_next[i,(p+q+1)])
    N_total=N_total+1
    n_node[leaf_selected]=n_node[leaf_selected]+1
    
    
    #------------------------------------------------------
    ##update
    #------------------------------------------------------
    n0_dat=rbind(n0_dat,new_input)
    #print(n0_dat)
    model = EzGP_fit(n0_dat[,1:(p+q)],n0_dat[,(p+q+1)],p=p,q=q,m=m,tau = tau) 
    #-------------------------------------------------------------------------------
    #Measurements
    #-------------------------------------------------------------------------------    
    #min|y-a|    
    y_a[i]=n0_dat[nrow(n0_dat),(p+q+1)]
    
    #predictions  
    pred= EzGP_predict(C_t_XZ, model, MSE_on = 0)
    Yhat = pred$Y_hat
    #M_c0
    M_c0_mean[i]=(1/nrow(M_c0_M))*(sum(abs(C_t_Y-Yhat)))
    
  }
  
  #-------------------------------------------------------------------------------
  #save results
  #-------------------------------------------------------------------------------   
  filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(M_c0_mean,file=filename)
  filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(y_a,file=filename)
  filename= paste0("time-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(time,file=filename)
  filename= paste0("n0_dat-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  saveRDS(n0_dat,file=filename)
}

l=as.integer(commandArgs(trailingOnly = TRUE))
f(l)

