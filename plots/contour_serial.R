

rm(list=ls())
nsim=30
n0=18
N_total=81
a=1.15
nmethod=7
P=N_total-n0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method_name="RCC-EI"
method_1=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
 #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_1[,i]=output
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#2
method_name="RCC"
method_2=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
#filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_2[,i]=output
}


#3
method_name="ECL"
method_3=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_3[,i]=output
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#4
method_name="EI"
method_4=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
 # filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_4[,i]=output
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#5
method_name="ARSD-LCB"
method_5=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_5[,i]=output
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

method_name="LCB"
method_6=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
 #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_6[,i]=output
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

method_name="one-shot"
method_7=matrix(NA,nrow=P,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_7[,i]=output
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#M_c0
#Ex2 Ex3
#N=c(18,27,36,45)
#real data
N=c(105,110,115,120)

methods=rep(c("RCC-EI","RCC","ECL","EI","ARSD-LCB","LCB","one-shot"), each=length(N))
kkk=1:30
nsim=30
P22=length(N)
P2=N
#P23=c(2,3,4,5)
M_c0_mean_mat=array(NA,dim=c(P22,nsim,nmethod))
M_c0_mean_mat[,,1]=method_1[P2,kkk]#RCC-EI-dis
M_c0_mean_mat[,,2]=method_2[P2,kkk]#RCC
M_c0_mean_mat[,,3]=method_3[P2,kkk]#ECL
M_c0_mean_mat[,,4]=method_4[P2,kkk]#EI
M_c0_mean_mat[,,5]=method_5[P2,kkk]#ARSD-LCB
M_c0_mean_mat[,,6]=method_6[P2,kkk]#LCB
M_c0_mean_mat[,,7]=method_7[P2,kkk]#oneshot

M_c0_mean_center=NULL
for(i in 1:nmethod){
  for(j in 1:length(P2)){
    M_c0_mean_center=  c(M_c0_mean_center, mean(M_c0_mean_mat[j,,i]))
  }
}

df_M_c0 = data.frame(samplesize = rep(N,nmethod), M_c0= M_c0_mean_center, Methods=methods)


filename= paste0("M_c0_final","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#y-a
methods=c("RCC-EI","RCC","ECL","EI","ARSD-LCB","LCB","one-shot")
P=45
kkk=1:50
#y_a
y_a_mean_mat=array(NA,dim=c(P,nsim,nmethod))
y_a_mean_mat[,,1]=method_5[1:P,kkk]#ARSD-LCB
y_a_mean_mat[,,2]=method_2[1:P,kkk]#RCC
y_a_mean_mat[,,3]=method_1[1:P,kkk]#RCC-EI
y_a_mean_mat[,,4]=method_3[1:P,kkk]#ECL
y_a_mean_mat[,,5]=method_4[1:P,kkk]#EI
y_a_mean_mat[,,6]=method_6[1:P,kkk]#LCB
y_a_mean_mat[,,7]=method_7[1:P,kkk]#oneshot

methods_box=rep(c("ARSD-LCB","RCC","RCC-EI","ECL","EI","LCB","one-shot"), each=nsim)

label=c("V1","V2","V3","V4","V5","V6","V7")

y_a_boxplot=NULL
for(i in 1:(nmethod)){
  for(j in 1:nsim){
    y_a_boxplot=  c(y_a_boxplot, min(abs(y_a_mean_mat[,j,i]-a)))
  }
}

df_y_a_box = data.frame(labels=rep(label,each=nsim), y_a= y_a_boxplot, Methods=methods_box)


library(ggplot2)
library(dplyr)
library(latex2exp)
library(ggpubr)
library("gridExtra")

filename=paste("y-a","_","n0",n0,"_",nsim,".pdf",sep="")
pdf(file=filename,width =12, # The width of the plot in inches
    height =10)
ggplot(df_y_a_box,aes(x=labels,y=log(y_a)))+
  geom_boxplot(aes(fill=Methods),width=0.8)+
  ylab(TeX("$\\log(min(|Y(\\textbf{w}_0)-a|))$"))+
  ggtitle("(a) a=-20;N=14")+
  #scale_x_discrete(labels=c("1","2","3","4","5","6"))+
  theme(legend.position = "bottom", legend.box = "horizontal",legend.text=element_text(size=15,face="bold"),legend.title = element_text(size=15,face = "bold"),
        axis.title.y =  element_text(size = 15,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),
        axis.text=element_text(size=15,face="bold",colour="black"),plot.title = element_text(hjust = 0.5,size=16),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(fill = guide_legend(nrow = 1, byrow = TRUE))
dev.off()



filename= paste0("y-a","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#time
PPP=120
methods=c("RCC","ECL","EI","ARSD-LCB","LCB")
time_secs=round(colMeans(cbind(colSums(method_2[1:PPP,]),colSums(method_3[1:PPP,]),
                               colSums(method_4[1:PPP,]), colSums(method_5[1:PPP,]),colSums(method_6[1:PPP,]))),digits = 2)
names(time_secs)=methods
time_secs
filename= paste0("time","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#count criterion
nsim=50
nm=9

method_name="RCC"
method_22=matrix(NA,nrow=N_total-n0,ncol=nsim)
for(i in 1:nsim){
  filename= paste0("count_RCC-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),"_",i,".rds",sep="")
  output=readRDS(filename)
  method_22[,i]=output
}

cc_1=matrix(NA,nrow=(N_total-n0),ncol=nsim)  
cc_2=matrix(NA,nrow=(N_total-n0),ncol=nsim)  
for (i in 1:nsim){
  for(j in 1:nrow(method_22)){
    if((method_22[j,i]==1)){cc_1[j,i]=1}else{
      cc_1[j,i]=0
    }
    if((method_22[j,i]==2)){cc_2[j,i]=1}else{
      cc_2[j,i]=0
    }
  }
}

s1=c()
s2=c()
s3=c()
s4=c()
for(i in 1:nsim){
  s1[i]=(sum(cc_1[1:((N_total-n0)-nm*4),i]))
  s2[i]=(sum(cc_1[1:((N_total-n0)-nm*3),i]))
  s3[i]=(sum(cc_1[1:((N_total-n0)-nm*2),i]))
  s4[i]=(sum(cc_1[1:((N_total-n0)-nm*1),i]))
}
A_min_1=round(mean(s1/((N_total-n0)-nm*4)),digits=3)
A_min_2=round(mean(s2/((N_total-n0)-nm*3)),digits=3)
A_min_3=round(mean(s3/((N_total-n0)-nm*2)),digits=3)
A_min_4=round(mean(s4/((N_total-n0)-nm*1)),digits=3)
A_min_1;A_min_2;A_min_3;A_min_4



filename= paste0("cout-criterion","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)




h1=c()
h2=c()
h3=c()
h4=c()
for(i in 1:nsim){
  h1[i]=(sum(cc_2[1:((N_total-n0)-nm*3),i]))
  h2[i]=(sum(cc_2[1:((N_total-n0)-nm*2),i]))
  h3[i]=(sum(cc_2[1:((N_total-n0)-nm),i]))
  h4[i]=(sum(cc_2[1:(N_total-n0),i]))
}

A_2_1=round(mean(h1/((N_total-n0)-nm*3)),digits = 2)
A_2_2=round(mean(h2/((N_total-n0)-nm*2)),digits=2)
A_2_3=round(mean(h3/((N_total-n0)-nm)),digits=2)
A_2_4=round(mean(h4/((N_total-n0))),digits=2)


A_2_1;A_2_2;A_2_3;A_2_4




