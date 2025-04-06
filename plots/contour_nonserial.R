
rm(list=ls())
nsim=50
n0=9
N=N_total=24
a=0.5
nmethod=7
P=N-n0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method_name="RCC-EI"
method_1=matrix(NA,nrow=P,ncol=nsim)
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_1=readRDS(filename)[,1:nsim]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2
method_name="RCC"
method_2=matrix(NA,nrow=P,ncol=nsim)
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3),".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_2=readRDS(filename)[,1:nsim]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#3
method_name="ECL"
method_3=matrix(NA,nrow=P,ncol=nsim)
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
 #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
 #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_3=readRDS(filename)[,1:nsim]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4
method_name="EI"
method_4=matrix(NA,nrow=P,ncol=nsim)
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_4=readRDS(filename)[,1:nsim]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#5
method_name="ARSD-LCB"
method_5=matrix(NA,nrow=P,ncol=nsim)
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_5=readRDS(filename)[,1:nsim]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

method_name="LCB"
method_6=matrix(NA,nrow=P,ncol=nsim)
  filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  #filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_6=readRDS(filename)[,1:nsim]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

method_name="one-shot"
method_7=matrix(NA,nrow=P,ncol=nsim)
filename= paste0("M_c0_mean-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
#filename= paste0("y_a-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
#filename= paste0("time-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
method_7=readRDS(filename)[,1:nsim]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#M_c0
#Ex1
#N_seq=c(3,6,9,12)
#N_seq=c(2,4,6,8,10)

methods=rep(c("RCC-EI","RCC","ECL","EI","ARSD-LCB","LCB","one-shot"), each=length(N_seq))

kkk=1:50
nsim=50
P22=length(N_seq)
P2=N_seq
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

df_M_c0 = data.frame(samplesize = rep(N_seq,nmethod), M_c0= M_c0_mean_center, Methods=methods)


filename= paste0("M_c0_final","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#y-a
methods=c("RCC-EI","RCC","ECL","EI","ARSD-LCB","LCB","one-shot")
P=10
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
  ggtitle("(a) a=0.75")+
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
nsim=50
ppp=12
methods=c("RCC-EI","RCC","ECL","EI","ARSD-LCB","LCB")
time_secs=round(colMeans(cbind(colSums(method_1[1:ppp,1:nsim]),colSums(method_2[1:ppp,1:nsim]),colSums(method_3[1:ppp,1:nsim]),
                               colSums(method_4[1:ppp,1:nsim]),colSums(method_5[1:ppp,1:nsim]),colSums(method_6[1:ppp,1:nsim]))),digits = 2)
names(time_secs)=methods
time_secs
filename= paste0("time","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#count criterion
nm=3
nsim=50

method_name="RCC"

method_322=matrix(NA,nrow=N_total-n0,ncol=nsim)
  filename= paste0("count_RCC-n",n0,"_","N",N_total,"_",method_name,"_",round(a,digits = 3) ,".rds",sep="")
  method_322=readRDS(filename)
  nsim=50
cc_1=matrix(NA,nrow=(N_total-n0),ncol=nsim)  
cc_2=matrix(NA,nrow=(N_total-n0),ncol=nsim)  
for (i in 1:nsim){
  for(j in 1:nrow(method_322)){
    if((method_322[j,i]==1)){cc_1[j,i]=1}else{
      cc_1[j,i]=0
    }
    if((method_322[j,i]==2)){cc_2[j,i]=1}else{
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
  #s4[i]=(sum(cc_1[1:((N_total-n0)),i]))
  s4[i]=(sum(cc_1[1:((N_total-n0)-nm*1),i]))
}
A_min_1=round(mean(s1/((N_total-n0)-nm*4)),digits=3)
A_min_2=round(mean(s2/((N_total-n0)-nm*3)),digits=3)
A_min_3=round(mean(s3/((N_total-n0)-nm*2)),digits=3)
A_min_4=round(mean(s4/((N_total-n0)-nm*1)),digits=3)
#A_min_4=round(mean(s4/((N_total-n0))),digits=3)
A_min_1;A_min_2;A_min_3;A_min_4



filename= paste0("cout-criterion","_","n",n0,"_","N",N_total,"_",round(a,digits = 3),".Rdata",sep="")
save.image(filename)




h1=c()
h2=c()
h3=c()
h4=c()
for(i in 1:nsim){
  h1[i]=(sum(cc_2[1:((N-n0)-nm*3),i]))
  h2[i]=(sum(cc_2[1:((N-n0)-nm*2),i]))
  h3[i]=(sum(cc_2[1:((N-n0)-nm),i]))
  h4[i]=(sum(cc_2[1:(N-n0),i]))
}

A_2_1=round(mean(h1/((N-n0)-nm*3)),digits = 2)
A_2_2=round(mean(h2/((N-n0)-nm*2)),digits=2)
A_2_3=round(mean(h3/((N-n0)-nm)),digits=2)
A_2_4=round(mean(h4/((N-n0))),digits=2)


A_2_1;A_2_2;A_2_3;A_2_4



method_1=method_1[,1:50]
method_2=method_2[,1:50]
method_3=method_3[,1:50]
method_4=method_4[,1:50]
method_5=method_5[,1:50]
method_6=method_6[,1:50]
method_7=method_7[,1:50]



