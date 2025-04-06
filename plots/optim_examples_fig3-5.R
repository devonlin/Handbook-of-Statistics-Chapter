rm(list=ls())
#compare diff EzGP
nsim=50
n0=9
N=63



M_c0_method4=matrix(NA,nrow=N-n0,ncol=nsim)
time_method4=matrix(NA,nrow=N-n0,ncol=nsim)
ymin_method4=matrix(NA,nrow=N-n0,ncol=nsim)

M_c0_method5=matrix(NA,nrow=N-n0,ncol=nsim)
time_method5=matrix(NA,nrow=N-n0,ncol=nsim)
ymin_method5=matrix(NA,nrow=N-n0,ncol=nsim)

M_c0_method6=matrix(NA,nrow=N-n0,ncol=nsim)
time_method6=matrix(NA,nrow=N-n0,ncol=nsim)
ymin_method6=matrix(NA,nrow=N-n0,ncol=nsim)

M_c0_method7=matrix(NA,nrow=N-n0,ncol=nsim)
ymin_method7=matrix(NA,nrow=N-n0,ncol=nsim)

M_c0_method8=matrix(NA,nrow=N-n0,ncol=nsim)
time_method8=matrix(NA,nrow=N-n0,ncol=nsim)
ymin_method8=matrix(NA,nrow=N-n0,ncol=nsim)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#nonserial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1
method_name="EI-C"
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,".rds",sep="")
M_c0_method4=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,".rds",sep="")
ymin_method4=readRDS(filename)
filename= paste0("time-n",n0,"_","N",N,"_",method_name,".rds",sep="")
time_method4=readRDS(filename)

#2  
method_name="ARSD-C"
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,".rds",sep="")
M_c0_method5=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,".rds",sep="")
ymin_method5=readRDS(filename)
filename= paste0("time-n",n0,"_","N",N,"_",method_name,".rds",sep="")
time_method5=readRDS(filename)

#3
method_name="LCB-C"
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,".rds",sep="")
M_c0_method6=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,".rds",sep="")
ymin_method6=readRDS(filename)
filename= paste0("time-n",n0,"_","N",N,"_",method_name,".rds",sep="")
time_method6=readRDS(filename)

#4
method_name="one-shot"
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,".rds",sep="")
M_c0_method7=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,".rds",sep="")
ymin_method7=readRDS(filename)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#serial
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1
method_name="EI-C"
for(i in 1:nsim){
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
M_c0_method4[,i]=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
ymin_method4[,i]=readRDS(filename)
filename= paste0("time-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
time_method4[,i]=readRDS(filename)
}
#2  
method_name="ARSD-C"
for(i in 1:nsim){
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
M_c0_method5[,i]=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
ymin_method5[,i]=readRDS(filename)
filename= paste0("time-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
time_method5[,i]=readRDS(filename)
}
#3
method_name="LCB-C"
for(i in 1:nsim){
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
M_c0_method6[,i]=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
ymin_method6[,i]=readRDS(filename)
filename= paste0("time-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
time_method6[,i]=readRDS(filename)
}
#4
method_name="one-shot"
for(i in 1:nsim){
filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
M_c0_method7[,i]=readRDS(filename)
filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
ymin_method7[,i]=readRDS(filename)[(n0+1):N]
}

#5
method_name="MCTS-EI"
for(i in 1:nsim){
  filename= paste0("M_c0_mean-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
  M_c0_method8[,i]=readRDS(filename)
  filename= paste0("y_a-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
  ymin_method8[,i]=readRDS(filename)
  filename= paste0("time-n",n0,"_","N",N,"_",method_name,"_",i,".rds",sep="")
  time_method8[,i]=readRDS(filename)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
nmethod=5
N_seq=c(3,6,9,12)
N_seq=c(18,27,36,45)
N_seq=c(9,18,27,36)

ymin_method8=matrix(0,nrow=15,ncol=50)
for(i in 1:50){
  filename= paste0("func_hybridM_",i-1,"_Y.csv",sep="")
  Y_res[,i]=read.csv(filename,header = FALSE)[,1]
}

library(ggplot2)
library(dplyr)
library(latex2exp)
library(ggpubr)


Y_res=matrix(0,nrow=30,ncol=50)
for(i in 1:50){
  filename= paste0("func3_hybridM_",i-1,"_Y.csv",sep="")
  Y_res[,i]=read.csv(filename,header = FALSE)[,1]
}

stage=9


Methods=c("EI", "ARSD", "LCB", "Hybrid","one-shot")
method_colors <- c(
  "EI" ="#CD9600",
  "ARSD"="#F8766D",
  "LCB" = "#00C19A",
  "Hybrid" = "#00A9FF",
  "one-shot" =  "#C77CFF"
)



# Assuming the data is already calculated, like:
best_so_far_ARSD <- rowMeans(apply(ymin_method5[1:stage,], 2, cummin))
best_so_far_LCB <- rowMeans(apply(ymin_method6[1:stage,], 2, cummin))
best_so_far_EI <- rowMeans(apply(ymin_method4[1:stage,], 2, cummin))
best_so_far_oneshot <- rowMeans(apply(ymin_method7[1:stage,], 2, cummin))
best_so_far_mcts <- rowMeans(apply(Y_res[(n0+1):(stage+n0),], 2, cummin))



# Create a data frame for ggplot
data <- data.frame(
  iteration = rep((1+n0):(stage+n0), 5),
  value = c( best_so_far_EI,best_so_far_ARSD, best_so_far_LCB,best_so_far_mcts,best_so_far_oneshot),
  Method = rep(c("EI", "ARSD", "LCB", "Hybrid","one-shot"), each = stage)
)

# Set the factor levels to ensure the colors match the order of methods
data$Method <- factor(data$Method, levels = c("EI", "ARSD", "LCB","Hybrid","one-shot"))

filename<- paste("Best_min.pdf",sep="")
pdf(file=filename,width =10, # Th width of the plot in inches
    height =5)
# Create the plot
ggplot(data, aes(x = iteration, y = value, color = Method)) +
  geom_point(size=4) +
  geom_line(size=0.5) +  # Add lines to connect points if desired
  ylab(TeX("$Y(\\textbf{w})$"))+
  xlab("N")+
  #ggtitle("Best ")+
  scale_color_manual(values =method_colors)+ 
  theme(legend.position = "bottom", legend.box = "horizontal",legend.text=element_text(size=15,face="bold"),legend.title = element_text(size=15,face = "bold"),
        axis.title.y =  element_text(size = 15,face="bold"),axis.title.x = element_text(size = 15,face="bold"),axis.text.x=element_text(size = 15,face="bold") ,
        axis.text=element_text(size=15,face="bold",colour="black"),plot.title = element_text(hjust = 0.5,size=16),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(fill = guide_legend(nrow = 1, byrow = TRUE))
dev.off()
save.image("best_min.RData")




Y_table=cbind(best_so_far_oneshot,best_so_far_ARSD,best_so_far_EI,best_so_far_LCB,best_so_far_mcts)
colnames(Y_table)=c("one-shot","ARSD","EI","LCB","Hybrid")

library(xtable)
xtable(Y_table,digits = 4)

Y_table


