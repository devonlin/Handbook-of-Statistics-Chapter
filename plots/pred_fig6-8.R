rm(list=ls())

nsim=30
nmethod=3
nsim=30
n0=35
N=100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1
RMSE_1=matrix(0,nrow=N-n0,ncol=nsim)
whole_1=matrix(0,nrow=N-n0,ncol=nsim)
update_1=matrix(0,nrow=N-n0,ncol=nsim)
cri_1=matrix(0,nrow=N-n0,ncol=nsim)

method_name="EI_MC"
for(l in 1:nsim){
  filename= paste0("RMSE-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  RMSE_1[,l]=readRDS(filename)
  filename= paste0("time_whole-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  whole_1[,l]=readRDS(filename)
  filename= paste0("time_update-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  update_1[,l]=readRDS(filename)
  filename= paste0("time_cri-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  cri_1[,l]=readRDS(filename)
}

#3
RMSE_3=matrix(0,nrow=N-n0,ncol=nsim)
whole_3=matrix(0,nrow=N-n0,ncol=nsim)
update_3=matrix(0,nrow=N-n0,ncol=nsim)
cri_3=matrix(0,nrow=N-n0,ncol=nsim)
count_RCC_3=matrix(0,nrow=N-n0,ncol=nsim)

method_name="EI_SC"
for(l in 1:nsim){
  filename= paste0("RMSE-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  RMSE_3[,l]=readRDS(filename)
  filename= paste0("time_whole-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  whole_3[,l]=readRDS(filename)
  filename= paste0("time_update-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  update_3[,l]=readRDS(filename)
  filename= paste0("time_cri-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  cri_3[,l]=readRDS(filename)
}



#2  
RMSE_2=matrix(0,nrow=N-n0,ncol=nsim)
method_name="one-shot"
for(l in 1:nsim){
  filename= paste0("RMSE-n",n0,"_","N",N,"_",method_name,"_",l,".rds",sep="")
  RMSE_2[,l]=readRDS(filename)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#RMSE
#Ex1
P2=c(10,20)
P3=c("20","30")
#Ex2
P2=c(40,60)
P3=c("30","40")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

RMSE_mat=array(NA,dim=c(length(P2),nsim,nmethod))
RMSE_mat[,,1]=RMSE_1[P2,1:nsim]
RMSE_mat[,,2]=RMSE_2[P2,1:nsim]
RMSE_mat[,,3]=RMSE_3[P2,1:nsim]

Methods=rep(c("EI-MC","one-shot","EI-SC"),each=length(P2)*nsim)
method_colors <- c(
  "EI-MC" =  "#7CAE04",
  "EI-SC"= "#F8766D",
   # "S-ALX" = "#00A9FF",
   # "S-AOC" = "#C77CFF",
   # "D-ALX" =  "#F8766D",
   # "D-AOC" = "#CD9600",
  "one-shot" = "#00A9FF"
)


stages=rep(rep(P3,each=nsim),nmethod)

#boxplot
RMSE_boxplot=NULL

for(i in 1:nmethod){
  for(j in 1:length(P2)){
    RMSE_boxplot=  c(RMSE_boxplot, (RMSE_mat[j,,i]))
  }
}
df_RMSE_boxplot = data.frame(Methods=Methods, RMSE= RMSE_boxplot, stages=stages)
df_RMSE_boxplot$stages <- factor(df_RMSE_boxplot$stages, levels = P3)


library(ggplot2)
library(dplyr)
library(latex2exp)
library(ggpubr)
library("gridExtra")


# filename<- paste("RMSE.pdf",sep="")
#  pdf(file=filename,width =10, height =5)
 RMSE_plot=ggplot(df_RMSE_boxplot, aes(x = Methods, y = log(RMSE), color = Methods)) +
  geom_boxplot(size = 1) +
  facet_wrap(~stages,  scales = "free_x",nrow=1, labeller = labeller(stages = function(x) paste0("n=", x))) +  # Add "n=" to stage labels
  #xlab("Methods") +  # Add x-axis label
  ylab("log(RMSE)") +
  ggtitle("log(RMSE)") +
  scale_color_manual(values = method_colors, name = "Methods") +
  # Add a vertical line between stages
 # geom_vline(
  #  xintercept = Inf,  # Adds a vertical boundary at the end of each facet
   # color = "lightgray", linetype = "solid", size = 0.8) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_blank(),
    #axis.text.x=element_text(size = 10, face = "bold", colour = "black",angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    axis.text.y = element_text(size = 15, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    strip.text = element_text(size = 15, face = "bold"),  # Style facet labels
    #strip.background = element_blank(), 
    panel.spacing = unit(2, "lines")                     # Add spacing between facets
  ) +
  guides(color = guide_legend(nrow = 1))
#dev.off()
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``
Methods2=rep(c("EI-MC","EI-SC"),each=length(P2))
stages2=rep(c(P3),nmethod-1)


time_whole_mat=array(NA,dim=c(N-n0,nsim,nmethod-1))
time_whole_mat[,,1]=(whole_1[,1:nsim])
time_whole_mat[,,2]=(whole_3[,1:nsim])


#boxplot
time_whole=NULL

for(i in 1:(nmethod-1)){
  for(j in 1:length(P2)){
    time_whole=  c(time_whole, mean(colSums(time_whole_mat[1:P2[j],,i])))
  }
}
df_time_whole = data.frame(Methods=Methods2, Time= time_whole/60, stages=stages2)
df_time_whole$stages <- factor(df_time_whole$stages, levels = P3)

# 
Time_plot_whole=ggplot(df_time_whole, aes(x = Methods, y = Time, group = stages)) +
  geom_point(aes(color = Methods),size = 6) +  # Points for each method
  geom_line( linetype = "solid") +
  facet_wrap(~ stages, scales = "free_x",nrow=1,labeller = labeller(stages = function(x) paste0("n=", x))) +  # Move facet labels to the bottom
  # xlab("N") +
  ylab("Time (mins)") +
  ggtitle("(b) Total Time (mins)") +
  scale_color_manual(values = method_colors, name = "Methods") +
  #geom_vline(
   # xintercept = Inf,  # Adds a vertical boundary at the end of each facet
    #color = "lightgray", linetype = "solid", size = 0.8) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.size = unit(1.5, "lines"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_blank(),
    #axis.text.x=element_text(size = 10, face = "bold", colour = "black",angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    axis.text.y = element_text(size = 15, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    #strip.background = element_blank(),  # Optional: Remove background of facet labels
    strip.text = element_text(size = 15, face = "bold")  # Optional: Adjust text size and style
  ) +
  guides(color = guide_legend(nrow = 1))
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
time_update_mat=array(NA,dim=c(N-n0,nsim,nmethod-1))
time_update_mat[,,1]=(update_1[,1:nsim])
time_update_mat[,,2]=(update_3[,1:nsim])
# time_update_mat[,,3]=(update_4[,1:nsim])
# time_update_mat[,,4]=(update_5[,1:nsim])
# time_update_mat[,,5]=(update_6[,1:nsim])
# time_update_mat[,,6]=(update_7[,1:nsim])
# 
# 
#boxplot
time_update=NULL

for(i in 1:(nmethod-1)){
  for(j in 1:length(P2)){
    time_update=  c(time_update, mean(colSums(time_update_mat[1:P2[j],,i])))
  }
}
df_time_update = data.frame(Methods=Methods2, Time= time_update/60, stages=stages2)
df_time_update$stages <- factor(df_time_update$stages, levels = P3)


Time_plot_update=ggplot(df_time_update, aes(x = Methods, y = Time, group = stages)) +
  geom_point(aes(color = Methods),size = 6) +  # Points for each method
  geom_line( linetype = "solid") +
  facet_wrap(~ stages, scales = "free_x",nrow=1,labeller = labeller(stages = function(x) paste0("n=", x))) +  # Move facet labels to the bottom
  # xlab("N") +
  ylab("Time (mins)") +
  ggtitle("(c) Update Time (mins)") +
  scale_color_manual(values = method_colors, name = "Methods") +
 # geom_vline(
  #  xintercept = Inf,  # Adds a vertical boundary at the end of each facet
   # color = "lightgray", linetype = "solid", size = 0.8) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.size = unit(1.5, "lines"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_blank(),
    #axis.text.x=element_text(size = 10, face = "bold", colour = "black",angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    axis.text.y = element_text(size = 15, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
   # strip.background = element_blank(),  # Optional: Remove background of facet labels
    strip.text = element_text(size = 15, face = "bold")  # Optional: Adjust text size and style
  ) +
  guides(color = guide_legend(nrow = 1))

# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
time_cri_mat=array(NA,dim=c(N-n0,nsim,nmethod-1))
time_cri_mat[,,1]=(cri_1[,1:nsim])
time_cri_mat[,,2]=(cri_3[,1:nsim])
# time_cri_mat[,,3]=(cri_4[,1:nsim])
# time_cri_mat[,,4]=(cri_5[,1:nsim])
# time_cri_mat[,,5]=(cri_6[,1:nsim])
# time_cri_mat[,,6]=(cri_7[,1:nsim])
# 
# 
# #boxplot
time_cri=NULL

for(i in 1:(nmethod-1)){
  for(j in 1:length(P2)){
    time_cri=  c(time_cri, mean(colSums(time_cri_mat[1:P2[j],,i])))
  }
}
df_time_cri = data.frame(Methods=Methods2, Time= time_cri/60, stages=stages2)
df_time_cri$stages <- factor(df_time_cri$stages, levels = P3)

Time_plot_cri=ggplot(df_time_cri, aes(x = Methods, y = Time, group = stages)) +
  geom_point(aes(color = Methods),size = 6) +  # Points for each method
  geom_line( linetype = "solid") +
  facet_wrap(~ stages, scales = "free_x",nrow=1,labeller = labeller(stages = function(x) paste0("n=", x))) +  # Move facet labels to the bottom
  # xlab("N") +
  ylab("Time (mins)") +
  ggtitle("(d) Criterion Time (mins)") +
  scale_color_manual(values = method_colors, name = "Methods") +
  #geom_vline(
   # xintercept = Inf,  # Adds a vertical boundary at the end of each facet
  #  color = "lightgray", linetype = "solid", size = 0.8) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.size = unit(1.5, "lines"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.title.x = element_blank(),
    #axis.text.x=element_text(size = 10, face = "bold", colour = "black",angle = 45, hjust = 1),
    axis.text.x=element_blank(),
    axis.text.y = element_text(size = 15, face = "bold", colour = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
   # strip.background = element_blank(),  # Optional: Remove background of facet labels
    strip.text = element_text(size = 15, face = "bold")  # Optional: Adjust text size and style
  ) +
  guides(color = guide_legend(nrow = 1))

# 
filename<- paste("all.pdf",sep="")
pdf(file=filename,width =15, # The width of the plot in inches
    height =10)
ggarrange( RMSE_plot,Time_plot_whole,Time_plot_update,Time_plot_cri,ncol=2, nrow=2, common.legend = TRUE, legend="bottom")
 dev.off()


save.image("Total.Rdata")




