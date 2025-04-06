
#------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(latex2exp)
library(ggpubr)
library("gridExtra")






#test
testdata_frame=data.frame(Data_M_c0)
names(testdata_frame)=c("x","z","y")
z_1_test=testdata_frame %>% filter(z ==1 )
z_2_test=testdata_frame %>% filter(z ==2 )
z_3_test=testdata_frame %>% filter(z ==3 )

search=6
#train
Data_M_c0_train=cbind(Data_M_c0_XZ,Yhat_stage[,search])
traindata_frame=data.frame(Data_M_c0_train)
names(traindata_frame)=c("x","z","y")
z_1_train=traindata_frame %>% filter(z ==1 )
z_2_train=traindata_frame %>% filter(z ==2 )
z_3_train=traindata_frame %>% filter(z ==3 )

seq_point=data.frame(n0_dat[1:(n0+search),],iteration=1:(n0+search))
names(seq_point)=c("x","z","y","iter")
z_1_n=seq_point %>% filter(z ==1 )
z_2_n=seq_point %>% filter(z ==2 )
z_3_n=seq_point %>% filter(z ==3 )


EI_cri_3=ggplot()+
  geom_line(data=z_1_test,color="coral",linetype="dashed",aes(x,y,colour="z=1"),size=0.5)+
  geom_line(data=z_2_test,color="cyan",linetype="dashed",aes(x,y,colour="z=2"),size=0.5)+
  geom_line(data=z_3_test,color="blue",linetype="dashed",aes(x,y,colour="z=3"),size=0.5)+
   # geom_line(data=z_1_train,linetype="dashed",color="coral2",aes(x,y,colour="z=1"))+
   # geom_line(data=z_2_train,linetype="dashed",color="cyan3",aes(x,y,colour="z=2"))+
   # geom_line(data=z_3_train,linetype="dashed",color="blue",aes(x,y,colour="z=3"))+
  geom_hline(yintercept =c(-1), linetype="solid",color = "orange", size=0.5)+
  geom_point(data=z_1_n[z_1_n$iter<=9,],color="coral1",aes(x,y,colour="z=1"),size=3)+
  geom_text(data=z_1_n[z_1_n$iter>9,],color="coral3" ,aes(x,y,label = iter,colour="z=1"),size = 4,fontface = "bold")+
  geom_point(data=z_2_n[z_2_n$iter<=9,],color="cyan1",aes(x,y,colour="z=2"),size=3)+
  geom_text(data=z_2_n[z_2_n$iter>9,],color="cyan3",aes(x,y,label = iter,colour="z=2"),size = 4,fontface = "bold")+
  geom_point(data=z_3_n[z_3_n$iter<=9,],color="blue1",aes(x,y,colour="z=3"),size=3)+
  geom_text(data=z_3_n[z_3_n$iter>9,],color="blue4",aes(x,y,label = iter,colour="z=3"),size = 4,fontface = "bold")+
  labs(y= TeX("$f(\\textbf{w})$"), x = "X")+ ggtitle("(a) EI; N=15")+
  coord_cartesian(xlim = c(0, 1))+xlim(0,1)+
  theme(axis.title.y = element_text(size = 13),axis.text=element_text(size=13,face="bold",colour="black"),
        text = element_text(size = 13,),plot.title = element_text(hjust = 0.5,size=13),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(color = guide_legend(nrow = 1))


search=6
#train
Data_M_c0_train=cbind(Data_M_c0_XZ,Yhat_stage[,search])
traindata_frame=data.frame(Data_M_c0_train)
names(traindata_frame)=c("x","z","y")
z_1_train=traindata_frame %>% filter(z ==1 )
z_2_train=traindata_frame %>% filter(z ==2 )
z_3_train=traindata_frame %>% filter(z ==3 )

seq_point=data.frame(n0_dat[1:(n0+search),],iteration=1:(n0+search))
names(seq_point)=c("x","z","y","iter")
z_1_n=seq_point %>% filter(z ==1 )
z_2_n=seq_point %>% filter(z ==2 )
z_3_n=seq_point %>% filter(z ==3 )

ARSD_cri_3=ggplot()+
  geom_line(data=z_1_test,color="coral",linetype="dashed",aes(x,y,colour="z=1"),size=0.5)+
  geom_line(data=z_2_test,color="cyan",linetype="dashed",aes(x,y,colour="z=2"),size=0.5)+
  geom_line(data=z_3_test,color="blue",linetype="dashed",aes(x,y,colour="z=3"),size=0.5)+
  # geom_line(data=z_1_train,linetype="dashed",color="coral2",aes(x,y,colour="z=1"))+
  # geom_line(data=z_2_train,linetype="dashed",color="cyan3",aes(x,y,colour="z=2"))+
  # geom_line(data=z_3_train,linetype="dashed",color="blue",aes(x,y,colour="z=3"))+
  geom_hline(yintercept =c(-1), linetype="solid",color = "orange", size=0.5)+
  geom_point(data=z_1_n[z_1_n$iter<=9,],color="coral1",aes(x,y,colour="z=1"),size=3)+
  geom_text(data=z_1_n[z_1_n$iter>9,],color="coral3" ,aes(x,y,label = iter,colour="z=1"),size = 4,fontface = "bold")+
  geom_point(data=z_2_n[z_2_n$iter<=9,],color="cyan1",aes(x,y,colour="z=2"),size=3)+
  geom_text(data=z_2_n[z_2_n$iter>9,],color="cyan3",aes(x,y,label = iter,colour="z=2"),size = 4,fontface = "bold")+
  geom_point(data=z_3_n[z_3_n$iter<=9,],color="blue1",aes(x,y,colour="z=3"),size=3)+
  geom_text(data=z_3_n[z_3_n$iter>9,],color="blue4",aes(x,y,label = iter,colour="z=3"),size = 4,fontface = "bold")+
  labs(y= TeX("$f(\\textbf{w})$"), x = "X")+ ggtitle("(b) ARSD; N=15")+
  coord_cartesian(xlim = c(0, 1))+xlim(0,1)+
  theme(axis.title.y = element_text(size = 13),axis.text=element_text(size=13,face="bold",colour="black"),
        text = element_text(size = 13,),plot.title = element_text(hjust = 0.5,size=13),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(color = guide_legend(nrow = 1))

search=6
#train
Data_M_c0_train=cbind(Data_M_c0_XZ,Yhat_stage[,search])
traindata_frame=data.frame(Data_M_c0_train)
names(traindata_frame)=c("x","z","y")
z_1_train=traindata_frame %>% filter(z ==1 )
z_2_train=traindata_frame %>% filter(z ==2 )
z_3_train=traindata_frame %>% filter(z ==3 )

seq_point=data.frame(n0_dat[1:(n0+search),],iteration=1:(n0+search))
names(seq_point)=c("x","z","y","iter")
z_1_n=seq_point %>% filter(z ==1 )
z_2_n=seq_point %>% filter(z ==2 )
z_3_n=seq_point %>% filter(z ==3 )

LCB_cri_3=ggplot()+
  geom_line(data=z_1_test,color="coral",linetype="dashed",aes(x,y,colour="z=1"),size=0.5)+
  geom_line(data=z_2_test,color="cyan",linetype="dashed",aes(x,y,colour="z=2"),size=0.5)+
  geom_line(data=z_3_test,color="blue",linetype="dashed",aes(x,y,colour="z=3"),size=0.5)+
  # geom_line(data=z_1_train,linetype="dashed",color="coral2",aes(x,y,colour="z=1"))+
  # geom_line(data=z_2_train,linetype="dashed",color="cyan3",aes(x,y,colour="z=2"))+
  # geom_line(data=z_3_train,linetype="dashed",color="blue",aes(x,y,colour="z=3"))+
  geom_hline(yintercept =c(-1), linetype="solid",color = "orange", size=0.5)+
  geom_point(data=z_1_n[z_1_n$iter<=9,],color="coral1",aes(x,y,colour="z=1"),size=3)+
  geom_text(data=z_1_n[z_1_n$iter>9,],color="coral3" ,aes(x,y,label = iter,colour="z=1"),size = 4,fontface = "bold")+
  geom_point(data=z_2_n[z_2_n$iter<=9,],color="cyan1",aes(x,y,colour="z=2"),size=3)+
  geom_text(data=z_2_n[z_2_n$iter>9,],color="cyan3",aes(x,y,label = iter,colour="z=2"),size = 4,fontface = "bold")+
  geom_point(data=z_3_n[z_3_n$iter<=9,],color="blue1",aes(x,y,colour="z=3"),size=3)+
  geom_text(data=z_3_n[z_3_n$iter>9,],color="blue4",aes(x,y,label = iter,colour="z=3"),size = 4,fontface = "bold")+
  labs(y= TeX("$f(\\textbf{w})$"), x = "X")+ ggtitle("(c) LCB; N=15")+
  coord_cartesian(xlim = c(0, 1))+xlim(0,1)+
  theme(axis.title.y = element_text(size = 13),axis.text=element_text(size=13,face="bold",colour="black"),
        text = element_text(size = 13,),plot.title = element_text(hjust = 0.5,size=13),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(color = guide_legend(nrow = 1))


search=6
#train

Y_res=read.csv("func_hybridM_0_Y.csv",header=FALSE)
X_res=read.csv("func_hybridM_0_X.csv",header = FALSE)
n0_dat=cbind(X_res[,2],X_res[,1],Y_res)
seq_point=data.frame(n0_dat[1:(n0+search),],iteration=1:(n0+search))
names(seq_point)=c("x","z","y","iter")
z_1_n=seq_point %>% filter(z ==1 )
z_2_n=seq_point %>% filter(z ==2 )
z_3_n=seq_point %>% filter(z ==3 )

MCTS_cri_3=ggplot()+
  geom_line(data=z_1_test,color="coral",linetype="dashed",aes(x,y,colour="z=1"),size=0.5)+
  geom_line(data=z_2_test,color="cyan",linetype="dashed",aes(x,y,colour="z=2"),size=0.5)+
  geom_line(data=z_3_test,color="blue",linetype="dashed",aes(x,y,colour="z=3"),size=0.5)+
  # geom_line(data=z_1_train,linetype="dashed",color="coral2",aes(x,y,colour="z=1"))+
  # geom_line(data=z_2_train,linetype="dashed",color="cyan3",aes(x,y,colour="z=2"))+
  # geom_line(data=z_3_train,linetype="dashed",color="blue",aes(x,y,colour="z=3"))+
  geom_hline(yintercept =c(-1), linetype="solid",color = "orange", size=0.5)+
  #geom_point(data=z_1_n[z_1_n$iter<=9,],color="coral1",aes(x,y,colour="z=1"),size=3)+
  geom_text(data=z_1_n,color="coral3" ,aes(x,y,label = iter,colour="z=1"),size = 4,fontface = "bold")+
  #geom_point(data=z_2_n[z_2_n$iter<=9,],color="cyan1",aes(x,y,colour="z=2"),size=3)+
  geom_text(data=z_2_n,color="cyan3",aes(x,y,label = iter,colour="z=2"),size = 4,fontface = "bold")+
  #geom_point(data=z_3_n[z_3_n$iter<=9,],color="blue1",aes(x,y,colour="z=3"),size=3)+
  geom_text(data=z_3_n,color="blue4",aes(x,y,label = iter,colour="z=3"),size = 4,fontface = "bold")+
  labs(y= TeX("$f(\\textbf{w})$"), x = "X")+ ggtitle("(d) Hybrid; N=15")+
  coord_cartesian(xlim = c(0, 1))+xlim(0,1)+
  theme(axis.title.y = element_text(size = 13),axis.text=element_text(size=13,face="bold",colour="black"),
        text = element_text(size = 13,),plot.title = element_text(hjust = 0.5,size=13),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(color = guide_legend(nrow = 1))

search=6
#train
Data_M_c0_train=cbind(Data_M_c0_XZ,Yhat_stage[,search])
traindata_frame=data.frame(Data_M_c0_train)
names(traindata_frame)=c("x","z","y")
z_1_train=traindata_frame %>% filter(z ==1 )
z_2_train=traindata_frame %>% filter(z ==2 )
z_3_train=traindata_frame %>% filter(z ==3 )

seq_point=data.frame(n0_dat[1:(n0+search),],iteration=1:(n0+search))
names(seq_point)=c("x","z","y","iter")
z_1_n=seq_point %>% filter(z ==1 )
z_2_n=seq_point %>% filter(z ==2 )
z_3_n=seq_point %>% filter(z ==3 )

oneshot_cri_3=ggplot()+
  geom_line(data=z_1_test,color="coral",linetype="dashed",aes(x,y,colour="z=1"),size=0.5)+
  geom_line(data=z_2_test,color="cyan",linetype="dashed",aes(x,y,colour="z=2"),size=0.5)+
  geom_line(data=z_3_test,color="blue",linetype="dashed",aes(x,y,colour="z=3"),size=0.5)+
  # geom_line(data=z_1_train,linetype="dashed",color="coral2",aes(x,y,colour="z=1"))+
  # geom_line(data=z_2_train,linetype="dashed",color="cyan3",aes(x,y,colour="z=2"))+
  # geom_line(data=z_3_train,linetype="dashed",color="blue",aes(x,y,colour="z=3"))+
  geom_hline(yintercept =c(-1), linetype="solid",color = "orange", size=0.5)+
  geom_point(data=z_1_n[z_1_n$iter<=9,],color="coral1",aes(x,y,colour="z=1"),size=3)+
  geom_text(data=z_1_n[z_1_n$iter>9,],color="coral3" ,aes(x,y,label = iter,colour="z=1"),size = 4,fontface = "bold")+
  geom_point(data=z_2_n[z_2_n$iter<=9,],color="cyan1",aes(x,y,colour="z=2"),size=3)+
  geom_text(data=z_2_n[z_2_n$iter>9,],color="cyan3",aes(x,y,label = iter,colour="z=2"),size = 4,fontface = "bold")+
  geom_point(data=z_3_n[z_3_n$iter<=9,],color="blue1",aes(x,y,colour="z=3"),size=3)+
  geom_text(data=z_3_n[z_3_n$iter>9,],color="blue4",aes(x,y,label = iter,colour="z=3"),size = 4,fontface = "bold")+
  labs(y= TeX("$f(\\textbf{w})$"), x = "X")+ ggtitle("(e) one-shot; N=15")+
  coord_cartesian(xlim = c(0, 1))+xlim(0,1)+
  theme(axis.title.y = element_text(size = 13),axis.text=element_text(size=13,face="bold",colour="black"),
        text = element_text(size = 13,),plot.title = element_text(hjust = 0.5,size=13),
        panel.background = element_blank(),axis.line = element_line(colour = "black"))+guides(color = guide_legend(nrow = 1))




filename<- paste("optim_methods.pdf",sep="")
pdf(file=filename,width =12, # The width of the plot in inches
    height =7)
# layout <- rbind(
#   c(1, 2),  # First row: two plots
#   c(3, 4),  # Second row: two plots
#   c(NA, 5)  # Third row: center the last plot
# )
ggarrange( EI_cri_3,ARSD_cri_3,LCB_cri_3,MCTS_cri_3,oneshot_cri_3,ncol=3, nrow=2,  layout_matrix = layout, common.legend = TRUE, legend="bottom")

# library(cowplot)
# 
# # Create a 2x2 grid for the first four plots
# top_plots <- plot_grid(EI_cri_3, ARSD_cri_3, LCB_cri_3, MCTS_cri_3, ncol = 2)
# 
# # Add the last plot centered below the 2x2 grid
# final_plot <- plot_grid(top_plots, oneshot_cri_3, ncol = 1, rel_heights = c(2, 1))
# 
# # Display the final plot
# final_plot
# 




dev.off()

save.image("optim_methods.RData")








