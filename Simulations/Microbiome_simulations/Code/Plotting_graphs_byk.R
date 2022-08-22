
data_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Data/Wrapped/"
graph_path ="C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Graphs/"

# dir.create(result_path)


library(dplyr)
library("xlsx")
library(tidyverse)
require(gridExtra)

plot_barplot_g = function( Dataset, ymin_ = 0, ymax_ = 1){
  
  if (sum(Dataset$Median_AUC+ Dataset$sd > 1) > 0){
    ymax_ = 1.05
  } #else {ymax_ = 1}
  # png(name, width = width, height = height, units='mm', res = 300)
  ggplot(Dataset, aes(fill=group, y=Median_AUC, x=K)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(y= "Median AUC", fill = "")+ #, x = "x axis name")
    geom_errorbar(aes(ymin=(Median_AUC-sd), ymax=(Median_AUC+sd)), width = 0.3, position = position_dodge(0.9))+
    # ylim(ymin_,ymax_)+
    coord_cartesian(ylim=c(ymin_,ymax_))+
    theme_bw(base_size = 17, base_line_size = 1.1)+
    # theme_dark()+
    # theme_classic(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 35),legend.position="top")
  
  # dev.off()
}


plot_barplot_g_nosd = function( Dataset, ymin_ = 0, ymax_ = 1){
  # png(name, width = width, height = height, units='mm', res = 300)
  ggplot(Dataset, aes(fill=group, y=Median_AUC, x=K)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(y= "Median AUC", fill = "")+ #, x = "x axis name")
    # geom_errorbar(aes(ymin=(Median_AUC-sd), ymax=(Median_AUC+sd)), width = 0.3, position = position_dodge(0.9))+
    # ylim(ymin_,ymax_)+
    coord_cartesian(ylim=c(ymin_,ymax_))+
    theme_bw(base_size = 17, base_line_size = 1.1)+
    # theme_dark()+
    # theme_classic(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 35),legend.position="top")
  
  # dev.off()
}


plot_ggplot = function( Dataset, ymin_ = 0, ymax_ = 1){
  ggplot(Dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
    labs(y= "Median AUC", color = "")+ #, x = "x axis name") +
    geom_line(size=2.5)+
    ylim(ymin_,ymax_)+
    geom_point(aes(stroke = 1.2),size = 6,show.legend = FALSE) +
    geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
    geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
    # ggtitle("Performance 1 outlier")+#,"This is the graph for the best methods")+
    theme_classic(base_size = 17, base_line_size = 1.1)+
    # theme_bw(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 35),legend.position="top")
  
}


plot_ggplot_nosd = function( Dataset, ymin_ = 0, ymax_ = 1){
  ggplot(Dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
    labs(y= "Median AUC", color = "")+ #, x = "x axis name") +
    geom_line(size=1.5)+
    ylim(ymin_,ymax_)+
    geom_point(aes(stroke = 1.2),size = 6,show.legend = FALSE) +
    # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
    # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
    # ggtitle("Performance 1 outlier")+#,"This is the graph for the best methods")+
    theme_classic(base_size = 17, base_line_size = 1.1)+
    # theme_bw(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 35),legend.position="top")
  
}

plot_ggplot_nosd_ = function( Dataset, ymin_ = 0, ymax_ = 1){
  ggplot(Dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
    labs(y= "Median AUC", color = "")+ #, x = "x axis name") +
    geom_line(size=2.5)+
    ylim(ymin_,ymax_)+
    geom_point(aes(stroke = 1.2),size = 8,show.legend = FALSE) +
    # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
    # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
    # ggtitle("Performance 1 outlier")+#,"This is the graph for the best methods")+
    theme_classic(base_size = 17, base_line_size = 1.1)+
    # theme_bw(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 40),legend.position="top")
  
}
plot_ggplot_nosd_trasp = function( Dataset, ymin_ = 0, ymax_ = 1){
  ggplot(Dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
    labs(y= "Median AUC", color = "")+ #, x = "x axis name") +
    geom_line(size=2.5, alpha = 0.8)+
    ylim(ymin_,ymax_)+
    geom_point(aes(stroke = 1.2),size = 8,show.legend = FALSE, alpha = 0.5)+#,position=position_jitter(h=0.01,w=0)) +
    # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
    # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
    # ggtitle("Performance 1 outlier")+#,"This is the graph for the best methods")+
    theme_classic(base_size = 17, base_line_size = 1.1)+
    # theme_bw(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 40),legend.position="top")
  
}
# Data importing ----------------------------------------------------------


set.seed(123)

setwd(data_path)
Results_all_methods200 <- read.csv("Results_all_methods.txt")


setwd(graph_path)
Res = Results_all_methods200[,2:ncol(Results_all_methods200)]
colnames(Res) = c("N_obs", "N_out", "k", colnames(Res)[4:36])
head(Res)

# Summarize information grouped -------------------------------------------

direttoria_creata = "high_mult_high_het"
dir.create(file.path(direttoria_creata), showWarnings = FALSE)
setwd(direttoria_creata)


Res_mult2 = Res[Res$type_of_pareto_data == 1 & Res$mult == 2,]


my.df1 = Res_mult2



# Grouped by n_observation ------------------------------------------------

# with na removed

BY_n_obs = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(N_obs) %>% 
  summarise_all(c("mean", "sd"),na.rm = TRUE)

# without

BY_n_obs = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(N_obs) %>% 
  summarise_all(c("mean", "sd"),na.rm = TRUE)

# Data preparation





## gg plot of two examples
BY_n_obs = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(k) %>% 
  summarise_all(c("mean"),na.rm = TRUE)

rbind()
Mean_obs = unlist(BY_n_obs%>%select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))

# Mean_obs =unlist(BY_n_obs%>%select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter)))


mean_database = data.frame("K"=rep(BY_n_obs$k, length(Mean_obs)/4), "Median_AUC" = Mean_obs, "group"=gsub(".[0-9]","",names(Mean_obs)))

BY_n_obs_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(k) %>% 
  summarise_all(c("sd"),na.rm = TRUE)

SD_obs = unlist(BY_n_obs_sd%>%select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))


global_database = cbind(mean_database, "sd"= SD_obs)
global_database$K = as.factor(global_database$K)
global_database$group = as.factor(global_database$group)
# Use geom_pointrange


ggplot(global_database[c(1:20,44:48),], aes(x=K, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))


# 1 plot: spoutlier with cosine and not and the median --------------------
single_cosine = c("auc.roc_OTS","auc.roc_spoutlier", "auc.roc_OTS_cosine")
Cosine_dataset = global_database[global_database$group %in% single_cosine,]
colnames(Cosine_dataset)

# Cosine_dataset$K = (as.numeric(as.character(Cosine_dataset$K)))#  + rep(c(1,11,21), each = 4))
Cosine_dataset$group = ((as.character(Cosine_dataset$group)))
Cosine_dataset$group[Cosine_dataset$group == "auc.roc_OTS"] = "OTS euclidean"
Cosine_dataset$group[Cosine_dataset$group == "auc.roc_OTS_cosine"] = "OTS cosine"
Cosine_dataset$group[Cosine_dataset$group == "auc.roc_spoutlier"] = "Spoutlier-l"
# Cosine_dataset$K = (as.numeric(as.character(Cosine_dataset$K))  + rep(c(0,0.1,0.2), each = 4))
Cosine_dataset2 = Cosine_dataset[-5,]
(g = plot_barplot_g( Dataset = Cosine_dataset2))
(g_nosd = plot_barplot_g_nosd( Dataset = Cosine_dataset2))
png("barplot_single_COSINE.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_single_COSINE_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_single_COSINE_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
# Cosine_dataset$K = (as.numeric(as.character(Cosine_dataset$K))  + rep(c(0,0.1,0.2), each = 4))
Cosine_dataset$K = (as.numeric(as.character(Cosine_dataset$K))  + rep(c(0,0.1,0.2), each = 4))
Cosine_dataset3 = Cosine_dataset[-5,]
(g_pl = plot_ggplot( Dataset = Cosine_dataset3, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = Cosine_dataset3, 0.5))
(g_nosd_pl_= plot_ggplot_nosd_( Dataset = Cosine_dataset3, 0.5))
png("graphs_single_COSINE.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("graphs_single_COSINE_noSD.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_single_COSINE_noSD_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_single_COSINE_noSD_diffsize2.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
# 2 plot: spoutlier with global and median  --------------------
global_cosine = c("auc.roc_OTS_glob_avg_OTS","auc.roc_OTS_euc_med", "auc.roc_OTS_cosine_med")
Cosine_avg_dataset = global_database[global_database$group %in% global_cosine,]
colnames(Cosine_avg_dataset)
Cosine_avg_dataset$group = ((as.character(Cosine_avg_dataset$group)))
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "mOTS cosine"
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_euc_med"] = "mOTS euc"
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_glob_avg_OTS"] = "mOTS glob"
Cosine_avg_dataset_num = Cosine_avg_dataset
Cosine_avg_dataset_num$K = (as.numeric(as.character(Cosine_avg_dataset_num$K))  + rep(c(0,0.1,0.2), each = 4))
Cosine_avg_dataset = Cosine_avg_dataset[complete.cases(Cosine_avg_dataset),]
(g = plot_barplot_g( Dataset = Cosine_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = Cosine_avg_dataset))
png("barplot_group_COSINE.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_group_COSINE_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_group_COSINE_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
(g_pl = plot_ggplot( Dataset = Cosine_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = Cosine_avg_dataset_num, 0.5))
(g_nosd_pl_= plot_ggplot_nosd_( Dataset = Cosine_avg_dataset_num, 0.5))
png("graphs_group_COSINE.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("graphs_group_COSINE_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_group_COSINE_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_group_COSINE_nosd_diffsize2.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
qplot1_sd = plot_barplot_g( Dataset = Cosine_dataset2)
qplot1 = plot_barplot_g_nosd( Dataset = Cosine_dataset2)
qplot2_sd = plot_barplot_g( Dataset = Cosine_avg_dataset)
qplot2 = plot_barplot_g_nosd( Dataset = Cosine_avg_dataset)
grid.arrange(qplot1,qplot2)
grid.arrange(qplot1,qplot2, ncol = 2)
png("barplot_COSINE_GROUP_AND_SINGLE.png", width = 465, height = 225, units='mm', res = 300)
grid.arrange(qplot1_sd,qplot2_sd)
dev.off()
png("barplot_COSINE_nosd_GROUP_AND_SINGLE.png", width = 465, height = 225, units='mm', res = 300)
grid.arrange(qplot1,qplot2)
dev.off()
png("barplot_COSINE_GROUP_AND_SINGLE_diffsize.png", width = 300, height = 300, units='mm', res = 300)
grid.arrange(qplot1_sd,qplot2_sd)
dev.off()
png("barplot_COSINE_nosd_GROUP_AND_SINGLE_diffsize.png", width = 300, height = 300, units='mm', res = 300)
grid.arrange(qplot1,qplot2)
dev.off()
png("barplot_COSINE_nosd_GROUP_AND_SINGLE_diff_shape.png", width = 465, height = 225, units='mm', res = 300)
grid.arrange(qplot1,qplot2)
dev.off()
png("barplot_COSINE_GROUP_AND_SINGLE_diff_shape.png", width = 465, height = 225, units='mm', res = 300)
grid.arrange(qplot1_sd,qplot2_sd)
dev.off()
png("barplot_COSINE_nosd_GROUP_AND_SINGLE_vert.png", width = 600, height = 300, units='mm', res = 300)
grid.arrange(qplot1,qplot2, ncol = 2)
dev.off()
png("barplot_COSINE_GROUP_AND_SINGLE_vert.png", width = 600, height = 300, units='mm', res = 300)
grid.arrange(qplot1_sd,qplot2_sd, ncol = 2)
dev.off()
# 3 plot: LOO ; MultiLOO, DPCC  --------------------
LOO_dataset = c("auc.roc_Loo_multi","auc_DPC", "auc.roc_Loo")
LOO_avg_dataset = global_database[global_database$group %in% LOO_dataset,]
colnames(LOO_avg_dataset)
LOO_avg_dataset$group = ((as.character(LOO_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] = "SSN-m"
LOO_avg_dataset$group[LOO_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO-ISN"
LOO_avg_dataset$group[LOO_avg_dataset$group == "auc.roc_Loo"] = "LOO-ISN"
LOO_avg_dataset$group = as.factor(LOO_avg_dataset$group)
LOO_avg_dataset_num = LOO_avg_dataset
LOO_avg_dataset_num$K = (as.numeric(as.character(LOO_avg_dataset_num$K))   + rep(c(0,0.1,0.2), each = 4))
LOO_avg_dataset = LOO_avg_dataset[complete.cases(LOO_avg_dataset),]
(g = plot_barplot_g( Dataset = LOO_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = LOO_avg_dataset))
png("barplot_group_LOO_pvalue.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_group_LOO_pvalue_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_group_LOO_pvalue_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
LOO_avg_dataset_num= LOO_avg_dataset_num[complete.cases(LOO_avg_dataset_num),]
(g_pl = plot_ggplot( Dataset = LOO_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = LOO_avg_dataset_num, 0.5))
(g_nosd_pl_= plot_ggplot_nosd_( Dataset = LOO_avg_dataset_num, 0.5))
png("graphs_group_LOO_pvalue.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("graphs_group_LOO_pvalue_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_group_LOO_pvalue_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_group_LOO_pvalue_nosd_diffsize2.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
# 4 plot: LOO ; Cook's, kNN, optics  --------------------
METHODS_dataset = c("auc.roc_cook_max","auc.roc_cook_med","auc.roc_optics_sqrt_n", "auc.roc_KNN_sqrt_N")
METHODS_avg_dataset = global_database[global_database$group %in% METHODS_dataset,]
colnames(METHODS_avg_dataset)
METHODS_avg_dataset$group = ((as.character(METHODS_avg_dataset$group)))
# METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_optics_sqrt_n"] = "OPTICS N"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
METHODS_avg_dataset$group = as.factor(METHODS_avg_dataset$group)
METHODS_avg_dataset_num = METHODS_avg_dataset
METHODS_avg_dataset_num$K = (as.numeric(as.character(METHODS_avg_dataset_num$K))   + rep(c(0,0.1,0.2), each = 4))
METHODS_avg_dataset = METHODS_avg_dataset[ rownames(METHODS_avg_dataset) != rownames(METHODS_avg_dataset["auc.roc_cook_med.1",] ),]
METHODS_avg_dataset = METHODS_avg_dataset[ rownames(METHODS_avg_dataset) != rownames(METHODS_avg_dataset["auc.roc_cook_max.1",] ),]
METHODS_avg_dataset_num = METHODS_avg_dataset_num[ rownames(METHODS_avg_dataset_num) != rownames(METHODS_avg_dataset_num["auc.roc_cook_med.1",] ),]
METHODS_avg_dataset_num = METHODS_avg_dataset_num[ rownames(METHODS_avg_dataset_num) != rownames(METHODS_avg_dataset_num["auc.roc_cook_max.1",] ),]
(g = plot_barplot_g( Dataset = METHODS_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = METHODS_avg_dataset))
png("barplot_group_methods.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_group_methods_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_group_methods_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
(g_pl = plot_ggplot( Dataset = METHODS_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = METHODS_avg_dataset_num, 0.5))
(g_nosd_pl_= plot_ggplot_nosd_( Dataset = METHODS_avg_dataset_num, 0.5))
png("graphs_group_methods.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("graphs_group_methods_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_group_methods_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_group_methods_nosd_diffsize2.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
# 5 plot: LOO ; best_plots  --------------------
Mean_obs
mean_glob
ALL_dataset = c("auc.roc_cook_med","auc.roc_cook_max","auc_DPC", "auc.roc_Loo","auc.roc_OTS_euc_med", "auc.roc_optics_sqrt_n", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database[global_database$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo"] = "LOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_sqrt_n"] = "OPTICS N"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_euc_med"] = "mOTS euc"
ALL_avg_dataset_num = ALL_avg_dataset
ALL_avg_dataset_num$K = (as.numeric(as.character(ALL_avg_dataset_num$K))   + rep(c(0,0.05,0.1,0.15,0.2,0.25), each = 4))
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_med.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_max.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
(g = plot_barplot_g( Dataset = ALL_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = ALL_avg_dataset))
png("barplot_ALL_method.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_ALL_method_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_ALL_method_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
(g_pl = plot_ggplot( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_= plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
png("graphs_ALL_methods.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("graphs_ALL_methods_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_ALL_methods_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("graphs_ALL_methods_nosd_diffsize2.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()



# SINGLE OUTLIER -------------------------------------------------------------
BY_n_obs_singleout = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out < 3) %>%
  group_by(N_out, k) %>%
  summarise_all(funs(mean(. )))
BY_n_obs_singleout = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out < 3) %>%
  group_by(N_out, k) %>%
  summarise_all(c("mean"), na.rm = T)
Mean_obs_single_out =unlist(BY_n_obs_singleout%>%select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))
# Mean_obs_single_k =unlist(
# BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
#
# )
mean_database_single_out = data.frame("K"=rep(BY_n_obs_singleout$k, length(Mean_obs_single_out)/4), "Median_AUC" = Mean_obs_single_out, "group"=gsub(".[0-9]","",names(Mean_obs_single_out)))
BY_n_obs_singlek_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out < 3) %>%
  group_by(N_out, k) %>%
  summarise_all(c("sd"), na.rm = T)
SD_obs_singlek =unlist(BY_n_obs_singlek_sd%>%select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))
global_database_singleout = cbind(mean_database_single_out, "sd"= SD_obs_singlek)
global_database_singleout$K = as.factor(global_database_singleout$K)
global_database_singleout$group = as.factor(global_database_singleout$group)
# Use geom_pointrange
ALL_dataset = c("auc.roc_cook_med","auc.roc_cook_max","auc_DPC", "auc.roc_Loo","auc.roc_OTS_euc_med", "auc.roc_optics_sqrt_n", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database_singleout[global_database_singleout$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo"] = "LOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_sqrt_n"] = "OPTICS N"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_euc_med"] = "mOTS euc"
ALL_avg_dataset_num = ALL_avg_dataset
ALL_avg_dataset_num$K = (as.numeric(as.character(ALL_avg_dataset_num$K))   + rep(c(0,0.05,0.1,0.15,0.2,0.25), each = 4))
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_max.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_max.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
(g = plot_barplot_g( Dataset = ALL_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = ALL_avg_dataset))
png("barplot_ALL_methods_1outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_ALL_methods_1outlier_diff_width_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_ALL_methods_1outlier_diff_width_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
(g_pl = plot_ggplot( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_ = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_1out= plot_ggplot_nosd_trasp( Dataset = ALL_avg_dataset_num, 0.5))
png("ggplot_ALL_methods_1outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("ggplot_ALL_methods_1outlier_diff_width_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("ggplot_ALL_methods_1outlier_diff_width_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("ggplot_ALL_methods_1outlier_diff_width_nosd_diffsize_inv__.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
# 5 OUTLIER -------------------------------------------------------------
BY_n_obs_5out = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 5) %>%
  group_by(N_out, k) %>%
  summarise_all(funs(mean(. )))
# NA removal: consider for cook not k = 17 N = 100
BY_n_obs_5out = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 5) %>%
  group_by(N_out, k) %>%
  summarise_all(c("mean"), na.rm = T)
Mean_obs_5out =unlist(BY_n_obs_5out%>%select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))
# Mean_obs_single_k =unlist(
# BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
#
# )
mean_database_5out = data.frame("K"=rep(BY_n_obs_5out$k, length(Mean_obs_5out)/4), "Median_AUC" = Mean_obs_5out, "group"=gsub(".[0-9]","",names(Mean_obs_5out)))
BY_n_obs_5out_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 5) %>%
  group_by(N_out, k) %>%
  summarise_all(c("sd"), na.rm = T)
SD_obs_5out =unlist(BY_n_obs_5out_sd %>% select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))
global_database_5out = cbind(mean_database_5out, "sd"= SD_obs_5out)
global_database_5out$K = as.factor(global_database_5out$K)
global_database_5out$group = as.factor(global_database_5out$group)
# Use geom_pointrange
ALL_dataset = c("auc.roc_cook_med", "auc.roc_cook_max","auc_DPC", "auc.roc_Loo","auc.roc_OTS_euc_med", "auc.roc_optics_sqrt_n", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database_5out[global_database_5out$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo"] = "LOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_sqrt_n"] = "OPTICS N"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_euc_med"] = "mOTS euc"
ALL_avg_dataset_num = ALL_avg_dataset
ALL_avg_dataset_num$K = (as.numeric(as.character(ALL_avg_dataset_num$K))   + rep(c(0,0.05,0.1,0.15,0.2,0.25), each = 4))
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_med.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_max.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
(g = plot_barplot_g( Dataset = ALL_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = ALL_avg_dataset))
png("barplot_ALL_methods_5outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_ALL_methods_5outlier_diff_width_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_ALL_methods_5outlier_diff_width_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
(g_pl = plot_ggplot( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_ = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_5out= plot_ggplot_nosd_trasp( Dataset = ALL_avg_dataset_num, 0.5))
png("ggplot_ALL_methods_5outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("ggplot_ALL_methods_5outlier_diff_width_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("ggplot_ALL_methods_5outlier_diff_width_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("ggplot_ALL_methods_5outlier_diff_width_nosd_diffsize_inv__.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
# 10 OUTLIER -------------------------------------------------------------
BY_n_obs_10out = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 10) %>%
  group_by(N_out, k) %>%
  summarise_all(funs(mean(. )))
# THIS ONE HAS NA REMOVAL : E.G. THERE IS NO NA FOR COOK FOR K = 17 CAUSED BY CASE WITH 100 OBS AND
#EDGES > N
BY_n_obs_10out = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 10) %>%
  group_by(N_out, k) %>%
  summarise_all(c("mean"),na.rm = TRUE)
Mean_obs_10out =unlist(BY_n_obs_10out %>% select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))
# Mean_obs_single_k =unlist(
# BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
#
# )
mean_database_10out = data.frame("K"=rep(BY_n_obs_10out$k, length(Mean_obs_10out)/4), "Median_AUC" = Mean_obs_10out, "group"=gsub(".[0-9]","",names(Mean_obs_10out)))
# SD NON REMOVAL
BY_n_obs_10out_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 10) %>%
  group_by(N_out, k) %>%
  summarise_all(c("sd"))
BY_n_obs_10out_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 10) %>%
  group_by(N_out, k) %>%
  summarise_all(c("sd"), na.rm = TRUE)
SD_obs_10out =unlist(BY_n_obs_10out_sd %>% select(-c(N_obs,N_out, type_of_pareto_data, mult, intensity_parameter, perc_increase, Type, Size)))
global_database_10out = cbind(mean_database_10out, "sd"= SD_obs_10out)
global_database_10out$K = as.factor(global_database_10out$K)
global_database_10out$group = as.factor(global_database_10out$group)
# Use geom_pointrange
ALL_dataset = c("auc.roc_cook_max", "auc.roc_cook_med","auc_DPC", "auc.roc_Loo","auc.roc_OTS_euc_med", "auc.roc_optics_sqrt_n", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database_10out[global_database_10out$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo"] = "LOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_sqrt_n"] = "OPTICS N"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_euc_med"] = "mOTS euc"
ALL_avg_dataset_num = ALL_avg_dataset
ALL_avg_dataset_num$K = (as.numeric(as.character(ALL_avg_dataset_num$K))   + rep(c(0,0.05,0.1,0.15,0.2,0.25), each = 4))
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_med.1",] ),]
ALL_avg_dataset_num = ALL_avg_dataset_num[ rownames(ALL_avg_dataset_num) != rownames(ALL_avg_dataset_num["auc.roc_cook_max.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
(g = plot_barplot_g( Dataset = ALL_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = ALL_avg_dataset))
png("barplot_ALL_methods_10outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_ALL_methods_10outlier_diff_width_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_ALL_methods_10outlier_diff_width_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()
(g_pl = plot_ggplot( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_ = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_ = plot_ggplot_nosd_( Dataset = ALL_avg_dataset_num, 0.5))
(g_nosd_pl_10out= plot_ggplot_nosd_trasp( Dataset = ALL_avg_dataset_num, 0.5))
png("ggplot_ALL_methods_10outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)
g_pl
dev.off()
png("ggplot_ALL_methods_10outlier_diff_width_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd_pl
dev.off()
png("ggplot_ALL_methods_10outlier_diff_width_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd_pl
dev.off()
png("ggplot_ALL_methods_10outlier_diff_width_nosd_diffsize_inv__.png", width = 300, height = 350, units='mm', res = 300)
g_nosd_pl_
dev.off()
png("ggplot_ALLoutlier_inv__.png", width = 900, height = 350, units='mm', res = 300)
grid.arrange(g_nosd_pl_1out, g_nosd_pl_5out,g_nosd_pl_10out, ncol = 3)
dev.off()
