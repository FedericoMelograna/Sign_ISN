
# GOAL doing some report on the HIGH-dimensional server experiment --------

# http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements

# interesting themes
# 11/5

library(dplyr)
library("xlsx")
library(tidyverse)
require(gridExtra)

plot_barplot_g = function( Dataset, ymin_ = 0, ymax_ = 1,size = 35){
  
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
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = size),legend.position="top")
  
  # dev.off()
}


plot_barplot_g_nosd = function( Dataset, ymin_ = 0, ymax_ = 1, size = 35){
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
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = size),legend.position="top")
  
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
    geom_point(aes(stroke = 1.2),size = 8,show.legend = FALSE)+#,position=position_jitter(h=0.01,w=0)) +
    # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
    # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
    # ggtitle("Performance 1 outlier")+#,"This is the graph for the best methods")+
    theme_classic(base_size = 17, base_line_size = 1.1)+
    # theme_bw(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 35),legend.position="top")
  
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
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 35),legend.position="top")
  
}
# Data importing ----------------------------------------------------------


set.seed(123)

data_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Normality_simulations/Data"
result_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Normality_simulations/Graphs/Graphs_grouped_by_k/"
setwd(data_path)

Results_all_methods200 <- read.csv("Results_all_methods200.txt")

Res = Results_all_methods200[,2:ncol(Results_all_methods200)]
colnames(Res) = c("N_obs", "N_out", "k", "same_distrib", colnames(Res)[1:29])
head(Res)



# GLOBAL mean and median statistic ----------------------------------------



median_glob = apply(Res, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

# beware: not for every method each setting is available
setwd(result_path)

dd = data.frame(rbind(median_glob, mean_glob))
# write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC.xlsx", sheetName = "Sheet1", 
#            col.names = TRUE, row.names = TRUE, append = FALSE)



dd

## select the best method for each category. 

## auc.roc_Loo_multi.  
# auc.roc_Loo.
# auc.roc_KNN_5_sqrt_N.
# auc.roc_optics_k_5_diff_net.
# auc.roc_OTS.
# auc.roc_OTS_cosine.
# auc.roc_OTS_cosine_med.
# auc_DPCC
# auc.roc_cook_med.


# Those are the winning methods


# N.b. the median is the best method, the cosine is clearly better than the euclidean distance and the enseble method is a step up on 
#  the single methods






# Summarize information grouped -------------------------------------------



my.df1 = Res



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



#####
# Data preparation


## gg plot of two examples
BY_n_obs = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(k) %>% 
  summarise_all(c("mean"),na.rm = TRUE)

Mean_obs =unlist(BY_n_obs%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))

mean_database = data.frame("K"=rep(BY_n_obs$k, length(Mean_obs)/7), "Median_AUC" = Mean_obs, "group"=gsub(".[0-9]","",names(Mean_obs)))

BY_n_obs_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(k) %>% 
  summarise_all(c("sd"),na.rm = TRUE)

SD_obs =unlist(BY_n_obs_sd%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))


global_database = cbind(mean_database, "sd"= SD_obs)
global_database$K = as.factor(global_database$K)
global_database$group = as.factor(global_database$group)
# Use geom_pointrange



# 4 plot: LOO ; Cook's, kNN, optics  --------------------

METHODS_dataset = c("auc.roc_cook_max" ,"auc.roc_cook_med","auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
METHODS_avg_dataset = global_database[global_database$group %in% METHODS_dataset,]
colnames(METHODS_avg_dataset)
METHODS_avg_dataset$group = ((as.character(METHODS_avg_dataset$group)))
# METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_optics_k"] = "OPTICS"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"

METHODS_avg_dataset$group = as.factor(METHODS_avg_dataset$group)

METHODS_avg_dataset = METHODS_avg_dataset[complete.cases(METHODS_avg_dataset),]
# ELIMINATE COOK for k = 2
METHODS_avg_dataset = METHODS_avg_dataset[ rownames(METHODS_avg_dataset) != rownames(METHODS_avg_dataset["auc.roc_cook_med.1",] ),]
METHODS_avg_dataset = METHODS_avg_dataset[ rownames(METHODS_avg_dataset) != rownames(METHODS_avg_dataset["auc.roc_cook_max.1",] ),]

# %>% select(-c"auc.roc_cook_med.1")
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



# 5 plot: LOO ; best_plots  --------------------


ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_cosine_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N", "auc.roc_cook_max")
ALL_avg_dataset = global_database[global_database$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))

ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "mOTS cosine"

ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
# ELIMINATE COOK for k = 2
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]

(g = plot_barplot_g( Dataset = ALL_avg_dataset,0,1,30))
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


Mean_obs_single_out =unlist(BY_n_obs_singleout%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))
# Mean_obs_single_k =unlist(
  # BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
  # 
# )

mean_database_single_out = data.frame("K"=rep(BY_n_obs_singleout$k, length(Mean_obs_single_out)/7), "Median_AUC" = Mean_obs_single_out, "group"=gsub(".[0-9]","",names(Mean_obs_single_out)))

BY_n_obs_singlek_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out < 3) %>%
  group_by(N_out, k) %>%
  summarise_all(c("sd"), na.rm = T)

SD_obs_singlek =unlist(BY_n_obs_singlek_sd%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))


global_database_singleout = cbind(mean_database_single_out, "sd"= SD_obs_singlek)
global_database_singleout$K = as.factor(global_database_singleout$K)
global_database_singleout$group = as.factor(global_database_singleout$group)
# Use geom_pointrange



ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_cosine_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N", "auc.roc_cook_max")
ALL_avg_dataset = global_database_singleout[global_database_singleout$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "mOTS cosine"



ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
# ELIMINATE COOK for k = 2
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]

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

ALL_avg_dataset$K = (as.numeric(as.character(ALL_avg_dataset$K)))  




(g_pl = plot_ggplot( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl_ = plot_ggplot_nosd_( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl_1out = plot_ggplot_nosd_trasp( Dataset = ALL_avg_dataset, 0.3))

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

Mean_obs_5out =unlist(BY_n_obs_5out%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))
# Mean_obs_single_k =unlist(
# BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
# 
# )

mean_database_5out = data.frame("K"=rep(BY_n_obs_5out$k, length(Mean_obs_5out)/7), "Median_AUC" = Mean_obs_5out, "group"=gsub(".[0-9]","",names(Mean_obs_5out)))

BY_n_obs_5out_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( N_out == 5) %>%
  group_by(N_out, k) %>%
  summarise_all(c("sd"), na.rm = T)

SD_obs_5out =unlist(BY_n_obs_5out_sd%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))


global_database_5out = cbind(mean_database_5out, "sd"= SD_obs_5out)
global_database_5out$K = as.factor(global_database_5out$K)
global_database_5out$group = as.factor(global_database_5out$group)
# Use geom_pointrange



ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_cosine_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N", "auc.roc_cook_max")
ALL_avg_dataset = global_database_5out[global_database_5out$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)

ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "mOTS cosine"

ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
# ELIMINATE COOK for k = 2
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]

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

ALL_avg_dataset$K = (as.numeric(as.character(ALL_avg_dataset$K)))  


(g_pl = plot_ggplot( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl_ = plot_ggplot_nosd_( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl_5out = plot_ggplot_nosd_trasp( Dataset = ALL_avg_dataset, 0.3))



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


Mean_obs_10out =unlist(BY_n_obs_10out%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))
# Mean_obs_single_k =unlist(
# BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
# 
# )

mean_database_10out = data.frame("K"=rep(BY_n_obs_10out$k, length(Mean_obs_10out)/7), "Median_AUC" = Mean_obs_10out, "group"=gsub(".[0-9]","",names(Mean_obs_10out)))


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

SD_obs_10out =unlist(BY_n_obs_10out_sd%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))


global_database_10out = cbind(mean_database_10out, "sd"= SD_obs_10out)
global_database_10out$K = as.factor(global_database_10out$K)
global_database_10out$group = as.factor(global_database_10out$group)
# Use geom_pointrange


ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_cosine_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N", "auc.roc_cook_max")
ALL_avg_dataset = global_database_10out[global_database_10out$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)
ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO-ISN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's med"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_max"] = "Cook's max"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "mOTS cosine"


ALL_avg_dataset = ALL_avg_dataset[complete.cases(ALL_avg_dataset),]
# ELIMINATE COOK for k = 2
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_med.1",] ),]
ALL_avg_dataset = ALL_avg_dataset[ rownames(ALL_avg_dataset) != rownames(ALL_avg_dataset["auc.roc_cook_max.1",] ),]

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


ALL_avg_dataset$K = (as.numeric(as.character(ALL_avg_dataset$K)))  


(g_pl = plot_ggplot( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl = plot_ggplot_nosd_( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl_= plot_ggplot_nosd_( Dataset = ALL_avg_dataset, 0.3))
(g_nosd_pl_10out = plot_ggplot_nosd_trasp( Dataset = ALL_avg_dataset, 0.3))

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

###############################à------------------------------------------------------###########################
###############################à------------------------------------------------------###########################

###############################à------------------------------------------------------###########################
###############################à------------------------------------------------------###########################

ALL_avg_dataset$K = (as.numeric(as.character(ALL_avg_dataset$K)) + rep(c(0,0.05,0.1,0.15,0.2,0.25), each = 7))    
png("graphs_ALL_methods_10outlier_diff_width.png", width = 300, height = 300, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  labs(y= "Median AUC")+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Performance 10 outliers")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))


dev.off()


png("graphs_ALL_methods_10outlier_nosd_diff_width.png", width = 300, height = 300, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  labs(y= "Median AUC")+
  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Performance 10 outliers")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

dev.off()

png("graphs_ALL_methods_10outlier_nosd.png", width = 465, height = 225, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=K, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  labs(y= "Median AUC")+
  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Performance 10 outliers")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

dev.off()


