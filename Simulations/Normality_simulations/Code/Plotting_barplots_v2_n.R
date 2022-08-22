
# GOAL doing some report on the HIGH-dimensional server experiment --------


# 11/5

library(dplyr)
library("xlsx")
library(tidyverse)
require(gridExtra)

plot_barplot_g = function( Dataset, ymin_ = 0, ymax_ = 1){
  
  if (sum(Dataset$Median_AUC+ Dataset$sd > 1) > 0){
    ymax_ = 1.05
  } #else {ymax_ = 1}
  # png(name, width = width, height = height, units='mm', res = 300)
  ggplot(Dataset, aes(fill=group, y=Median_AUC, x=N)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(y= "Median AUC", fill = "")+ #, x = "x axis name")
    geom_errorbar(aes(ymin=(Median_AUC-sd), ymax=(Median_AUC+sd)), width = 0.3, position = position_dodge(0.9))+
    # ylim(ymin_,ymax_)+
    coord_cartesian(ylim=c(ymin_,ymax_))+
    theme_bw(base_size = 17, base_line_size = 1.1)+
    # theme_dark()+
    # theme_classic(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 20),legend.position="top")
  
  # dev.off()
}


plot_barplot_g_nosd = function( Dataset, ymin_ = 0, ymax_ = 1){
  # png(name, width = width, height = height, units='mm', res = 300)
  ggplot(Dataset, aes(fill=group, y=Median_AUC, x=N)) + 
    geom_bar(position="dodge", stat="identity")+
    labs(y= "Median AUC", fill = "")+ #, x = "x axis name")
    # geom_errorbar(aes(ymin=(Median_AUC-sd), ymax=(Median_AUC+sd)), width = 0.3, position = position_dodge(0.9))+
    # ylim(ymin_,ymax_)+
    coord_cartesian(ylim=c(ymin_,ymax_))+
    theme_bw(base_size = 17, base_line_size = 1.1)+
    # theme_dark()+
    # theme_classic(base_size = 17, base_line_size = 1.1)+
    theme(axis.text=element_text(face="bold"),
          axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 20),legend.position="top")
  
  # dev.off()
}



# Data importing ----------------------------------------------------------

set.seed(123)
setwd("C:/Users/fmelo/Desktop/Backup_Federico/Edge_mod_outlier/Simulations/Normality_simulations/Code/")


Results_all_methods200 <- read.csv("C:/Users/fmelo/Desktop/Backup_Federico/Edge_mod_outlier/Simulations/Normality_simulations/Data/Results_all_methods200.txt")

Res = Results_all_methods200[,2:ncol(Results_all_methods200)]
colnames(Res) = c("N_obs", "N_out", "k", "same_distrib", colnames(Res)[1:29])
head(Res)



# GLOBAL mean and median statistic ----------------------------------------



median_glob = apply(Res, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

# beware: not for every method each setting is available

setwd("C:/Users/fmelo/Desktop/Backup_Federico/Edge_mod_outlier/Simulations/Normality_simulations/Graphs/Grouped_by_n/Barplot/")
dd = data.frame(rbind(median_glob, mean_glob))
write.xlsx(rbind(median_glob, mean_glob), "AUC/SUMMARY_AUC.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

write.xlsx(cbind(median_glob, mean_glob), "AUC/SUMMARY_AUC_cbind.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



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
  group_by(N_obs) %>% 
  summarise_all(c("mean"),na.rm = TRUE)

Mean_obs =unlist(BY_n_obs%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))

mean_database = data.frame("N"=rep(BY_n_obs$N_obs, length(Mean_obs)/4), "Median_AUC" = Mean_obs, "group"=gsub(".[0-9]","",names(Mean_obs)))

BY_n_obs_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  group_by(N_obs) %>% 
  summarise_all(c("sd"),na.rm = TRUE)

SD_obs =unlist(BY_n_obs_sd%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))


global_database = cbind(mean_database, "sd"= SD_obs)
global_database$N = as.factor(global_database$N)
global_database$group = as.factor(global_database$group)
# Use geom_pointrange


ggplot(global_database[c(1:20,44:48),], aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))




# 1 plot: spoutlier with cosine and not and the median --------------------
single_cosine = c("auc.roc_OTS","auc.roc_spoutlier", "auc.roc_OTS_cosine")
Cosine_dataset = global_database[global_database$group %in% single_cosine,]
colnames(Cosine_dataset)
Cosine_dataset$group = ((as.character(Cosine_dataset$group)))
Cosine_dataset$group[Cosine_dataset$group == "auc.roc_OTS"] = "OTS euclidean"
Cosine_dataset$group[Cosine_dataset$group == "auc.roc_OTS_cosine"] = "OTS cosine"
Cosine_dataset$group[Cosine_dataset$group == "auc.roc_spoutlier"] = "Spoutlier"

(g = plot_barplot_g( Dataset = Cosine_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = Cosine_dataset))


png("barplot_single_COSINE.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_single_COSINE_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_single_COSINE_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()




# 2 plot: spoutlier with global and median  --------------------

global_cosine = c("auc.roc_OTS_glob_avg_OTS","auc.roc_OTS_euc_med", "auc.roc_OTS_cosine_med")
Cosine_avg_dataset = global_database[global_database$group %in% global_cosine,]
colnames(Cosine_avg_dataset)

Cosine_avg_dataset$group = ((as.character(Cosine_avg_dataset$group)))
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "Cosine OTS ensemble"
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_euc_med"] = "Euclidean OTS ensemble"
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_glob_avg_OTS"] = "Cosine + euclidean ensemble"


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



qplot1_sd = plot_barplot_g( Dataset = Cosine_dataset)
qplot1 = plot_barplot_g_nosd( Dataset = Cosine_dataset)
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
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
LOO_avg_dataset$group = ((as.character(LOO_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] = "SSN-m"
LOO_avg_dataset$group[LOO_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO"
LOO_avg_dataset$group[LOO_avg_dataset$group == "auc.roc_Loo"] = "LOO"
LOO_avg_dataset$group = as.factor(LOO_avg_dataset$group)

g = plot_barplot_g( Dataset = LOO_avg_dataset)
g_nosd = plot_barplot_g_nosd( Dataset = LOO_avg_dataset)


png("barplot_group_LOO_pvalue.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_group_LOO_pvalue_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_group_LOO_pvalue_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()





# 4 plot: LOO ; Cook's, kNN, optics  --------------------


METHODS_dataset = c("auc.roc_cook_med","auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
METHODS_avg_dataset = global_database[global_database$group %in% METHODS_dataset,]
colnames(METHODS_avg_dataset)
METHODS_avg_dataset$group = ((as.character(METHODS_avg_dataset$group)))
# METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_optics_k"] = "OPTICS"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_cook_med"] = "Cook's distance"
METHODS_avg_dataset$group = as.factor(METHODS_avg_dataset$group)


g = ggplot(METHODS_avg_dataset, aes(fill=group, y=Median_AUC, x=N)) + 
  geom_bar(position="dodge", stat="identity")+
  labs(y= "Median AUC", fill = "")+ #, x = "x axis name")
  geom_errorbar(aes(ymin=(Median_AUC-sd), ymax=(Median_AUC+sd)), width = 0.3, position = position_dodge(0.9))+
  ylim(0,1.05)+
  theme_bw(base_size = 17, base_line_size = 1.1)+
  # theme_dark()+
  # theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 20),legend.position="top")

## GO BEYOND 1
# (g = plot_barplot_g( Dataset = METHODS_avg_dataset))
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


ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_cosine_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database[global_database$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)

ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's distance"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "Cosine OTS median"


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


# SINGLE EDGE -------------------------------------------------------------

BY_n_obs_singlek = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( k < 3) %>%
  group_by(N_obs, k) %>%
  summarise_all(funs(mean(. )))

Mean_obs_single_k =unlist(BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))
Mean_obs_single_k =unlist(
  BY_n_obs_singlek%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size))
  
)

mean_database_single_k = data.frame("N"=rep(BY_n_obs_singlek$N_obs, length(Mean_obs_single_k)/4), "Median_AUC" = Mean_obs_single_k, "group"=gsub(".[0-9]","",names(Mean_obs_single_k)))

BY_n_obs_singlek_sd = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  filter( k < 3) %>%
  group_by(N_obs,k) %>% 
  summarise_all(c("sd"))

SD_obs_singlek =unlist(BY_n_obs_singlek_sd%>%select(-c(N_obs,k,same_distrib,N_out,Type,Size)))


global_database_singlek = cbind(mean_database_single_k, "sd"= SD_obs_singlek)
global_database_singlek$N = as.factor(global_database_singlek$N)
global_database_singlek$group = as.factor(global_database_singlek$group)
# Use geom_pointrange


ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_cosine_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database_singlek[global_database_singlek$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)

ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
# LOO_avg_dataset$group[LOO_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's distance"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "Cosine OTS median"


ALL_dataset = c("auc.roc_Loo","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_euc_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database_singlek[global_database_singlek$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)

ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo"] = "LOO"
# ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "Cosine OTS median"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_euc_med"] = "Euclidean OTS median"



(g = plot_barplot_g( Dataset = ALL_avg_dataset))
(g_nosd = plot_barplot_g_nosd( Dataset = ALL_avg_dataset))


png("barplot_ALL_method_SINGLE_EDGE.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_ALL_method_SINGLE_EDGE_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_ALL_method_SINGLE_EDGE_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()

(g = plot_barplot_g( Dataset = ALL_avg_dataset,0.4,0.7))
(g_nosd = plot_barplot_g_nosd( Dataset = ALL_avg_dataset,0.4,0.7))

png("barplot_ALL_method_shr_SINGLE_EDGE.png", width = 300, height = 300, units='mm', res = 300)
g
dev.off()
png("barplot_ALL_method_shr_SINGLE_EDGE_nosd.png", width = 300, height = 300, units='mm', res = 300)
g_nosd
dev.off()
png("barplot_ALL_method_shr_SINGLE_EDGE_nosd_diffsize.png", width = 465, height = 225, units='mm', res = 300)
g_nosd
dev.off()



#########################################----------------------########################################
#########################################----------------------########################################


#########################################----------------------########################################
#########################################----------------------########################################




BY_n_obs_n_out = tibble::as_tibble(my.df1) %>%
  mutate(Type=NA, Size=NA) %>%
  bind_rows(my.df1) %>%
  filter( k < 3) %>%
  group_by(N_obs, N_out, k) %>%
  summarise_all(funs(mean(. )))

# Only single edge: k = 2 --> grouped by N_obs and #outliers

# mean
BY_n_obs_n_out = tibble::as_tibble(my.df1) %>% 
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  filter( k < 3) %>%
  group_by(N_obs, N_out, k) %>% 
  summarise_all(c("mean")) %>% 
  round(.,3)


# median
BY_n_obs_n_out2 = tibble::as_tibble(my.df1) %>% 
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  filter( k < 3) %>%
  group_by(N_obs, N_out, k) %>% 
  summarise_all(c("median")) # %>% # here round does not work!
# round(.,3)



# grouped by K ------------------------------------------------------------
setwd("C:/Users/fmelo/Desktop/Backup_Federico/Edge_mod_outlier/Simulations/Normality_simulations/Results/")

average_BY_n_obs_n_out2 = tibble::as_tibble(my.df1) %>% 
  mutate(Type=NA, Size=NA) %>% 
  bind_rows(my.df1) %>% 
  # filter( k < 3) %>%
  group_by( k) %>%
  summarise_all(c("mean")) %>% 
  round(.,5)

write.xlsx(data.frame(average_BY_n_obs_n_out2), "AUC/mean_AUC_per_k.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


BY_n_obs_n_out2 = round(BY_n_obs_n_out2[,1:(ncol(BY_n_obs_n_out2)-2)],3)
write.xlsx(data.frame(BY_n_obs_n_out), "AUC/mean_AUC_single_edge.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(data.frame(BY_n_obs_n_out2), "AUC/median_AUC_median_single_edge.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



write.xlsx(data.frame(global_database), "AUC/AUC_per_sample_size_all_methods.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(data.frame(global_database_singlek), "AUC/AUC_per_sample_size_SINGLE_EDGE_all_methods.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



