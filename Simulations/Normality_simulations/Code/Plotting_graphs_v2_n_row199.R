
# GOAL doing some report on the HIGH-dimensional server experiment --------


# 11/5

library(dplyr)
library("xlsx")
library(tidyverse)

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

setwd("C:/Users/fmelo/Desktop/Backup_Federico/Edge_mod_outlier/Simulations/Normality_simulations/Graphs/Grouped_by_n/")
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

# Data preparation
library("tidyverse")







ggplot(BY_n_obs, aes(N_obs)) + 
  geom_line(aes(y = auc.roc_cook_max_diff_net._mean, colour = "var0")) + 
  geom_pointrange(aes(ymin=auc.roc_cook_max_diff_net._mean-auc.roc_cook_max_diff_net._sd, ymax=auc.roc_cook_max_diff_net._mean+auc.roc_cook_max_diff_net._sd))

  geom_line(aes(y = auc_DPCC_mean, colour = "var1"))

  
  
ggplot(BY_n_obs, aes(x=N_obs, y = auc.roc_cook_max_diff_net._mean )) + 
    geom_pointrange(aes(ymin=auc.roc_cook_max_diff_net._mean-auc.roc_cook_max_diff_net._sd, ymax=auc.roc_cook_max_diff_net._mean+auc.roc_cook_max_diff_net._sd))
  
  geom_line(aes(y = auc_DPCC_mean, colour = "var1"))

  
  
  
    
  
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

Cosine_dataset$N = (as.numeric(as.character(Cosine_dataset$N)) + rep(c(1,11,21), each = 4))   

png("graphs_single_COSINE.png", width = 465, height = 225, units='mm', res = 300)
ggplot(Cosine_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier single shot")+ #,"This is the graph for the spoutlier single methods") +
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
                           axis.title=element_text(face="bold"), title = element_text(face = "bold",))

dev.off()

Cosine_dataset$N = (as.numeric(as.character(Cosine_dataset$N)) + rep(c(1,11,21), each = 4))   

png("graphs_single_COSINE_nosd.png", width = 465, height = 225, units='mm', res = 300)
ggplot(Cosine_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  ylim(0.6,0.85)+
  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier single shot")+ #,"This is the graph for the spoutlier single methods") +
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

### 199
dev.off()
### SAVE HERE 
png("graphs_single_COSINE_nosd_diffsize.png", width = 300, height = 300, units='mm', res = 300)
ggplot(Cosine_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC", color = "")+ #, x = "x axis name") +
  geom_line(size=1.5)+
  ylim(0.6,0.85)+
  geom_point(aes(stroke = 1.2),size = 6,show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  # ggtitle("Spoutlier single shot")+ #,"This is the graph for the spoutlier single methods") +
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",),legend.key.size = unit(1, 'cm'),text = element_text(size = 20))


dev.off()


# 2 plot: spoutlier with global and median  --------------------

global_cosine = c("auc.roc_OTS_glob_avg_OTS","auc.roc_OTS_euc_med", "auc.roc_OTS_cosine_med")
Cosine_avg_dataset = global_database[global_database$group %in% global_cosine,]
colnames(Cosine_avg_dataset)

Cosine_avg_dataset$group = ((as.character(Cosine_avg_dataset$group)))
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "Cosine OTS ensemble"
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_euc_med"] = "Euclidean OTS ensemble"
Cosine_avg_dataset$group[Cosine_avg_dataset$group == "auc.roc_OTS_glob_avg_OTS"] = "Cosine + euclidean ensemble"


Cosine_avg_dataset$N = (as.numeric(as.character(Cosine_avg_dataset$N)) + rep(c(1,11,21), each = 4))   
png("graphs_group_COSINE.png", width = 465, height = 225, units='mm', res = 300)

ggplot(Cosine_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier ensemble methods")+#,"This is the graph for the spoutlier ensemble methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

dev.off()


png("graphs_group_COSINE_nosd.png", width = 465, height = 225, units='mm', res = 300)

ggplot(Cosine_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line(aes())+
  ylim(0.6,0.9)+
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier ensemble methods")+#,"This is the graph for the spoutlier ensemble methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

dev.off()


png("graphs_group_COSINE_nosd_diffsize.png", width = 300, height = 300, units='mm', res = 300)

ggplot(Cosine_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line(aes())+
  ylim(0.6,0.9)+
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier ensemble methods")+#,"This is the graph for the spoutlier ensemble methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

dev.off()


require(gridExtra)
qplot1 = ggplot(Cosine_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  coord_cartesian(ylim = c(0.6, 0.95)) +
  labs(y= "Median AUC")+ #, x = "x axis name") +
  
  geom_point(aes(size = 2,stroke = 2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier single shot")+ #,"This is the graph for the spoutlier single methods") +
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

qplot2 = ggplot(Cosine_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  coord_cartesian(ylim = c(0.6, 0.95)) +
  labs(y= "Median AUC")+ #, x = "x axis name") +
  
  geom_point(aes(size = 2,stroke = 2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Spoutlier ensemble methods")+#,"This is the graph for the spoutlier ensemble methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))

grid.arrange(qplot1,qplot2)
grid.arrange(qplot1,qplot2, ncol = 2)
png("graphs_COSINE_nosd_GROUP_AND_SINGLE.png", width = 465, height = 225, units='mm', res = 300)

grid.arrange(qplot1,qplot2)

dev.off()

png("graphs_COSINE_nosd_GROUP_AND_SINGLE_diffsize.png", width = 300, height = 300, units='mm', res = 300)

grid.arrange(qplot1,qplot2)

dev.off()

png("graphs_COSINE_nosd_GROUP_AND_SINGLE_diff_shape.png", width = 465, height = 225, units='mm', res = 300)

grid.arrange(qplot1,qplot2)

dev.off()


png("graphs_COSINE_nosd_GROUP_AND_SINGLE_vert.png", width = 600, height = 300, units='mm', res = 300)

grid.arrange(qplot1,qplot2, ncol = 2)

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

LOO_avg_dataset$N = (as.numeric(as.character(LOO_avg_dataset$N)) + rep(c(1,11,21), each = 4))   
png("graphs_group_LOO_pvalue.png", width = 465, height = 225, units='mm', res = 300)

ggplot(LOO_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("P-value methods")+# ,"This is the graph for the p-value yielding methods")+ theme_classic()
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()

png("graphs_group_LOO_pvalue_nosd.png", width = 465, height = 225, units='mm', res = 300)


ggplot(LOO_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  labs(y= "Median AUC")+ #, x = "x axis name") +
  
  coord_cartesian(ylim = c(0.55, 0.7))+
  # scale_fill_discrete( labels = c("A", "B", "C"))+
  # scale_colour_manual(values = c('Avg' = 'red', 'Individual Runs' = 'grey'))+

  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("P-value methods")+# ,"This is the graph for the p-value yielding methods")+ theme_classic()
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()


png("graphs_group_LOO_pvalue_nosd_diffsize.png", width = 300, height = 300, units='mm', res = 300)


ggplot(LOO_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  geom_line()+
  labs(y= "Median AUC")+ #, x = "x axis name") +
  
  coord_cartesian(ylim = c(0.55, 0.7))+
  # scale_fill_discrete( labels = c("A", "B", "C"))+
  # scale_colour_manual(values = c('Avg' = 'red', 'Individual Runs' = 'grey'))+
  
  geom_point(aes(size = 0.2,stroke = 1.2),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("P-value methods")+# ,"This is the graph for the p-value yielding methods")+ theme_classic()
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()



# 4 plot: LOO ; Cook's, kNN, optics  --------------------
Mean_obs
mean_glob


METHODS_dataset = c("auc.roc_cook_med","auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
METHODS_avg_dataset = global_database[global_database$group %in% METHODS_dataset,]
colnames(METHODS_avg_dataset)
METHODS_avg_dataset$group = ((as.character(METHODS_avg_dataset$group)))
# METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc_DPC"] == as.factor("auc.roc_SSN")
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_optics_k"] = "OPTICS"
METHODS_avg_dataset$group[METHODS_avg_dataset$group == "auc.roc_cook_med"] = "Cook's distance"
METHODS_avg_dataset$group = as.factor(METHODS_avg_dataset$group)




METHODS_avg_dataset$N = (as.numeric(as.character(METHODS_avg_dataset$N)) + rep(c(1,11,21), each = 4))   
png("graphs_group_methods.png", width = 465, height = 225, units='mm', res = 300)

ggplot(METHODS_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Miscellanous methods") +# ,"This is the graph for the generic methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()

METHODS_avg_dataset$N = (as.numeric(as.character(METHODS_avg_dataset$N)) + rep(c(1,11,21), each = 4))   


png("graphs_group_methods_nosd.png", width = 465, height = 225, units='mm', res = 300)

ggplot(METHODS_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  ylim(0.5,0.9)+
  geom_point(aes(size = 0.2,stroke = 1.5),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Miscellanous methods") +# ,"This is the graph for the generic methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()

png("graphs_group_methods_nosd_diffsize.png", width = 300, height = 300, units='mm', res = 300)

ggplot(METHODS_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  ylim(0.5,0.9)+
  geom_point(aes(size = 0.2,stroke = 1.5),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Miscellanous methods") +# ,"This is the graph for the generic methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()



# 5 plot: LOO ; best_plots  --------------------
Mean_obs
mean_glob


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


auc.roc_optics_k
# ALL_avg_dataset = ALL_avg_dataset[-9,]

ALL_avg_dataset$N = (as.numeric(as.character(ALL_avg_dataset$N)) + rep(c(1,11,21,31,41,51), each = 4))   
png("graphs_ALL_methods.png", width = 465, height = 225, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best methods")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()

METHODS_avg_dataset$N = (as.numeric(as.character(METHODS_avg_dataset$N)) + rep(c(1,11,21), each = 4))   


png("graphs_ALL_methods_nosd.png", width = 465, height = 225, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  coord_cartesian(ylim = c(0.5, 0.9))+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best methods")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()

png("graphs_ALL_methods_nosd_diffsize.png", width = 300, height = 300, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  coord_cartesian(ylim = c(0.5, 0.9))+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best methods")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()




png("graphs_ALL_methods_diff_SD.png", width = 465, height = 225, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best methods")+#,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
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



ALL_dataset = c("auc.roc_cook_med","auc_DPC", "auc.roc_Loo_multi","auc.roc_OTS_euc_med", "auc.roc_optics_k", "auc.roc_KNN_sqrt_N")
ALL_avg_dataset = global_database_singlek[global_database_singlek$group %in% ALL_dataset,]
colnames(ALL_avg_dataset)

ALL_avg_dataset$group = ((as.character(ALL_avg_dataset$group)))

ALL_avg_dataset$group[ALL_avg_dataset$group == "auc_DPC"] = "SSN-m"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_Loo_multi"] = "MultiLOO"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_KNN_sqrt_N"] = "kNN"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_optics_k"] = "Optics"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_cook_med"] = "Cook's distance"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_cosine_med"] = "Cosine OTS median"
ALL_avg_dataset$group[ALL_avg_dataset$group == "auc.roc_OTS_euc_med"] = "Euclidean OTS median"

ALL_avg_dataset = ALL_avg_dataset[-9,]
ALL_avg_dataset$N = (as.numeric(as.character(ALL_avg_dataset$N)) + rep(c(1,11,21,31,41,51), each = 4)[-1])
png("graphs_ALL_methods_SINGLE_EDGE.png", width = 465, height = 225, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  coord_cartesian(ylim = c(0.55, 0.7))+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best method SINGLE_EDGE") + #,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()

png("graphs_ALL_methods_SINGLE_EDGE_nosd.png", width = 465, height = 225, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  coord_cartesian(ylim = c(0.55, 0.64))+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best method SINGLE EDGE") + #,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()


png("graphs_ALL_methods_SINGLE_EDGE_nosd_diffsize.png", width = 300, height = 300, units='mm', res = 300)

ggplot(ALL_avg_dataset, aes(x=N, y=Median_AUC, group=group, color=group)) + 
  labs(y= "Median AUC")+ #, x = "x axis name") +
  geom_line()+
  coord_cartesian(ylim = c(0.55, 0.64))+
  geom_point(aes(size = 0.2,stroke = 1),show.legend = FALSE) +
  # geom_pointrange(aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)))+
  # geom_errorbar( mapping= aes(ymin=as.numeric(Median_AUC-sd), ymax=as.numeric(Median_AUC+sd)), width=0.2, size=1)+
  ggtitle("Best method SINGLE EDGE") + #,"This is the graph for the best methods")+
  theme_classic(base_size = 17, base_line_size = 1.1)+
  theme(axis.text=element_text(face="bold"),
        axis.title=element_text(face="bold"), title = element_text(face = "bold",))
dev.off()















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



