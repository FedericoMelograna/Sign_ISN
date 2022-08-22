
result_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Results/AUC/"
data_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Data/Wrapped"
setwd(data_path)
# dir.create(result_path)


# GOAL doing some report on the HIGH-dimensional server experiment --------

# http://www.sthda.com/english/wiki/ggplot2-themes-and-background-colors-the-3-elements

# interesting themes
# 11/5

library(dplyr)
library("xlsx")
library(tidyverse)

# Data importing ----------------------------------------------------------



setwd(data_path)


Results_all_methods200 <- read.csv("Results_all_methods.txt")


Res = Results_all_methods200[,2:ncol(Results_all_methods200)]
colnames(Res) = c("N_obs", "N_out", "k", colnames(Res)[4:36])
head(Res)


setwd(result_path)

# GLOBAL mean and median statistic ----------------------------------------



median_glob = apply(Res, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

# beware: not for every method each setting is available

dd = data.frame(rbind(median_glob, mean_glob))
write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


Res_uniform = Res[Res$type_of_pareto_data == 0,]
Res_pareto1 = Res[Res$type_of_pareto_data == 1,]
Res_pareto2 = Res[Res$type_of_pareto_data == 2,]



median_glob = apply(Res_uniform, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_uniform, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

dd_uniform = data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_uniform.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


median_glob = apply(Res_pareto1, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_pareto1, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

dd_pareto1 = data.frame(rbind(median_glob, mean_glob))


write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_pareto_alph07.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

median_glob = apply(Res_pareto2, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_pareto2, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

dd_pareto2 = data.frame(rbind(median_glob, mean_glob))
write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_pareto_alph4.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

Res_mult1 = Res[Res$mult == 1.1,]
Res_mult15 = Res[Res$mult == 1.5,]
Res_mult2 = Res[Res$mult == 2,]

median_glob = apply(Res_mult1, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_mult1, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)
dd_Mult1= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_mult1_1.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

median_glob = apply(Res_mult15, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_mult15, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)
dd_Mult15= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_mult1_5.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

median_glob = apply(Res_mult2, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_mult2, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)
dd_Mult2= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_mult2_0.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)




# EXPLORING PERCENTAGES ---------------------------------------------------




Res_perc01 = Res[Res$perc_increase == 0.1,]
Res_perc025 = Res[Res$perc_increase == 0.25,]
Res_perc04 = Res[Res$perc_increase == 0.4,]

median_glob = apply(Res_perc01, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_perc01, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)
dd_perc01= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_perc0_1.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

median_glob = apply(Res_perc025, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_perc025, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)
dd_perc025= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_perc0_25.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

median_glob = apply(Res_perc04, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_perc04, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)
dd_perc04= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_perc0_4.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)



Res_single_edge = Res[Res$k == 2 & Res$type_of_pareto_data == 1 & Res$mult == 2,]
median_glob = apply(Res_single_edge, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_single_edge, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

dd_single_edge= data.frame(rbind(median_glob, mean_glob))

write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_single_edge_highmulti_pareto1.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

Res_mult15 = Res[Res$mult == 1.5,]
Res_mult2 = Res[Res$mult == 2,]





Res_nonsingle_edge = Res[Res$type_of_pareto_data == 1 & Res$mult == 2,]
median_glob = apply(Res_nonsingle_edge, 2, function(x) median(x, na.rm = T)) 
median_glob = round(median_glob,3)
mean_glob = apply(Res_nonsingle_edge, 2, function(x) mean(x, na.rm = T)) 
mean_glob = round(mean_glob,3)

dd_nonsingle_edge= data.frame(rbind(median_glob, mean_glob))


write.xlsx(rbind(median_glob, mean_glob), "SUMMARY_AUC_highmulti_pareto1.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


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
