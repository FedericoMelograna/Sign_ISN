library(abind)


data_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Data/"
result_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Data/Raw"


setwd(data_path)

# setwd("C:/Users/fmelo/Desktop/Backup_Federico/High_dimensional_gaussian_sampling/Results_multivariate_module_detection/Results_and_analysis_server/Results_and_analysis_dirichlet_microbiome_samples/")


direc =  list.dirs(path=".", full.names=TRUE, recursive=FALSE)
direc = direc[grepl("Run", direc, fixed=TRUE)]
directories_no_point = gsub("./", "",direc)
array_res <- array(rep(NA, 972*29*1), dim=c(972,29,1))
optimal_cutoff <- array(rep(NA, 972*29*1), dim=c(972,29,1))
confidence_interval <- array(rep(NA, 972*29*2*1), dim=c(972,29*2,1))
curr_wd = getwd()
for (dir in directories_no_point ){
  setwd(dir)
  result <- list.files(path=".", pattern="Not_median_res*.", 
                       full.names=TRUE, recursive=FALSE)
  conf_int <- list.files(path=".", pattern="Not_median_Conf*.", 
                         full.names=TRUE, recursive=FALSE)
  opt_thr <- list.files(path=".", pattern="Not_median_opt*.", 
                        full.names=TRUE, recursive=FALSE)
  
  last_result = result[sapply(result, file.size) == max(sapply(result, file.size))]
  last_conf_int = conf_int[sapply(conf_int, file.size) == max(sapply(conf_int, file.size))]
  last_opt_thr = opt_thr[sapply(opt_thr, file.size) == max(sapply(opt_thr, file.size))]
  
  Result = readRDS(last_result)
  Conf_int = readRDS(last_conf_int)
  Opt_thr = readRDS(last_opt_thr)
  array_res = abind(array_res, Result, along = 3)
  confidence_interval = abind(confidence_interval, Conf_int, along = 3)
  optimal_cutoff = abind(optimal_cutoff, Opt_thr, along = 3)
  
  setwd(curr_wd)
}

dim(array_res)

array_res = array(array_res, dim = dim(array_res))
confidence_interval = array(confidence_interval, dim = dim(confidence_interval))
optimal_cutoff = array(optimal_cutoff, dim = dim(optimal_cutoff))
# FIRST WAS THE NA
array_res = array_res[,,-1]
confidence_interval = confidence_interval[,,-1]
optimal_cutoff = optimal_cutoff


# save --------------------------------------------------------------------



setwd(result_path)

bb =  paste0("Not_median_result_final_all_iteration.rda") 
saveRDS(array_res, file = bb)
## txt of the rds ?? of every run of the optimal threhsold
dd = paste0("Not_median_Confidence_interval_all_methods_all_iteration.rda")
saveRDS(confidence_interval,dd)

ff = paste0("Not_median_optimal_THRE_all_methods_all_iteration.rda")
saveRDS(optimal_cutoff,ff)
## txt of the rds ?? of every run of the confidence interval
