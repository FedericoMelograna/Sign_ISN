
data_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Data/Raw"
result_path = "C:/Users/fmelo/Documents/GitHub/Sign_ISN/Simulations/Microbiome_simulations/Data/Wrapped"

dir.create(result_path)


setwd(data_path)


# setwd("C:/Users/fmelo/Desktop/Backup_Federico/High_dimensional_gaussian_sampling/Results_multivariate_module_detection/Results_and_analysis_server/Results_and_analysis_dirichlet_microbiome_samples/")


array_res = readRDS(file = "Not_median_result_final_all_iteration.rda")
confidence_interval = readRDS(file = "Not_median_Confidence_interval_all_methods_all_iteration.rda")
optimal_cutoff = readRDS(file = "Not_median_optimal_THRE_all_methods_all_iteration.rda")

nsamples <- 5000 # number of sample analyzed for a single individual

# PARAMETER non-dirichlet
ns = c(100,500,1000)
ms = c(1,5,10)
dims = c(2,5,11,17)
out_gene_same_distrib = c(TRUE, FALSE)

# DIRICHLET parameters
perc_increase = c(0.1,0.25,0.4) ## there are no infor on these percentages
mult=c(1.1,1.5,2)
intensity_parameter = c(3)  
type_of_pareto_data = c(0,1,2)

full_GR = expand.grid(n =ns,m = ms,dim = dims, type_of_pareto_data = type_of_pareto_data,
                      perc_increase = perc_increase, mult = mult, intensity_parameter = intensity_parameter)


median_results = apply(array_res, MARGIN = c(1,2), function(x) median(x,na.rm = TRUE) )
mean_setting = apply(median_results,1, function(x) {mean(x, na.rm = T)})
plot(mean_setting)
results_multi = cbind(full_GR, median_results)
colnames(results_multi) =   c(colnames(full_GR),"auc(roc_Loo_multi)","auc(roc_Loo)","auc(roc_KNN_log_N_P)","auc(roc_KNN_log_N_P_diff_net)","auc(roc_KNN_5_sqrt_N)",
                              "auc(roc_KNN_5_sqrt_N_diff_net)","auc(roc_optics_k_mean)","auc(roc_optics_k_mean_diff_net)",
                              "auc(roc_optics_k_5)","auc(roc_optics_k_5_diff_net)","auc(roc_optics_sqrt_n)","auc(roc_optics_sqrt_n_diff_net)", 
                              "auc(roc_OTS)","auc(roc_OTS_diff_net)","auc(roc_OTS_cosine)","auc(roc_OTS_diff_net_cosine)",
                              "auc(roc_OTS_cosine_med)","auc(roc_OTS_euc_med)","auc(roc_OTS_glob_avg_OTS)",
                              "auc(roc_spoutlier)","auc(roc_spoutlier_diff_net)","auc(roc_spoutlier_diff_net_norm)","auc_DPCC",
                              "auc(roc_cook_max)","auc(roc_cook_max_diff_net)","auc(roc_cook_avg)","auc(roc_cook_avg_diff_net)",
                              "auc(roc_cook_med)","auc(roc_cook_med_diff_net)")




median_results_CI = apply(confidence_interval, MARGIN = c(1,2), function(x) median(x,na.rm = TRUE) )
results_multi_CI = cbind(full_GR, median_results_CI)
high_low = c("low", "high")
names_rocs = c("roc_Loo_multi","roc_Loo","roc_KNN_log_N_P","roc_KNN_log_N_P_diff_net","roc_KNN_5_sqrt_N",
               "roc_KNN_5_sqrt_N_diff_net","roc_optics_k_mean","roc_optics_k_mean_diff_net",
               "roc_optics_k_5","roc_optics_k_5_diff_net","roc_optics_sqrt_n","roc_optics_sqrt_n_diff_net", 
               "roc_OTS","roc_OTS_diff_net","roc_OTS_cosine","roc_OTS_diff_net_cosine",
               "roc_OTS_cosine_med","roc_OTS_euc_med","roc_OTS_glob_avg_OTS",
               "roc_spoutlier","roc_spoutlier_diff_net","roc_spoutlier_diff_net_norm","roc_DPCC",
               "roc_cook_max","roc_cook_max_diff_net","roc_cook_avg","roc_cook_avg_diff_net",
               "roc_cook_med","roc_cook_med_diff_net" )
# ll = levels(interaction(names_rocs,high_low,sep='_'))
## n.b there is a mistake into the names_rocs in the original file in the runs
ll = paste(rep(names_rocs, each = 2),rep(high_low, length(names_rocs)), sep = "_" )
# ll_real = sort(ll)


colnames(results_multi_CI) =   c(colnames(full_GR), ll)

median_results_OPT = apply(optimal_cutoff, MARGIN = c(1,2), function(x) median(x,na.rm = TRUE) )
results_multi_OPT = cbind(full_GR, median_results_OPT)
colnames(results_multi_OPT) =   c(colnames(full_GR),"OPT_roc_Loo_multi","OPT_roc_Loo","OPT_roc_KNN_log_N_P","OPT_roc_KNN_log_N_P_diff_net","OPT_roc_KNN_5_sqrt_N",
                                  "OPT_roc_KNN_5_sqrt_N_diff_net","OPT_roc_optics_k_mean","OPT_roc_optics_k_mean_diff_net",
                                  "OPT_roc_optics_k_5","OPT_roc_optics_k_5_diff_net","OPT_roc_optics_sqrt_n","OPT_roc_optics_sqrt_n_diff_net", 
                                  "OPT_roc_OTS","OPT_roc_OTS_diff_net","OPT_roc_OTS_cosine","OPT_roc_OTS_diff_net_cosine",
                                  "OPT_roc_OTS_cosine_med","OPT_roc_OTS_euc_med","OPT_roc_OTS_glob_avg_OTS",
                                  "OPT_roc_spoutlier","OPT_roc_spoutlier_diff_net","OPT_roc_spoutlier_diff_net_norm","OPT_DPCC",
                                  "OPT_roc_cook_max","OPT_roc_cook_max_diff_net","OPT_roc_cook_avg","OPT_roc_cook_avg_diff_net",
                                  "OPT_roc_cook_med","OPT_roc_cook_med_diff_net")



getwd()

setwd(result_path)



# WRITING -----------------------------------------------------------------

# MEDIAN AUC  
aa=  paste0("Results_all_methods.txt")  
write.csv(results_multi,aa)
print(dim(array_res))

# MEDIAN CI
cc = paste0("Confidence_interval_all_methods.txt")
write.csv(results_multi_CI,cc)  

## MEDIAN optimal thre
ee = paste0("optimal_THRE_all_methods.txt")
write.csv(results_multi_OPT,ee)  



