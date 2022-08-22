

# SIMULATION microbiome data  ---------------------------------------------



library(gtools)
library(Pareto)
library(data.table)
# install.packages("splitstackshape")
library(splitstackshape)
library(OptimalCutpoints)




# ALL the possible dataset created ----------------------------------------

## PARAMETERS

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

# 
# d0 = rep(1/k,k)
# d1 = rPareto(k,1,0.7)
# #1 parameter of location --> the minimum 
# 
# d2 = rPareto(k,1,4) # --> 4 alpha parameter of dispersion alpha >> --> dispersion less

# dims = c(2,3)
# ms = c(1,5)
# ns = c(100)

# Create grid

full_GR = expand.grid(n =ns,m = ms,dim = dims, type_of_pareto_data = type_of_pareto_data,
                      perc_increase = perc_increase, mult = mult, intensity_parameter = intensity_parameter)


## we did not use all the right parameters as before, because there are too many parameters and would be too much
nass = rep(NA,nrow(full_GR))

# auc_list = data.frame(roc_Loo = nass, roc_KNN = nass, roc_optics = nass, roc_OTS = nass)
rep = 10000
# create the reference distribution with 10 000 repetition. 


set.seed(111)

# how many repetition for each grid 

num_rep = 100

# Crete empry container for the resutls

array_res <- array(rep(NA, nrow(full_GR)*29*num_rep), dim=c(nrow(full_GR),29,num_rep))
optimal_cutoff <- array(rep(NA, nrow(full_GR)*29*num_rep), dim=c(nrow(full_GR),29,num_rep))
confidence_interval <- array(rep(NA, nrow(full_GR)*29*2*num_rep), dim=c(nrow(full_GR),29*2,num_rep))



dim(array_res)


# for (j in 1:num_rep){

j = 1
  ## for each repetition
i = 1
# n m dim type_of_pareto_data perc_increase mult intensity_parameter
# 12 1000 1   5                   0           0.1  1.1                   3
# for (i in 1:nrow(full_GR)){
    
    # for each entry in the repetition matrix 
    
    # Create data with DIRICHLET
    
    element = full_GR[i,]
    print(element)
    multiplier = rep(1,element$dim)
    multiplier[1:(round( element$perc_increase* element$dim)+1)] = element$mult
    multiplier = sample(multiplier)
    
    # d0 = rep(1/k,k)
    # d1 = rPareto(k,1,0.7)
    # #1 parameter of location --> the minimum 
    # 
    # d2 = rPareto(k,1,4) # --> 4 alpha parameter of dispersion alpha >> --> dispersion less
    # 
    
    # N.b. we do not sample a module with the dimension K as the one we want to test: 
    # We sample a K whihc is 10 time this dimension in order to account for the compositionality of the data
    # and the impossibility of creating data with only single edge correlation difference without generating a full module
    k = element$dim * 10
    
    if (element$type_of_pareto_data == 0){
      used_d_data = rep(1,k)
      
    } else if(element$type_of_pareto_data == 1) {
      used_d_data = rPareto(k,1,0.7)
    } else {
      used_d_data = rPareto(k,1,4)
    }
    
    ## The Pareto sampled data [USED as parameter in a dirichlet] can be 
    ## a uniform [1,1,...1]
    ## or a rPareto with different sparsity parameter 
    
    
    ## Only a percentage (multiplier ) of the entries are inflated when passing from control to case
    ## The others are multiplied by 0. 
    mm = rep(1, k-length(multiplier))
    mm[1:(round( element$perc_increase*  k-length(multiplier)))] = element$mult
    mm = sample(mm)
    
    
    
    used_e_data = used_d_data * c(multiplier,mm)
    used_e_data = used_e_data * (sum(used_d_data) / sum(used_e_data))
    used_d_data = used_d_data * element$intensity_parameter  
    used_e_data = used_e_data * element$intensity_parameter 
    
    
    # PROBLEMA ----------------------------------------------------------------
    
    # dovremmo riuscire a fare ogni individuo indiivudo con un for rdir e rmultim
    control <- matrix(0, ncol = k, nrow = (element$n - element$m) )
    
    case <- matrix(0, ncol = k, nrow = element$m)
    
    #Assemblage
    n_obs = element$n
    n_out = element$m
    # question: WHY I SAMPL FROM A RDIRICHTLET sample??
    for (t in 1:(n_obs - n_out)) {
      
      # Extractinf from a dirichlet (baseded on the PARETO)
      d_data_dr = rdirichlet(1,used_d_data )
      
      # Input for a multinomial extraction
      control[t,] = rmultinom(1, nsamples, prob = d_data_dr)
      
    }
    
    for (t in (1 + n_obs - n_out):n_obs) {
      
      # Data inflated by multiplication for the case samppling
      e_data_dr = rdirichlet(1,used_e_data )
      case[t-(n_obs-n_out),] = rmultinom(1, nsamples, prob = e_data_dr)
    }
    
    
    full_data = rbind(control, case)
    
    # The data used
    # write.csv(full_data, file = "Current_data.csv")
    # apply(full_data,2,mean) == 0
    # SCALING the data --------------------------------------------------------
    
    
    scale_full = scale(full_data) # / nsamples
    ## N.B. THE DATA ARE SCALED --> CENT = t, scale = T
    if(sum(is.na(scale_full)) > 0)
    {
      # sometime theere is too less variability and with the scaling NA is generating: 
      # In that case the iteration is skipped
      print("NA generated")
      next; 
    }
    
    # a vector stating is outlier or not 
    is_out =  c(rep(0,element$n-element$m), rep(1,element$m))
    
    # Take the only dimension element of our module 
    sampled = scale_full[, 1: element$dim]
    
    # Individual Netowrk
    Tot_net = indNet_function(sampled)
    IndNet = Tot_net[[1]]
    DiffNet = Tot_net[[2]]
    
    # transpose single edge network for a problem in shaping
    if (dim(IndNet)[1] == 1){
      
      IndNet = t(IndNet)
      colnames(IndNet) = "Gene1_Gene2"
      DiffNet = t(DiffNet)
      colnames(DiffNet) = "Gene1_Gene2"
    }
    
    
    
    ########### LOO methods ----------------------------------
    if(sum(is.na(IndNet)) > 0){
      print("NA generated IndNet")
      next; 
    }
    
    # Evaluate the outlier with the LOO_multi method
    start.time <- Sys.time()
    list_results_LOO_MULTI = evaluate_LOO_multivariate_threhsold(element$n,element$m,rep, sampled, element$dim) # rep = #repetition creating a background network
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 5.988337 secs

    
    min_p_for_pers = apply(list_results_LOO_MULTI$p_values,MARGIN =  1, function(x) min(x) )
    # Take the minimum p-value
    
    
    real_min_p = ifelse(min_p_for_pers > 0.5,1-min_p_for_pers, min_p_for_pers ) # Since p-value is both sides, we take the minimum e.g. percentile 0.999 --> p-value 0.001
    temp_data = data.frame(is_out = is_out, real_min_p = real_min_p) # real vs predicted
    
    # Calculate the AUC cutoff --> optimal cutoff ; confidence interval and AUC
    result_LOO_multi = calculating_AUC_cutoff(temp_data, direction = ">")
    roc_Loo_multi<- result_LOO_multi["AUC"]
    CI_roc_Loo_multi <- c(result_LOO_multi["ll"], result_LOO_multi["ul"])
    OPT_roc_Loo_multi<- result_LOO_multi["OPT_threshold"]
    
    
    # LOO method 
    p = ncol(IndNet)
    n = nrow(IndNet)
    start.time <- Sys.time()
    Loo = evaluate_LOO(element$n,element$m,rep,sampled) 
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 5.191106  secs
    
    # Return a vector [0,0,1,.. 0] of prediction (with 1 if the summed edges  is over the threhsold
    # 0 otherwise
    # both edges and threhsold are founded in a module as the sum of the absolute univariate difference 
    # sum(abs(real_cor_ - cor_perturbed_ ) ) / 2
    
    
    temp_data = data.frame(is_out = is_out, Loo = Loo)
    
    result_LOO = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_Loo<- result_LOO["AUC"]
    CI_roc_Loo <- c(result_LOO["ll"], result_LOO["ul"])
    OPT_roc_Loo<- result_LOO["OPT_threshold"]
    
    
    ########### kNN methods ----------------------------------
    
    
    # TRIALS 
    # kimn = min(log(n),p+1) ; kmax =  max(log(n),p+1)
    # print(min(log(n),p+1))
    # print(max(log(n),p+1))
    
    # Knn evaluation with different NETWORK --> IndNet or DiffNet 
    start.time <- Sys.time()
    knn.agg_log_N_P          = evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(log(n),p+1), k_max =  max(log(n),min(p+1,50)) )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.08776283 secs
    
    start.time <- Sys.time()
    knn.agg_log_N_P_diff_net = evalute_knn(is_out = is_out, IndNet = DiffNet,k_min =  min(log(n),p+1), k_max =  max(log(n),min(p+1,50)) )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.06981206  secs
    
    
    # Result IndNet
    
    temp_data = data.frame(is_out = is_out, knn.agg_log_N_P = knn.agg_log_N_P)
    result_KNN_log_N_P = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_KNN_log_N_P<- result_KNN_log_N_P["AUC"]
    CI_roc_KNN_log_N_P <- c(result_KNN_log_N_P["ll"], result_KNN_log_N_P["ul"])
    OPT_roc_KNN_log_N_P<- result_KNN_log_N_P["OPT_threshold"]
    
    # Result LooNet
    
    temp_data = data.frame(is_out = is_out, knn.agg_log_N_P_diff_net = knn.agg_log_N_P_diff_net)
    result_KNN_log_N_P_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_KNN_log_N_P_diff_net<- result_KNN_log_N_P_diff_net["AUC"]
    CI_roc_KNN_log_N_P_diff_net <- c(result_KNN_log_N_P_diff_net["ll"], result_KNN_log_N_P_diff_net["ul"])
    OPT_roc_KNN_log_N_P_diff_net<- result_KNN_log_N_P_diff_net["OPT_threshold"]
    
    # Different parameters k min and k max 
    
    # print( min(sqrt(n),5)) 
    # print( max(sqrt(n),5))
    start.time <- Sys.time()
    knn.agg_5_sqrt_N =          evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(sqrt(n),5), k_max =  max(sqrt(n),5)  )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.1326451   secs
    
    start.time <- Sys.time()
    knn.agg_5_sqrt_N_diff_net = evalute_knn(is_out = is_out, IndNet = DiffNet,k_min =  min(sqrt(n),5), k_max =  max(sqrt(n),5)  )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.153589    secs
    
    # Result IndNet
    
    temp_data = data.frame(is_out = is_out, knn.agg_5_sqrt_N = knn.agg_5_sqrt_N)
    result_KNN_5_sqrt_N  = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_KNN_5_sqrt_N <- result_KNN_5_sqrt_N["AUC"]
    CI_roc_KNN_5_sqrt_N  <- c(result_KNN_5_sqrt_N["ll"], result_KNN_5_sqrt_N["ul"])
    OPT_roc_KNN_5_sqrt_N <- result_KNN_5_sqrt_N["OPT_threshold"]
    
    # Result LooNet
    
    temp_data = data.frame(is_out = is_out, knn.agg_5_sqrt_N_diff_net = knn.agg_5_sqrt_N_diff_net)
    result_KNN_5_sqrt_N_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_KNN_5_sqrt_N_diff_net<- result_KNN_5_sqrt_N_diff_net["AUC"]
    CI_roc_KNN_5_sqrt_N_diff_net <- c(result_KNN_5_sqrt_N_diff_net["ll"], result_KNN_5_sqrt_N_diff_net["ul"])
    OPT_roc_KNN_5_sqrt_N_diff_net <- result_KNN_5_sqrt_N_diff_net["OPT_threshold"]
    
    
    
    ############ OPTICS methods -------------------------
    
    
    # OPTICS evaluation with different NETWORK --> IndNet or DiffNet 
    start.time <- Sys.time()
    optics_res_k_mean  = evaluate_optics(X = IndNet, k = mean(c(p+1,log(n))) )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.116688     secs
    
    
    start.time <- Sys.time()
    optics_res_k_mean_diff_net = evaluate_optics(X = DiffNet, k = mean(c(p+1,log(n))) )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.124707 secs
    
    # Result IndNet
    
    temp_data = data.frame(is_out = is_out, optics_res_k_mean = optics_res_k_mean)
    result_optics_k_mean = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_optics_k_mean<- result_optics_k_mean["AUC"]
    CI_roc_optics_k_mean <- c(result_optics_k_mean["ll"], result_optics_k_mean["ul"])
    OPT_roc_optics_k_mean <- result_optics_k_mean["OPT_threshold"]
    
    
    # Result LooNet
    
    temp_data = data.frame(is_out = is_out, optics_res_k_mean_diff_net = optics_res_k_mean_diff_net)
    result_optics_k_mean_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_optics_k_mean_diff_net<- result_optics_k_mean_diff_net["AUC"]
    CI_roc_optics_k_mean_diff_net <- c(result_optics_k_mean_diff_net["ll"], result_optics_k_mean_diff_net["ul"])
    OPT_roc_optics_k_mean_diff_net <- result_optics_k_mean_diff_net["OPT_threshold"]
    
    
    # Different parameter k --> min number of neighbors
    start.time <- Sys.time()
    optics_res_k_5          = evaluate_optics(X = IndNet, k = 5 )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.1186819  secs
    
    start.time <- Sys.time()
    optics_res_k_5_diff_net = evaluate_optics(X = DiffNet, k = 5 )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.1227062   secs
    
    # Result IndNet
    
    temp_data = data.frame(is_out = is_out, optics_res_k_5 = optics_res_k_5)
    result_optics_k_5 = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_optics_k_5<- result_optics_k_5["AUC"]
    CI_roc_optics_k_5 <- c(result_optics_k_5["ll"], result_optics_k_5["ul"])
    OPT_roc_optics_k_5 <- result_optics_k_5["OPT_threshold"]
    
    # Result LooNet
    
    temp_data = data.frame(is_out = is_out, optics_res_k_5_diff_net = optics_res_k_5_diff_net)
    result_optics_k_5_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_optics_k_5_diff_net<- result_optics_k_5_diff_net["AUC"]
    CI_roc_optics_k_5_diff_net <- c(result_optics_k_5_diff_net["ll"], result_optics_k_5_diff_net["ul"])
    OPT_roc_optics_k_5_diff_net <- result_optics_k_5_diff_net["OPT_threshold"]
    
    
    # Different parameter k --> min number of neighbors
    start.time <- Sys.time()
    optics_res_sqrt_n          = evaluate_optics(X = IndNet, k = sqrt(n) )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.1276999   secs
    optics_res_sqrt_n          = evaluate_optics(X = IndNet, k = sqrt(n) )
    
    start.time <- Sys.time()
    optics_res_sqrt_n_diff_net = evaluate_optics(X = DiffNet, k = sqrt(n) )
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.145643    secs

    
    # Result IndNet
    
    temp_data = data.frame(is_out = is_out, optics_res_sqrt_n = optics_res_sqrt_n)
    result_optics_sqrt_n = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_optics_sqrt_n<- result_optics_sqrt_n["AUC"]
    CI_roc_optics_sqrt_n <- c(result_optics_sqrt_n["ll"], result_optics_sqrt_n["ul"])
    OPT_roc_optics_sqrt_n <- result_optics_sqrt_n["OPT_threshold"]
    
    # Result LooNet
    
    temp_data = data.frame(is_out = is_out, optics_res_sqrt_n_diff_net = optics_res_sqrt_n_diff_net)
    result_optics_sqrt_n_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_optics_sqrt_n_diff_net<- result_optics_sqrt_n_diff_net["AUC"]
    CI_roc_optics_sqrt_n_diff_net <- c(result_optics_sqrt_n_diff_net["ll"], result_optics_sqrt_n_diff_net["ul"])
    OPT_roc_optics_sqrt_n_diff_net <- result_optics_sqrt_n_diff_net["OPT_threshold"]
    
    
    ### OTS methods -----------------------------
    
    # OTS evaluation with different NETWORK --> IndNet or DiffNet 
    # Spoutlier method with our custom managing of the case in which the target is in the 
    # Reference area and with adding a cosine similarity & the ensemble tecnhique
    start.time <- Sys.time()
    distance_vector_OTS          = evaluate_OTS(IndNet)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 2.493325     secs
    
    start.time <- Sys.time()
    distance_vector_OTS_diff_net = evaluate_OTS(DiffNet)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 2.487344      secs
    
    
    
    # Result IndNet 
    
    temp_data = data.frame(is_out = is_out, distance_vector_OTS = as.numeric(distance_vector_OTS$DIST))
    result_OTS = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_OTS<- result_OTS["AUC"]
    CI_roc_OTS <- c(result_OTS["ll"], result_OTS["ul"])
    OPT_roc_OTS <- result_OTS["OPT_threshold"]
    
    # Result LooNet
    
    temp_data = data.frame(is_out = is_out, distance_vector_OTS_diff_net = as.numeric(distance_vector_OTS_diff_net$DIST))
    result_OTS_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_OTS_diff_net<- result_OTS_diff_net["AUC"]
    CI_roc_OTS_diff_net <- c(result_OTS_diff_net["ll"], result_OTS_diff_net["ul"])
    OPT_roc_OTS_diff_net <- result_OTS_diff_net["OPT_threshold"]
    
    
    # OTHER parameter setting (COSINE similarity and ensemble)    
    if (element$dim != 2){
      # NOT A SINGLE EDGE BUT A MODULE
      
      # OTS cosine evaluation with different NETWORK --> IndNet or DiffNet 
      start.time <- Sys.time()
      distance_vector_OTS_cosine          = evaluate_OTS_cosine(IndNet)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken # Time difference of 0.331099       secs
      
      start.time <- Sys.time()
      distance_vector_OTS_diff_net_cosine = evaluate_OTS_cosine(DiffNet)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken # Time difference of 0.2683108        secs
      
      # Result IndNet
      
      temp_data = data.frame(is_out = is_out, distance_vector_OTS_cosine = as.numeric(distance_vector_OTS_cosine$DIST))
      result_OTS_cosine = calculating_AUC_cutoff(temp_data, direction = ">")
      roc_OTS_cosine<- result_OTS_cosine["AUC"]
      CI_roc_OTS_cosine <- c(result_OTS_cosine["ll"], result_OTS_cosine["ul"])
      OPT_roc_OTS_cosine <- result_OTS_cosine["OPT_threshold"]
      
      # Result IndNet
      
      temp_data = data.frame(is_out = is_out, distance_vector_OTS_diff_net_cosine = as.numeric(distance_vector_OTS_diff_net_cosine$DIST))
      result_OTS_diff_net_cosine = calculating_AUC_cutoff(temp_data, direction = ">")
      roc_OTS_diff_net_cosine<- result_OTS_diff_net_cosine["AUC"]
      CI_roc_OTS_diff_net_cosine <- c(result_OTS_diff_net_cosine["ll"], result_OTS_diff_net_cosine["ul"])
      OPT_roc_OTS_diff_net_cosine <- result_OTS_diff_net_cosine["OPT_threshold"]
      
      
    } else {
      # IF single edge COSINE not possible --> NA 
      
      roc_OTS_cosine = NA; CI_roc_OTS_cosine = c(NA, NA) ; OPT_roc_OTS_cosine = NA
      roc_OTS_diff_net_cosine = NA; CI_roc_OTS_diff_net_cosine = c(NA, NA) ; OPT_roc_OTS_diff_net_cosine = NA
      
    }
    
    #####---OTS aggregate methods-#
    
    # This is done only on IndNet, not DIffNet
    
    # the number of repetition for the ensemble is the minimum between 20 and #SAMPLE_SIZE / 10
    # since the even ones are cosine and the odds are euclidean, the real number is min between 
    # 10 and #SAMPLE_SIZE / 20 || 20 is the #reference
    distance_matrices = matrix(NA, nrow = nrow(IndNet) , min(20,ncol = nrow(IndNet)/10 ))
    start.time <- Sys.time()
    
    
    for ( t in seq(1,min(20, nrow(IndNet)/10), by =2 ) )
    { 
      # in each repetition, er do one COSINE and one euclidean
      distance_matrices[,t] = evaluate_OTS_cosine(IndNet)$DIST 
      distance_matrices[,t+1] = evaluate_OTS(IndNet)$DIST 
      
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 26.31359         secs
    
    distance_matrices = matrix(NA, nrow = nrow(IndNet) , min(20,ncol = nrow(IndNet)/10 ))
    start.time <- Sys.time()
    
    
    for ( t in seq(1,min(20, nrow(IndNet)/10), by =2 ) )
    { 
      # in each repetition, er do one COSINE and one euclidean
      distance_matrices[,t] = evaluate_OTS_cosine(IndNet)$DIST 

    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 26.31359         secs
    
    start.time <- Sys.time()
    
    
    for ( t in seq(1,min(20, nrow(IndNet)/10), by =2 ) )
    { 
      # in each repetition, er do one COSINE and one euclidean
      distance_matrices[,t+1] = evaluate_OTS(IndNet)$DIST 
      
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 26.31359         secs
    
    
    
    distance_matrices = apply(distance_matrices,2, as.numeric)
    
    # COSINE MEDIAN matrice
    cos_matrices = distance_matrices[,seq_len(ncol(distance_matrices)) %% 2 ==1]
    median_cos_matrices = apply(cos_matrices,1, median)
    
    # EUCLIDEAN MEDIAN matrice
    euc_matrices = distance_matrices[,seq_len(ncol(distance_matrices)) %% 2 ==0]
    median_euc_matrices = apply(euc_matrices,1, median)
    
    # NORMALIZATION to merge them ( and with the cosine similarity is a similarity so 1 - ANS to have a distance)
    min_max_median_euc = (median_euc_matrices - min(median_euc_matrices) ) / ( max(median_euc_matrices) - min(median_euc_matrices) ) 
    min_max_median_cos = 1 - ( (median_cos_matrices - min(median_cos_matrices) ) / ( max(median_cos_matrices) - min(median_cos_matrices) ) )
    
    # Average
    glob_avg_OTS = ( min_max_median_euc + min_max_median_cos ) / 2
    
    if (element$dim == 2){
      # COSINE has sense only on a module
      roc_OTS_cosine_med = NA ; CI_roc_OTS_cosine_med = c(NA, NA) ; OPT_roc_OTS_cosine_med = NA
      
    } else {
      
      # Result IndNet Cosine ensemble
      temp_data = data.frame(is_out = is_out, median_cos_matrices = median_cos_matrices)
      result_OTS_cosine_med = calculating_AUC_cutoff(temp_data, direction = ">")
      roc_OTS_cosine_med<- result_OTS_cosine_med["AUC"]
      CI_roc_OTS_cosine_med <- c(result_OTS_cosine_med["ll"], result_OTS_cosine_med["ul"])
      OPT_roc_OTS_cosine_med <- result_OTS_cosine_med["OPT_threshold"]
      
      
    }
    
    # Result IndNet Euclidean ensemble
    temp_data = data.frame(is_out = is_out, median_euc_matrices = median_euc_matrices)
    result_OTS_euc_med = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_OTS_euc_med<- result_OTS_euc_med["AUC"]
    CI_roc_OTS_euc_med <- c(result_OTS_euc_med["ll"], result_OTS_euc_med["ul"])
    OPT_roc_OTS_euc_med <- result_OTS_euc_med["OPT_threshold"]
    
    
    if (element$dim == 2){
      # SINCE the merged cosine and euclidean has the cosine inside, it is not feasible for single edge
      roc_OTS_glob_avg_OTS = NA ; CI_roc_OTS_glob_avg_OTS = c(NA, NA) ; OPT_roc_OTS_glob_avg_OTS = NA; 
    } else {
      
      # Result IndNet Euclidean + Cosine ensemble
      
      temp_data = data.frame(is_out = is_out, glob_avg_OTS = glob_avg_OTS)
      result_OTS_glob_avg_OTS = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_OTS_glob_avg_OTS <- result_OTS_glob_avg_OTS["AUC"]
      CI_roc_OTS_glob_avg_OTS <- c(result_OTS_glob_avg_OTS["ll"], result_OTS_glob_avg_OTS["ul"])
      OPT_roc_OTS_glob_avg_OTS <- result_OTS_glob_avg_OTS["OPT_threshold"]
    }
    
    
    
    
    
    ###### SPOUTLIER methods ---------------------
    
    # SPOUTLIER evaluation with different NETWORK --> IndNet or DiffNet & normalization Y or N 
    
    # OTS cosine evaluation with different NETWORK --> IndNet or DiffNet 
    start.time <- Sys.time()
    score_spoutlier               <- qsp(IndNet, 20,normalize = FALSE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.0009980202        secs
    
    start.time <- Sys.time()
    score_spoutlier_diff_net      <- qsp(DiffNet, 20,normalize = FALSE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.0009782314         secs
    
    start.time <- Sys.time()
    score_spoutlier_diff_net_norm <- qsp(DiffNet, 20,normalize = TRUE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken # Time difference of 0.0009782314         secs
    
   
    
    
    # Result IndNet
    temp_data = data.frame(is_out = is_out, score_spoutlier = score_spoutlier)
    result_spoutlier = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_spoutlier<- result_spoutlier["AUC"]
    CI_roc_spoutlier <- c(result_spoutlier["ll"], result_spoutlier["ul"])
    OPT_roc_spoutlier <- result_spoutlier["OPT_threshold"]
    
    
    
    # Result LooNet false
    temp_data = data.frame(is_out = is_out, score_spoutlier_diff_net = score_spoutlier_diff_net)
    result_spoutlier_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_spoutlier_diff_net<- result_spoutlier_diff_net["AUC"]
    CI_roc_spoutlier_diff_net <- c(result_spoutlier_diff_net["ll"], result_spoutlier_diff_net["ul"])
    OPT_roc_spoutlier_diff_net <- result_spoutlier_diff_net["OPT_threshold"]
    
    # Result LooNet true
    temp_data = data.frame(is_out = is_out, score_spoutlier_diff_net_norm = score_spoutlier_diff_net_norm)
    result_spoutlier_diff_net_norm = calculating_AUC_cutoff(temp_data, direction = "<")
    roc_spoutlier_diff_net_norm<- result_spoutlier_diff_net_norm["AUC"]
    CI_roc_spoutlier_diff_net_norm <- c(result_spoutlier_diff_net_norm["ll"], result_spoutlier_diff_net_norm["ul"])
    OPT_roc_spoutlier_diff_net_norm <- result_spoutlier_diff_net_norm["OPT_threshold"]
    
    
    ############## DELTAPCC METHOD --------------------------------------------------------
    
    # Only feasible if single edge
    # Input the node matrix n *  p
    if (element$dim == 2){
      # DeltaPCC LIU et al evaluation with Node network
      Z_Values = evaluate_deltaPCC(sampled)
      temp_data = data.frame(is_out = is_out, Z_Values = Z_Values)
      result_DeltaPCC = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_DeltaPCC<- result_DeltaPCC["AUC"]
      CI_roc_DeltaPCC <- c(result_DeltaPCC["ll"], result_DeltaPCC["ul"])
      OPT_roc_DeltaPCC <- result_DeltaPCC["OPT_threshold"]
    }   else {roc_DeltaPCC = NA ;CI_roc_DeltaPCC = c(NA, NA) ; OPT_roc_DeltaPCC = NA }
    
    
    ############# COOK DISTANCE METHOD ------ 
    
    # Feasible ONLY when #features (edges) < #Individual 
    if (dim(IndNet)[2] < dim(IndNet)[1] && sum(is.na(evaluate_Cook_distance(IndNet))) == 0  ) ## 
    {
      # SPOUTLIER evaluation with different NETWORK --> IndNet or DiffNet
      start.time <- Sys.time()
      score_cook          = evaluate_Cook_distance(IndNet)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken # Time difference of 0.04986596          secs
      
      start.time <- Sys.time()
      score_cook_diff_net = evaluate_Cook_distance(DiffNet)
      end.time <- Sys.time()
      time.taken <- end.time - start.time
      time.taken # Time difference of 0.0249331           secs
      
      # Different post processing: MAX
      
      max_score_cook = apply(score_cook, 1, max ) 
      max_score_cook_diff_net = apply(score_cook_diff_net, 1, max ) 
      
      # Result IndNet max
      temp_data = data.frame(is_out = is_out, max_score_cook = max_score_cook)
      result_cook_max = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_cook_max<- result_cook_max["AUC"]
      CI_roc_cook_max <- c(result_cook_max["ll"], result_cook_max["ul"])
      OPT_roc_cook_max <- result_cook_max["OPT_threshold"]
      
      # Result LooNet max
      temp_data = data.frame(is_out = is_out, max_score_cook_diff_net = max_score_cook_diff_net)
      result_cook_max_diff_net  = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_cook_max_diff_net <- result_cook_max_diff_net ["AUC"]
      CI_roc_cook_max_diff_net  <- c(result_cook_max_diff_net ["ll"], result_cook_max_diff_net ["ul"])
      OPT_roc_cook_max_diff_net  <- result_cook_max_diff_net ["OPT_threshold"]
      
      # Different post processing: AVG
      
      avg_score_cook = apply(score_cook, 1, mean ) 
      avg_score_cook_diff_net = apply(score_cook_diff_net, 1, mean ) 
      
      # Result IndNet avg
      temp_data = data.frame(is_out = is_out, avg_score_cook = avg_score_cook)
      result_cook_avg = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_cook_avg<- result_cook_avg["AUC"]
      CI_roc_cook_avg <- c(result_cook_avg["ll"], result_cook_avg["ul"])
      OPT_roc_cook_avg <- result_cook_avg["OPT_threshold"]
      
      # Result LooNet avg
      temp_data = data.frame(is_out = is_out, avg_score_cook_diff_net = avg_score_cook_diff_net)
      result_cook_avg_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_cook_avg_diff_net<- result_cook_avg_diff_net["AUC"]
      CI_roc_cook_avg_diff_net <- c(result_cook_avg_diff_net["ll"], result_cook_avg_diff_net["ul"])
      OPT_roc_cook_avg_diff_net <- result_cook_avg_diff_net["OPT_threshold"]
      
      
      # Different post processing: MED
      
      med_score_cook = apply(score_cook, 1, median ) 
      med_score_cook_diff_net = apply(score_cook_diff_net, 1, median ) 
      
      
      # Result IndNet med
      temp_data = data.frame(is_out = is_out, med_score_cook = med_score_cook)
      result_cook_med = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_cook_med<- result_cook_med["AUC"]
      CI_roc_cook_med <- c(result_cook_med["ll"], result_cook_med["ul"])
      OPT_roc_cook_med <- result_cook_med["OPT_threshold"]
      
      
      # Result IndNet med
      temp_data = data.frame(is_out = is_out, med_score_cook_diff_net = med_score_cook_diff_net)
      result_cook_med_diff_net = calculating_AUC_cutoff(temp_data, direction = "<")
      roc_cook_med_diff_net<- result_cook_med_diff_net["AUC"]
      CI_roc_cook_med_diff_net <- c(result_cook_med_diff_net["ll"], result_cook_med_diff_net["ul"])
      OPT_roc_cook_med_diff_net <- result_cook_med_diff_net["OPT_threshold"]
    } else {
      
      if( sum(is.na(evaluate_Cook_distance(IndNet))) == 0 ) {print("MISSING VALUES in COOk")} ## 
      roc_cook_max = NA ; CI_roc_cook_max_diff_net  = c(NA, NA) ; OPT_roc_cook_max = NA
      roc_cook_max_diff_net = NA ; CI_roc_cook_max_diff_net  = c(NA, NA) ; OPT_roc_cook_max_diff_net  = NA
      roc_cook_avg = NA ;  CI_roc_cook_avg = c(NA, NA) ; OPT_roc_cook_avg = NA
      roc_cook_avg_diff_net = NA ; CI_roc_cook_avg_diff_net = c(NA, NA) ; OPT_roc_cook_avg_diff_net = NA
      roc_cook_med = NA ; CI_roc_cook_med = c(NA, NA) ; OPT_roc_cook_med = NA
      roc_cook_med_diff_net = NA ; CI_roc_cook_med_diff_net = c(NA, NA) ; OPT_roc_cook_med_diff_net = NA
    }
    
    
    