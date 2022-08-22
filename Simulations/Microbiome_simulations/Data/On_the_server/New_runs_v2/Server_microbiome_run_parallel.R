
# LIBRARIES ---------------------------------------------------------------


library(compositions)
library(spoutlier)
library(randcorr)
library(dbscan)
library(pROC)
library(randcorr)
library(DDoutlier)
library(WGCNA)
library(MASS)

library("lionessR")
library("SummarizedExperiment")
# install.packages("lsa")
library(lsa)

checkStrict <- function(f, silent=FALSE) {
  # """
  # Checking a function does not use global parameters
  # """
  vars <- codetools::findGlobals(f)
  found <- !vapply(vars, exists, logical(1), envir=as.environment(2))
  if (!silent && any(found)) {
    warning("global variables used: ", paste(names(found)[found], collapse=', '))
    return(invisible(FALSE))
  }
  
  !any(found)
}

asso_diff = function (x, f = netFun)
{
  # """
  # Calculating ISNs and LOO Net
  # """
  is.se <- inherits(x, "SummarizedExperiment")
  is.matrix <- is.matrix(x)
  if (!is.function(f)) {
    stop("please use a function")
  }
  if (is.matrix(x)) {
    print("take numeric matrix as input, ignore parameter for assay")
  }
  if (is.se) {
    colData <- SummarizedExperiment::colData(x)
    x <- SummarizedExperiment::assay(x)
  }
  if (!is.matrix(x)) {
    print("please use a numeric matrix as input")
  }
  if (is.null(colnames(x))) {
    colnames(x) = seq_len(ncol(x))
  }
  nrsamples <- ncol(x)
  samples <- colnames(x)
  # net1 <-(abs(bicor(t(x))))^2
  net <- f(x)
  # print(net[1:5,1:5])
  # print(net1[1:5,1:5])
  agg <- c(net)
  lionessOutput <- matrix(NA, nrow(net) * ncol(net), nrsamples + 
                            2)
  colnames(lionessOutput) <- c("reg", "tar", samples)
  lionessOutput[, 1] <- rep(row.names(net), ncol(net))
  lionessOutput[, 2] <- rep(colnames(net), each = nrow(net))
  lionessOutput <- as.data.frame(lionessOutput, stringsAsFactors = FALSE)
  # lionessOutput[, 3:ncol(lionessOutput)] <- vapply(lionessOutput[, 
  #                                                                3:ncol(lionessOutput)], as.numeric, vector("numeric", 
  #                                                                                                           nrow(lionessOutput)))
  # print("CHECKPOINT 1")
  lionessOutput_recons <- matrix(NA, nrow(net) * ncol(net), nrsamples + 
                                   2)
  colnames(lionessOutput_recons) <- c("reg", "tar", samples)
  lionessOutput_recons[, 1] <- rep(row.names(net), ncol(net))
  lionessOutput_recons[, 2] <- rep(colnames(net), each = nrow(net))
  lionessOutput_recons <- as.data.frame(lionessOutput_recons, stringsAsFactors = FALSE)
  # lionessOutput_recons[, 3:ncol(lionessOutput_recons)] <- vapply(lionessOutput_recons[, 
  #                                                                3:ncol(lionessOutput_recons)], as.numeric, vector("numeric", 
  #                                                                                                           nrow(lionessOutput_recons)))
  # print("CHECKPOINT 2")
  
  for (i in seq_len(nrsamples)) {
    # ss <- c((abs(bicor(t(x[, -i]))))^2 ) 
    ss <- c(f(x[,-i])) # apply netFun on all samples minus one
    #  print("CHECKPOINT 3")
    lionessOutput[, i + 2] <- nrsamples * (agg -ss) + ss
    lionessOutput_recons[, i + 2] <- agg - ss
  }
  edges <- paste(lionessOutput[, 1], lionessOutput[, 2], sep = "_")
  nodes <- colnames(x)
  rowData <- S4Vectors::DataFrame(row.names = edges, reg = lionessOutput[, 
                                                                         1], tar = lionessOutput[, 2])
  if (!is.se) {
    colData <- S4Vectors::DataFrame(row.names = nodes, sample = nodes)
  }
  # print("CHECKPOINT 4")
  
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(lioness = as.matrix(lionessOutput[, 3:ncol(lionessOutput)]),
                                                                 perturbed = as.matrix(lionessOutput_recons[, 3:ncol(lionessOutput_recons)])),
                                                   colData = colData, rowData = rowData)
  return(se)
}
checkStrict(asso_diff)
func = function(x, esp)
{
  function(x)
  {
    abs(bicor(t(x))) ^ esp
  }
  
}

indNet_function = function(sampled){
  # """
  # Calculated ISN with the LIONESS METHOD and then eliminate the double entries e.g. Gene1_gene2 & Gene2_gene1
  # 
  # Return: a vector with ISN and LOO 
  # """
  rownames(sampled) = paste0("Ind", seq(1,nrow(sampled)))
  colnames(sampled) = paste0("Gene", seq(1,ncol(sampled)))
  
  transpose_filtered_vector_ID = t(sampled)
  rowData <- S4Vectors::DataFrame(row.names = rownames(transpose_filtered_vector_ID), gene = rownames(transpose_filtered_vector_ID))
  colData <- S4Vectors::DataFrame(col.names = colnames(transpose_filtered_vector_ID), sample =colnames(transpose_filtered_vector_ID))
  
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = transpose_filtered_vector_ID), 
                                                   colData = colData, rowData = rowData)
  
  lionessResults  <- asso_diff(se)
  # lionessResults  <- lioness(se)
  
  # summary(lionessResults)
  all_assays = assays(lionessResults)
  Resulting_net = all_assays$lioness
  # dim(Resulting_net)
  # head(str(Resulting_net))
  # summary(Resulting_net)
  # type(Resulting_net)
  # head(rownames(Resulting_net) )
  Recons_net = all_assays$perturbed
  
  names_r_net = rownames(Resulting_net)
  
  length(names_r_net)
  to_eliminate_r = c()
  for (i in 1:(length(names_r_net)))
  {
    elemento = names_r_net[i]
    fin = unlist(strsplit(elemento,"_"))
    fin2 = paste0(fin[2],sep = "_", fin[1])
    value = match(fin2,names_r_net, nomatch = 0)
    if (value > i || fin2 == elemento) 
    {
      to_eliminate_r= append(to_eliminate_r, value)
    }
    
    if (i %% 1000 == 0){ print(i)}
  }
  Resulting_net = Resulting_net[-to_eliminate_r,]
  Recons_net = Recons_net[-to_eliminate_r,]
  # Recons_net[Recons_net>1]
  # Resulting_net[Resulting_net>1]
  # global_net2$global>1
  net = cor(t(transpose_filtered_vector_ID) )
  lionessOutput <- matrix(NA, nrow(net) * ncol(net),3)
  lionessOutput[, 1] <- rep(row.names(net), ncol(net))
  lionessOutput[, 2] <- rep(colnames(net), each = nrow(net))
  lionessOutput <- as.data.frame(lionessOutput, stringsAsFactors = FALSE)
  lionessOutput[,3] <- c(net)
  # global_net = data.frame("global" = lionessOutput[,3] )
  # rownames(global_net) = paste(lionessOutput[,1], lionessOutput[,2], sep = "_")
  
  # global_net$est = rownames(global_net)
  # global_net2 = global_net[-to_eliminate_r,]
  # r_glob = rownames(global_net)
  
  
  
  
  
  
  # dim(Resulting_net )
  return (list(IndNet = t(Resulting_net), Recons_net = t(Recons_net) ) )  
  
  
}
checkStrict(indNet_function)

evaluate_LOO = function(n,m, rep, sampled ){
  # """
  # 
  # 
  # Evaluate with the LOO method: take as input 
  # n = sample size
  # m = # outlier
  # rep = # repetition
  # sampled = Input (node) data
  # 
  # Calculate the quantiles 0.025 and 0.975 of the empirical distribution  
  # Calculate the accuracy and precision 
  # Return a vector [0,0,1,.. 0] of prediction (with 1 if the summed edges  is over the threhsold
  # 0 otherwise
  # both edges and threhsold are founded in a module as the sum of the absolute univariate difference 
  # sum(abs(real_cor_ - cor_perturbed_ ) ) / 2
  # """
  
  
  (tot_sigma =  cor(sampled) )
  
  diff_vector = c()
  for (i in 1:rep)
  {
    sampled_ = mvrnorm(n = n, rep(0, ncol(tot_sigma)), tot_sigma)
    # real_cor = abs(bicor(sampled))^2
    # real_cor = FUN(sampled_, ...)
    real_cor_ = cor(sampled_)
    # cor(sampled) #diff between cor and var --> corr hp that var ==1
    # cor_perturbed = abs(bicor(sampled[-1,]))^2
    cor_perturbed_ = cor(sampled_[-1,]) # FUN(sampled_[-1,], ...)
    
    diff =  sum(abs(real_cor_ - cor_perturbed_ ) ) / 2
    
    diff_vector[i] = diff
  }
  
  emp_dist_neg = ecdf(diff_vector)
  emp_dist_neg
  (qq_neg160 = quantile(ecdf(diff_vector),c(0.95)) ) 
  
  acc_same_n  = c()
  ref_same_n  = c()
  acc_beta_same_n= c()
  ref_beta_same_n= c()
  d_vector = c()
  prediction = c()
  for (i in 1:(n-m) ) 
  {
    
    cor_perturbed = cor(sampled[-i,])
    # trasf_cor = 0.5*log10( (1+real_cor)/(1-real_cor) )
    # trasf_cor_pert = 0.5*log10( (1+cor_perturbed)/(1-cor_perturbed) )
    diff =  sum( abs(tot_sigma - cor_perturbed ) ) / 2 
    d_vector = append(d_vector, diff)
    
    if (diff < qq_neg160[1][[1]]){
      acc_same_n = c(acc_same_n,diff) 
      prediction = c(prediction,0)
    } else {
      ref_same_n = c(ref_same_n,diff) 
      prediction = c(prediction,1)
    }
    
  }
  
  
  for (i in (n-m+1):n)
  {
    # print(i)
    cor_perturbed = cor(sampled[-i,])
    # trasf_cor = 0.5*log10( (1+real_cor)/(1-real_cor) )
    # trasf_cor_pert = 0.5*log10( (1+cor_perturbed)/(1-cor_perturbed) )
    diff =  sum( abs(tot_sigma - cor_perturbed ) ) / 2 
    d_vector = append(d_vector, diff)
    
    if (diff < qq_neg160[1][[1]]){
      acc_beta_same_n = c(acc_beta_same_n,diff) 
      prediction = c(prediction,0)
    } else {
      ref_beta_same_n = c(ref_beta_same_n,diff) 
      prediction = c(prediction,1)
      
    }
  }
  
  (alpha_same_n =     length(ref_same_n)/(length(acc_same_n)+length(ref_same_n)) )
  (beta_same_n =     length(ref_beta_same_n)/(length(acc_beta_same_n)+length(ref_beta_same_n)) )
  
  is_out= c(rep(0,n-m), rep(1,m))
  ( t_LOO = table(LOO=prediction, Actual=is_out) )
  return( d_vector)
  
} ## version witrh returning distance vector
checkStrict(evaluate_LOO)

evalute_knn = function(is_out, IndNet, k_min , k_max){
  # """
  # evaluate the kNN algorithm 
  # Input: a vector with [0,,1,..1] if an obseration is outlier or not 
  # IndNet . a network to use as input on the kNN algorithm (can be both Indnet or LOONet)
  # k_min and k_max the minimum and maximum extreme of the k used by the method and aggregated in the results
  # 
  # Return: a knn.aggregate object whose main interesting component for us is the distance ranking (outlier score)
  # 
  # """
  
  # colnames(IndNet)
  
  n = nrow(IndNet); p = ncol(IndNet)
  # kimn = min(log(n),p+1) ; kmax =  max(log(n),p+1)
  knn.agg<-KNN_AGG(IndNet, k_min = k_min, k_max = k_max)
  # plot(knn.agg)
  
  # rfind<- rpart_split_finder(knn.agg, n)
  # plot(sort(knn.agg),col=is_out[order(knn.agg)]+1)
  # abline(h=rfind)
  knn.agg #scegliere l'indice del valore degli split possibili che si desideri usare come soglia
  #Si ricorda che eventuali altri valori di soglia desiderati possono essere scelti manualmente
}

checkStrict(evalute_knn)


evaluate_LOO_multivariate_threhsold = function(n,m, rep, sampled , dims){
  # """
  # 
  # 
  # Evaluate with the MultiLOO method: take as input 
  # n = sample size
  # m = # outlier
  # rep = # repetition
  # sampled = Input (node) data
  # dims: the dimension of the module 
  # 
  # Calculate the quantiles 0.025 and 0.975 of the empirical distribution  
  # Calculate the accuracy and precision 
  
  # Calculate a p-value for each DIMENSION of the network , for each edge, 
  # multivariate cloud of significance. 
  # then to have a scalar result, the approximation is done via 
  # Return a vector [0,0,1,.. 0] of prediction (with 1 if the summed edges  is over the threhsold
  # 0 otherwise
  # both edges and threhsold are founded in a module as the sum of the absolute univariate difference 
  # sum(abs(real_cor_ - cor_perturbed_ ) ) / 2
  
  # Return : 
  # List of number of dimension (edges) for which the threhsold is refused [ THREHSOLD is BONFERRONI corrected]
  # and the pvalue for each dimension of na individual
  # """
  
  
  (tot_sigma =  cor(sampled) )
  
  diff_vector = matrix(NA, nrow = rep, ncol = (dims^2 - dims) /2)
  for (i in 1:rep)
  {
    sampled_ = mvrnorm(n = n, rep(0, ncol(tot_sigma)), tot_sigma)
    # real_cor = abs(bicor(sampled))^2
    # real_cor = FUN(sampled_, ...)
    real_cor_ = cor(sampled_)
    # cor(sampled) #diff between cor and var --> corr hp that var ==1
    # cor_perturbed = abs(bicor(sampled[-1,]))^2
    cor_perturbed_ = cor(sampled_[-1,]) # FUN(sampled_[-1,], ...)
    
    diff =  (real_cor_ - cor_perturbed_ )
    diff2 = as.vector(diff[upper.tri(diff)])
    diff_vector[i,] = diff2
  }
  qq = 0.025 / ((dims^2 - dims) /2)
  quantiles = c(qq,1-qq)
  qq_neg160 = apply(diff_vector, MARGIN = 2, function(x) quantile(ecdf(x), quantiles ) )
  ecdfs = apply(diff_vector, MARGIN = 2, function(x) ecdf(x) )
  
  prediction = c()
  ff_ref = c()
  all_p_values = matrix(NA, nrow = n, ncol = (dims^2 - dims) /2   )
  for (i in 1:(n) ) 
  {
    
    cor_perturbed = cor(sampled[-i,])
    # trasf_cor = 0.5*log10( (1+real_cor)/(1-real_cor) )
    # trasf_cor_pert = 0.5*log10( (1+cor_perturbed)/(1-cor_perturbed) )
    diff =  tot_sigma - cor_perturbed
    
    diff2 = as.vector(diff[upper.tri(diff)])
    diff2 = t(matrix(diff2))
    final_ref = ifelse( ( diff2 > qq_neg160[1,] ) + (diff2 < qq_neg160[2,] )  == 2, TRUE, FALSE ) 
    
    p_values_emp = sapply(1:ncol(diff2), function(i) ecdfs[i][[1]](diff2[i]) )
    all_p_values[i,] = p_values_emp
    if (sum(final_ref) == (dims^2 - dims) /2 )
    {
      # acc_same_n = c(acc_same_n,diff) 
      prediction = c(prediction,0)
    } else {
      # ref_same_n = c(ref_same_n,diff) 
      prediction = c(prediction,1)
    }
    ff_ref[i] = sum(final_ref)
    
  }
  is_out= c(rep(0,n-m), rep(1,m))
  ( t_LOO = table(LOO=prediction, Actual=is_out) )
  table(ff_ref)
  # plot(ff_ref, col = is_out + 1)
  ll = list(Refusal = ff_ref, p_values = all_p_values )
  return( ll)
  
}
checkStrict(evaluate_LOO_multivariate_threhsold)


evaluate_optics = function(X, k)
{
  
  # """
  # Evaluate optics method 
  # 
  # Input: X = Netwrok on which we want to perform the analysis 
  #             E.g. IndNet or LooNet
  #             k = the number of neighborhood for the optic procedure, anaolog of 
  #             the k in kNN 
  #             
  # Returns: OF --> a vecor of outlier scores for each individual 
  # """
  res<-optics(X,eps=500,minPts = k) #scegliamo k1 ad esempio, scegliere il k desiderato
  
  resultKNN=kNN(X,k)  #ad esempio
  # resultKNN# head(resultKNN$id,3)
  numberid=resultKNN$id
  # head(res$order,n=15)
  #altro modo per estrarlo
  distanzerech=res$reachdist
  # head(distanzerech)
  distanzerech[1]=distanzerech[2]
  dist=vector()
  lrd=vector()
  n = nrow(X)
  ###FIXING problems with replication
  for (i in 1:n){
    minptsvicini=numberid[i,]
    dist[i]=mean(distanzerech[numberid[i,]])
  }
  distmagg=dist[dist>0]
  valore=min(distmagg)
  dist[dist==0]=valore/2
  lrd=1/dist
  # hist(dist)
  numeratore=vector()
  OF=vector()
  
  for (i in 1:n){
    minptsvicini=numberid[i,]
    lrd_numero=lrd[i]
    lrd_minpts=lrd[numberid[i,]]
    numeratore[i]=sum(lrd_minpts/lrd_numero)
    OF[i]=numeratore[i]/k
  }
  # summary(lrd);sum(is.na(OF));str(OF)
  # (sum=summary(OF))
  ###cutting
  # (cfind=rpart_split_finder(OF,length(OF)))
  # (rfind=ctree_split_finder(OF,length(OF)))
  ####
  
  #OF 
  # plot(sort(OF),type="l",ylim=c(sum[1],sum[6]))
  # abline(h=cfind[4])
  # rbest_taglio <-  0.034# 0.017# sort(rfind)[2] #scegliere l'indice del valore degli split possibili che si desideri usare come soglia 
  return(OF)
  
}

checkStrict(evaluate_optics)


evaluate_OTS = function(IndNet, s = 20)
{
  # """
  # Evaluate OTS method with our custom implementation
  # 
  # Input: IndNet = Netwrok on which we want to perform the analysis
  #             E.g. IndNet or LooNet
  #        s = the number of reference observation used for the procedure
  #            Default = 20 as in the Karsten's paper 
  # 
  # This is the custom implemantion by me in whcih the choosen number of reference samples
  # Are 21, [s +1] so when we want toc alcualte the OTS score of an observation inside of the 
  # s+1 = 21 references samples we EXCLUDE THAT observation and every other observation is considered. 
  # Otherwise, a random set of 20 observation is used. 
  # 
  # Method: Euclidean distance, take the minimum distance with the rational taht an outleir is an observation far away from everyother obs
  # No scaling since the data are akready been pre scaled  
  #
  # Returns: distance vector --> a vecor of odistances for each individual, >> distance, >> probability being an outlier 
  # """
  
  # s_sampled = scale(IndNet)IndNet
  mean(IndNet)
  var(IndNet)
  dim(IndNet)
  # s = 20
  # one_time_sample %>% filter(rown)
  row_sampled = sample(nrow(IndNet),size = s+1, replace = F )
  one_time_sample = as.matrix(IndNet[row_sampled,])
  distance_vector = data.frame(IND = rep(NA, nrow(IndNet)), DIST = rep(NA, nrow(IndNet)) )
  for (i in 1:nrow(IndNet))
  {
    # print(i)
    if (rownames(IndNet)[i] %in% rownames(one_time_sample)){
      end_sample = as.matrix(one_time_sample[!row.names(one_time_sample)%in%rownames(IndNet)[i],])
    } else {
      end_sample = as.matrix(one_time_sample[sample(s+1,s, replace = F),])
    }
    min_dist = min(apply(end_sample, 1,function(x) dist(rbind(IndNet[i,],x))) )
    distance_vector[i,] = c(rownames(IndNet)[i], min_dist)
  }
  
  # plot(distance_vector$DIST)
  return(distance_vector)
  # roc_OTS<- roc(response = is_out, predictor = as.numeric(distance_vector$DIST))
  # auc(roc_OTS)
}
checkStrict(evaluate_OTS)

evaluate_OTS_cosine = function(IndNet, s = 20)
{
  # """
  # Evaluate OTS method with our custom implementation
  # 
  # Input: IndNet = Netwrok on which we want to perform the analysis
  #             E.g. IndNet or LooNet
  #        s = the number of reference observation used for the procedure
  #            Default = 20 as in the Karsten's paper 
  # 
  # This is the custom implemantion by me in whcih the choosen number of reference samples
  # Are 21, [s +1] so when we want toc alcualte the OTS score of an observation inside of the 
  # s+1 = 21 references samples we EXCLUDE THAT observation and every other observation is considered. 
  # Otherwise, a random set of 20 observation is used. 
  # 
  # Method: COSINE SIMILARITY , take the amximum: an outleir is not similar to any otehr observation s
  #  Nos caling: data has already been pre scaled 
  #
  # Returns: distance vector --> a vecor of odistances for each individual, >> distance, >> probability being an outlier 
  # """
  
  
  
  # s_sampled = scale(IndNet)IndNet
  mean(IndNet)
  var(IndNet)
  dim(IndNet)
  # s = 20
  # one_time_sample %>% filter(rown)
  row_sampled = sample(nrow(IndNet),size = s+1, replace = F )
  one_time_sample = as.matrix(IndNet[row_sampled,])
  distance_vector = data.frame(IND = rep(NA, nrow(IndNet)), DIST = rep(NA, nrow(IndNet)) )
  for (i in 1:nrow(IndNet))
  {
    # print(i)
    if (rownames(IndNet)[i] %in% rownames(one_time_sample)){
      end_sample = as.matrix(one_time_sample[!row.names(one_time_sample)%in%rownames(IndNet)[i],])
    } else {
      end_sample = as.matrix(one_time_sample[sample(s+1,s, replace = F),])
    }
    max_dist = max(apply(end_sample, 1,function(x) cosine(IndNet[i,],x)) )
    
    cosine(end_sample[1,], IndNet[1,])
    cosine(end_sample[2,], IndNet[1,])
    cosine(end_sample[3,], IndNet[1,])
    
    distance_vector[i,] = c(rownames(IndNet)[i], max_dist)
  }
  
  # plot(distance_vector$DIST)
  return(distance_vector)
  # roc_OTS<- roc(response = is_out, predictor = as.numeric(distance_vector$DIST))
  # auc(roc_OTS)
}
checkStrict(evaluate_OTS_cosine)

evaluate_deltaPCC = function(sampled)
{
  # """
  # Evaluate DeltaPCC method - the Liu et al implementation
  # 
  # Input: sampled : node network, a n * p network [ind * genes]
  # 
  # Procedure: leave one out an observation, calculate the difference and normalize it 
  # To find a z-value
  # 
  # Returns: Z_PCC --> a Z-value that cab be compared with a N(0,1) to find the p-value
  # """
  (tot_sigma =  cor(sampled) )
  n = nrow(sampled)
  Z_PCC = rep(NA,n)
  for (i in 1:n) 
  {
    cor_perturbed = cor(sampled[-i,])
    # trasf_cor = 0.5*log10( (1+real_cor)/(1-real_cor) )
    # trasf_cor_pert = 0.5*log10( (1+cor_perturbed)/(1-cor_perturbed) )
    diff =  tot_sigma[1,2] - cor_perturbed[1,2] 
    # d_vector = append(d_vector, diff)
    Z_PCC[i] = abs(diff / (1 -  cor_perturbed[1,2]^2 ) * (n -1) )
  }
  return(Z_PCC)
}
checkStrict(evaluate_deltaPCC)

evaluate_Cook_distance = function(IndNet)
{
  # """
  # Evaluate Cook_distance method - our custom implementation
  # 
  # Input: IndNet : A network that can be an IndNet or a LooNet
  # 
  # Procedure: For each edge that is in the module, it is a target and to predict all of the others 
  # the edges are used as features and the cook's distance is retrieved. 
  # 
  # Returns: Individual_cooks --> a matrix with same dimension of the input Individual network , where the 
  # ## i,j entry is the cook's value for variable j into predicting variable i 
  # 
  # """
  c_name = colnames(IndNet)
  data = data.frame(IndNet)
  Individual_cooks = matrix(data = NA, nrow = nrow(IndNet), ncol = ncol(IndNet) )
  for (i in 1: ncol(IndNet))
  { 
    forms = as.formula(paste(c_name[i], "~", "."))
    mod = lm(forms, data = data)
    cooksd <- cooks.distance(mod)
    cooksd[1:5]
    max(cooksd)
    Individual_cooks[,i] = cooksd
  }
  
  return(Individual_cooks)
  
}
checkStrict(evaluate_Cook_distance)


# SIMULATION microbiome data  ---------------------------------------------



library(gtools)
library(Pareto)
library(data.table)
# install.packages("splitstackshape")
library(splitstackshape)
library(OptimalCutpoints)





# FUNCTION ----------------------------------------------------------------


calculating_AUC_cutoff = function(temp_data, direction = "<"){
  # """
  # Evaluate Cook_distance method - our custom implementation
  # 
  # Input: temp_data : Is a dataframe with two columns: is_out if it is an outlier 
  #     and a vector of outlier values. 
  # 
  # Direction is if higher values of the predicted vector means more probable to be outlier or not. 
  # 
  # Calculation --> OPTIMAL CUTPOINT that calcualte the best cut to see outlier or not [ Youden method ]+
  # AUC (into the roc)
  # + confidence interval of the AUC 
  # 
  # Returns: a vector of vectos with the calculation belows
  # 
  # """
  optimal.cutpoint.Youden <- optimal.cutpoints(X = colnames(temp_data)[2], status = colnames(temp_data)[1], tag.healthy = 0,direction = direction,
                                               methods = "Youden", data = temp_data, pop.prev = NULL,
                                               control = control.cutpoints(), ci.fit = FALSE, conf.level = 0.95, trace = FALSE)
  
  OPT_cutoff = optimal.cutpoint.Youden$Youden$Global$optimal.cutoff$cutoff[1] 
  # this way it is assured to have the lowest threhsold
  
  roc = optimal.cutpoint.Youden$Youden$Global$measures.acc$AUC[1]
  CI_roc = c( optimal.cutpoint.Youden$Youden$Global$measures.acc$AUC[2], 
              optimal.cutpoint.Youden$Youden$Global$measures.acc$AUC[3] )
  return(c(OPT_threshold = OPT_cutoff, roc, CI_roc))
}
checkStrict(calculating_AUC_cutoff)


# Example in the paper ----------------------------------------------------

# https://www.biorxiv.org/content/10.1101/711317v3.full
# https://cran.r-project.org/web/packages/OptimalCutpoints/OptimalCutpoints.pdf


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

args=(commandArgs(TRUE))
print(args) #args[1] will be you job number (1 to 10 in that case)
fold_fin = args[1]

set.seed(fold_fin)

# how many repetition for each grid 

num_rep = 100

# Crete empry container for the resutls

array_res <- array(rep(NA, nrow(full_GR)*29*num_rep), dim=c(nrow(full_GR),29,num_rep))
optimal_cutoff <- array(rep(NA, nrow(full_GR)*29*num_rep), dim=c(nrow(full_GR),29,num_rep))
confidence_interval <- array(rep(NA, nrow(full_GR)*29*2*num_rep), dim=c(nrow(full_GR),29*2,num_rep))

setwd(paste0("/massstorage/URT/GEN/BIO3/Federico/High_dimensional_outlier/Microbiome_result/New_runs_v2/Run_", fold_fin))

dim(array_res)


for (j in 1:num_rep){
  ## for each repetition
  for (i in 1:nrow(full_GR)){
    
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
    write.csv(full_data, file = "Current_data.csv")
    # apply(full_data,2,mean) == 0
    # SCALING the data --------------------------------------------------------
    
    
    scale_full =data.frame(clr(x = full_data)) # / nsamples
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
    
    list_results_LOO_MULTI = evaluate_LOO_multivariate_threhsold(element$n,element$m,rep, sampled, element$dim) # rep = #repetition creating a background network
    
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
    Loo = evaluate_LOO(element$n,element$m,rep,sampled) 
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
    knn.agg_log_N_P          = evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(log(n),p+1), k_max =  max(log(n),min(p+1,50)) )
    knn.agg_log_N_P_diff_net = evalute_knn(is_out = is_out, IndNet = DiffNet,k_min =  min(log(n),p+1), k_max =  max(log(n),min(p+1,50)) )
    
    
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
    knn.agg_5_sqrt_N =          evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(sqrt(n),5), k_max =  max(sqrt(n),5)  )
    knn.agg_5_sqrt_N_diff_net = evalute_knn(is_out = is_out, IndNet = DiffNet,k_min =  min(sqrt(n),5), k_max =  max(sqrt(n),5)  )
    
    
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
    optics_res_k_mean          = evaluate_optics(X = IndNet, k = mean(c(p+1,log(n))) )
    optics_res_k_mean_diff_net = evaluate_optics(X = DiffNet, k = mean(c(p+1,log(n))) )
    
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
    optics_res_k_5          = evaluate_optics(X = IndNet, k = 5 )
    optics_res_k_5_diff_net = evaluate_optics(X = DiffNet, k = 5 )
    
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
    optics_res_sqrt_n          = evaluate_optics(X = IndNet, k = sqrt(n) )
    optics_res_sqrt_n_diff_net = evaluate_optics(X = DiffNet, k = sqrt(n) )
    
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
    distance_vector_OTS          = evaluate_OTS(IndNet)
    distance_vector_OTS_diff_net = evaluate_OTS(DiffNet)
    
    
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
      distance_vector_OTS_cosine          = evaluate_OTS_cosine(IndNet)
      distance_vector_OTS_diff_net_cosine = evaluate_OTS_cosine(DiffNet)
      
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
    
    for ( t in seq(1,min(20, nrow(IndNet)/10), by =2 ) )
    { 
      # in each repetition, er do one COSINE and one euclidean
      distance_matrices[,t] = evaluate_OTS_cosine(IndNet)$DIST 
      distance_matrices[,t+1] = evaluate_OTS(IndNet)$DIST 
      
    }
    
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
    score_spoutlier               <- qsp(IndNet, 20,normalize = FALSE)
    score_spoutlier_diff_net      <- qsp(DiffNet, 20,normalize = FALSE)
    score_spoutlier_diff_net_norm <- qsp(DiffNet, 20,normalize = TRUE)
    
    
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
      score_cook          = evaluate_Cook_distance(IndNet)
      score_cook_diff_net = evaluate_Cook_distance(DiffNet)
      
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
    
    
    
    
    
    
    
    # FILLING arrays ----------------------------------------------------------
    
    
    
    array_res[i,,j]= c(roc_Loo_multi, roc_Loo, roc_KNN_log_N_P, roc_KNN_log_N_P_diff_net,  roc_KNN_5_sqrt_N,
                       roc_KNN_5_sqrt_N_diff_net,  roc_optics_k_mean, roc_optics_k_mean_diff_net,
                       roc_optics_k_5, roc_optics_k_5_diff_net, roc_optics_sqrt_n, roc_optics_sqrt_n_diff_net, 
                       roc_OTS, roc_OTS_diff_net, roc_OTS_cosine, roc_OTS_diff_net_cosine,
                       roc_OTS_cosine_med, roc_OTS_euc_med, roc_OTS_glob_avg_OTS, 
                       roc_spoutlier, roc_spoutlier_diff_net, roc_spoutlier_diff_net_norm, roc_DeltaPCC ,
                       roc_cook_max, roc_cook_max_diff_net, roc_cook_avg, roc_cook_avg_diff_net,
                       roc_cook_med, roc_cook_med_diff_net)    # preds = ifelse(list_results$Refusal == (element$dim^2 - element$dim) /2,0,1 )
    
    
    
    confidence_interval[i,,j]= c(CI_roc_Loo_multi, CI_roc_Loo, CI_roc_KNN_log_N_P, CI_roc_KNN_log_N_P_diff_net,  CI_roc_KNN_5_sqrt_N,
                                 CI_roc_KNN_5_sqrt_N_diff_net,  CI_roc_optics_k_mean, CI_roc_optics_k_mean_diff_net,
                                 CI_roc_optics_k_5, CI_roc_optics_k_5_diff_net, CI_roc_optics_sqrt_n, CI_roc_optics_sqrt_n_diff_net, 
                                 CI_roc_OTS, CI_roc_OTS_diff_net, CI_roc_OTS_cosine, CI_roc_OTS_diff_net_cosine,
                                 CI_roc_OTS_cosine_med, CI_roc_OTS_euc_med, CI_roc_OTS_glob_avg_OTS, 
                                 CI_roc_spoutlier, CI_roc_spoutlier_diff_net, CI_roc_spoutlier_diff_net_norm, CI_roc_DeltaPCC ,
                                 CI_roc_cook_max, CI_roc_cook_max_diff_net, CI_roc_cook_avg, CI_roc_cook_avg_diff_net,
                                 CI_roc_cook_med, CI_roc_cook_med_diff_net)    # preds = ifelse(list_results$Refusal == (element$dim^2 - element$dim) /2,0,1 )
    
    
    
    optimal_cutoff[i,,j]= c(OPT_roc_Loo_multi, OPT_roc_Loo, OPT_roc_KNN_log_N_P, OPT_roc_KNN_log_N_P_diff_net,  OPT_roc_KNN_5_sqrt_N,
                            OPT_roc_KNN_5_sqrt_N_diff_net,  OPT_roc_optics_k_mean, OPT_roc_optics_k_mean_diff_net,
                            OPT_roc_optics_k_5, OPT_roc_optics_k_5_diff_net, OPT_roc_optics_sqrt_n, OPT_roc_optics_sqrt_n_diff_net, 
                            OPT_roc_OTS, OPT_roc_OTS_diff_net, OPT_roc_OTS_cosine, OPT_roc_OTS_diff_net_cosine,
                            OPT_roc_OTS_cosine_med, OPT_roc_OTS_euc_med, OPT_roc_OTS_glob_avg_OTS, 
                            OPT_roc_spoutlier, OPT_roc_spoutlier_diff_net, OPT_roc_spoutlier_diff_net_norm, OPT_roc_DeltaPCC ,
                            OPT_roc_cook_max, OPT_roc_cook_max_diff_net, OPT_roc_cook_avg, OPT_roc_cook_avg_diff_net,
                            OPT_roc_cook_med, OPT_roc_cook_med_diff_net)    # preds = ifelse(list_results$Refusal == (element$dim^2 - element$dim) /2,0,1 )
    
    
    
    
  }
  
  print("")
  print("")
  print("")
  print("")
  print("Iteration = ")
  print(j)
  print("")
  print("")
  print("")
  
  # DO THE MEDIAN OVER ALL THE RESULTS FOR ALL INTERESTING insights every each one of the 200 interaction (or the j already performed)
  
  median_results = apply(array_res, MARGIN = c(1,2), function(x) median(x,na.rm = TRUE) )
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
  ll = levels(interaction(names_rocs,high_low,sep='_'))
  ll_real = sort(ll)
  
  
  colnames(results_multi_CI) =   c(colnames(full_GR), ll_real)
  
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
  
  setwd(paste0("/massstorage/URT/GEN/BIO3/Federico/High_dimensional_outlier/Microbiome_result/New_runs_v2/Run_", fold_fin))
  
  
  # WRITING -----------------------------------------------------------------
  
  # MEDIAN AUC  
  aa=  paste0("Results_all_methods", j, ".txt")  
  write.csv(results_multi,aa)
  print(dim(array_res))
  
  ## SAVE the RDS of the AUC with all the run 
  bb =  paste0("Not_median_result", j, "rda") 
  saveRDS(array_res[,,c(1:j)], file = bb)
  # readRDS("Not_median_result1rda")# load(file = bb)
  
  # MEDIAN CI
  cc = paste0("Confidence_interval_all_methods", j, ".txt")
  write.csv(results_multi_CI,cc)  
  
  ## txt of the rds ?? of every run of the confidence interval
  dd = paste0("Not_median_Confidence_interval_all_methods", j, ".txt")
  saveRDS(confidence_interval[,,c(1:j)],dd)
  
  ## MEDIAN optimal thre
  ee = paste0("optimal_THRE_all_methods", j, ".txt")
  write.csv(results_multi_OPT,ee)  
  
  ## txt of the rds ?? of every run of the optimal threhsold
  ff = paste0("Not_median_optimal_THRE_all_methods", j, ".txt")
  saveRDS(optimal_cutoff[,,c(1:j)],ff)
  
  
}      


