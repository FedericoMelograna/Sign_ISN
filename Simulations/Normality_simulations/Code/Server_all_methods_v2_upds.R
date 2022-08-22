
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
  net1 <-(abs(bicor(t(x))))^2
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
  print("CHECKPOINT 1")
  lionessOutput_recons <- matrix(NA, nrow(net) * ncol(net), nrsamples + 
                                   2)
  colnames(lionessOutput_recons) <- c("reg", "tar", samples)
  lionessOutput_recons[, 1] <- rep(row.names(net), ncol(net))
  lionessOutput_recons[, 2] <- rep(colnames(net), each = nrow(net))
  lionessOutput_recons <- as.data.frame(lionessOutput_recons, stringsAsFactors = FALSE)
  # lionessOutput_recons[, 3:ncol(lionessOutput_recons)] <- vapply(lionessOutput_recons[, 
  #                                                                3:ncol(lionessOutput_recons)], as.numeric, vector("numeric", 
  #                                                                                                           nrow(lionessOutput_recons)))
  print("CHECKPOINT 2")
  
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
  print("CHECKPOINT 4")
  
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
  
  rownames(sampled) = paste0("Ind", seq(1,nrow(sampled)))
  colnames(sampled) = paste0("Gene", seq(1,ncol(sampled)))
  
  transpose_filtered_vector_ID = t(sampled)
  rowData <- S4Vectors::DataFrame(row.names = rownames(transpose_filtered_vector_ID), gene = rownames(transpose_filtered_vector_ID))
  colData <- S4Vectors::DataFrame(col.names = colnames(transpose_filtered_vector_ID), sample =colnames(transpose_filtered_vector_ID))
  
  se <- SummarizedExperiment::SummarizedExperiment(assays = list(counts = transpose_filtered_vector_ID), 
                                                   colData = colData, rowData = rowData)
  
  lionessResults  <- asso_diff(se)
  # lionessResults  <- lioness(se)
  
  summary(lionessResults)
  all_assays = assays(lionessResults)
  Resulting_net = all_assays$lioness
  dim(Resulting_net)
  head(str(Resulting_net))
  # summary(Resulting_net)
  # type(Resulting_net)
  head(rownames(Resulting_net) )
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
  Recons_net[Recons_net>1]
  Resulting_net[Resulting_net>1]
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
  
  
  
  
  
  
  dim(Resulting_net )
  return (list(IndNet = t(Resulting_net), Recons_net = t(Recons_net) ) )  
  
  
}
checkStrict(indNet_function)

evaluate_LOO = function(n,m, rep, sampled ){
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
    print(i)
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
  colnames(IndNet)
  
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
  summary(lrd);sum(is.na(OF));str(OF)
  (sum=summary(OF))
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

ns = c(100,500,1000,2000)
ms = c(1,5,10)
dims = c(2,3,5,7,9,11,17)
out_gene_same_distrib = c(TRUE, FALSE)
# dims = c(2,3)
# ms = c(1,5)
# ns = c(100)
full_GR = expand.grid(n =ns,m = ms,dim = dims, out_gene_same_distrib = out_gene_same_distrib)
nass = rep(NA,nrow(full_GR))
# auc_list = data.frame(roc_Loo = nass, roc_KNN = nass, roc_optics = nass, roc_OTS = nass)
rep = 20000


set.seed(123)
# mmm = as.matrix(auc_list)
num_rep = 400
array_res <- array(rep(NA, nrow(full_GR)*29*num_rep), dim=c(nrow(full_GR),29,num_rep))
# str(x)
# num [1:365, 1:5, 1:4] 1 1 1 1 1 1 1 1 1 1 ...
j = 1
i = 1
# Direction: https://stackoverflow.com/questions/31756682/what-does-coercing-the-direction-argument-input-in-roc-function-package-proc
for (j in 1:num_rep){
  for (i in 1:nrow(full_GR)) {
    element = full_GR[i,]
    cc = randcorr(element$dim)
    real_matrix =  MASS::mvrnorm(n = element$n-element$m, mu = rep(0,element$dim), Sigma = cc)
    if (element$out_gene_same_distrib){
      cc2 = randcorr(element$dim) 
      outlier =  MASS::mvrnorm(n = element$m, mu = rep(0,element$dim), Sigma = cc2)
    } else {
      outlier = c()
      for (int_i in 1:element$m){
        cc2 = randcorr(element$dim)
        outlier =  rbind(outlier, MASS::mvrnorm(n =1, mu = rep(0,element$dim), Sigma = cc2) )
      }
    }
    is_out =  c(rep(0,element$n-element$m), rep(1,element$m))
    sampled = rbind(real_matrix, outlier)
    sampled = scale(sampled)
    
    # Individual Netowrk
    Tot_net = indNet_function(sampled)
    IndNet = Tot_net[[1]]
    DiffNet = Tot_net[[2]]
    if (dim(IndNet)[1] == 1){
      print("ciao")
      IndNet = t(IndNet)
      colnames(IndNet) = "Gene1_Gene2"
      DiffNet = t(DiffNet)
      colnames(DiffNet) = "Gene1_Gene2"
    }
    dim(IndNet)
    
    
    ###########
    
    list_results_LOO_MULTI = evaluate_LOO_multivariate_threhsold(element$n,element$m,rep, sampled, element$dim)
    min_p_for_pers = apply(list_results_LOO_MULTI$p_values,MARGIN =  1, function(x) min(x) )
    real_min_p = ifelse(min_p_for_pers > 0.5,1-min_p_for_pers, min_p_for_pers )
    roc_Loo_multi<- roc(response = is_out, predictor = real_min_p,direction = ">")
    p = ncol(IndNet)
    n = nrow(IndNet)
    Loo = evaluate_LOO(element$n,element$m,rep,sampled)
    roc_Loo<- roc(response = is_out, predictor = Loo,direction = "<")
    auc(roc_Loo)
    
    ###########
    
    # kimn = min(log(n),p+1) ; kmax =  max(log(n),p+1)
    print("############################")
    print(min(log(n),p+1))
    print(max(log(n),p+1))
    knn.agg_log_N_P          = evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(log(n),p+1), k_max =  max(log(n),min(p+1,50)) )
    knn.agg_log_N_P_diff_net = evalute_knn(is_out = is_out, IndNet = DiffNet,k_min =  min(log(n),p+1), k_max =  max(log(n),min(p+1,50)) )
    
    # knn.agg_log_N_P = evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(log(n),p+1), k_max =  max(log(n),p+1) )
    #  knn.agg_log_N_P = evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(log(n),p+1), k_max =  max(log(n),p+1) )
    roc_KNN_log_N_P<- roc(response = is_out, predictor = knn.agg_log_N_P,direction = "<")
    auc(roc_KNN_log_N_P)
    roc_KNN_log_N_P_diff_net <- roc(response = is_out, predictor = knn.agg_log_N_P_diff_net,direction = "<")
    auc(roc_KNN_log_N_P_diff_net)
    
    ############
    
    print( min(sqrt(n),5)) 
    print( max(sqrt(n),5))
    knn.agg_5_sqrt_N =          evalute_knn(is_out = is_out, IndNet = IndNet,k_min =  min(sqrt(n),5), k_max =  max(sqrt(n),5)  )
    knn.agg_5_sqrt_N_diff_net = evalute_knn(is_out = is_out, IndNet = DiffNet,k_min =  min(sqrt(n),5), k_max =  max(sqrt(n),5)  )
    
    roc_KNN_5_sqrt_N <- roc(response = is_out, predictor = knn.agg_5_sqrt_N, direction = "<")
    auc(roc_KNN_5_sqrt_N)
    roc_KNN_5_sqrt_N_diff_net <- roc(response = is_out, predictor = knn.agg_5_sqrt_N_diff_net, direction = "<")
    auc(roc_KNN_5_sqrt_N_diff_net)
    
    ############
    
    optics_res_k_mean          = evaluate_optics(X = IndNet, k = mean(c(p+1,log(n))) )
    optics_res_k_mean_diff_net = evaluate_optics(X = DiffNet, k = mean(c(p+1,log(n))) )

    roc_optics_k_mean <- roc(response = is_out, predictor = optics_res_k_mean, direction = "<")
    auc(roc_optics_k_mean)
    
    roc_optics_k_mean_diff_net <- roc(response = is_out, predictor = optics_res_k_mean_diff_net, direction = "<")
    auc(roc_optics_k_mean_diff_net)
    
    ######
    
    optics_res_k_5          = evaluate_optics(X = IndNet, k = 5 )
    optics_res_k_5_diff_net = evaluate_optics(X = DiffNet, k = 5 )
    
    roc_optics_k_5 <- roc(response = is_out, predictor = optics_res_k_5, direction = "<")
    auc(roc_optics_k_5)
    
    roc_optics_k_5_diff_net <- roc(response = is_out, predictor = optics_res_k_5_diff_net, direction = "<")
    auc(roc_optics_k_5_diff_net)
    
    ######
    
    optics_res_sqrt_n          = evaluate_optics(X = IndNet, k = sqrt(n) )
    optics_res_sqrt_n_diff_net = evaluate_optics(X = DiffNet, k = sqrt(n) )
    
    
    roc_optics_sqrt_n <- roc(response = is_out, predictor = optics_res_sqrt_n, direction = "<")
    auc(roc_optics_sqrt_n)
    
    roc_optics_sqrt_n_diff_net <- roc(response = is_out, predictor = optics_res_sqrt_n_diff_net, direction = "<")
    auc(roc_optics_sqrt_n_diff_net)
    
    #############
    
    distance_vector_OTS          = evaluate_OTS(IndNet)
    distance_vector_OTS_diff_net = evaluate_OTS(DiffNet)
    
    
    roc_OTS<- roc(response = is_out, predictor = as.numeric(distance_vector_OTS$DIST), direction = "<")
    auc(roc_OTS)
    
    roc_OTS_diff_net<- roc(response = is_out, predictor = as.numeric(distance_vector_OTS_diff_net$DIST), direction = "<")
    auc(roc_OTS_diff_net)
    
    ######

    distance_vector_OTS_cosine          = evaluate_OTS_cosine(IndNet)
    distance_vector_OTS_diff_net_cosine = evaluate_OTS_cosine(DiffNet)
    
    
    roc_OTS_cosine<- roc(response = is_out, predictor = as.numeric(distance_vector_OTS_cosine$DIST), direction = ">")
    auc(roc_OTS_cosine)
    
    roc_OTS_diff_net_cosine<- roc(response = is_out, predictor = as.numeric(distance_vector_OTS_diff_net_cosine$DIST), direction = ">")
    auc(roc_OTS_diff_net_cosine)
    
    ######
    
    
    distance_matrices = matrix(NA, nrow = nrow(IndNet) , min(20,ncol = nrow(IndNet)/10 ))
    for ( t in seq(1,min(20, nrow(IndNet)/10), by =2 ) )
    {
      distance_matrices[,t] = evaluate_OTS_cosine(IndNet)$DIST 
      distance_matrices[,t+1] = evaluate_OTS(IndNet)$DIST 
      
    }

    distance_matrices = apply(distance_matrices,2, as.numeric)
    
    cos_matrices = distance_matrices[,seq_len(ncol(distance_matrices)) %% 2 ==1]
    median_cos_matrices = apply(cos_matrices,1, median)
    
    euc_matrices = distance_matrices[,seq_len(ncol(distance_matrices)) %% 2 ==0]
    median_euc_matrices = apply(euc_matrices,1, median)
    
    
    min_max_median_euc = (median_euc_matrices - min(median_euc_matrices) ) / ( max(median_euc_matrices) - min(median_euc_matrices) ) 
    min_max_median_cos = 1 - ( (median_cos_matrices - min(median_cos_matrices) ) / ( max(median_cos_matrices) - min(median_cos_matrices) ) )
    
    glob_avg_OTS = ( min_max_median_euc + min_max_median_cos ) / 2
    
    if (element$dim == 2){
      auc_roc_OTS_cosine_med = NA
    } else {
      roc_OTS_cosine_med <- roc(response = is_out, predictor = median_cos_matrices, direction = ">")
      auc_roc_OTS_cosine_med = auc(roc_OTS_cosine_med)
    }
    
    roc_OTS_euc_med <- roc(response = is_out, predictor = median_euc_matrices, direction = "<")
    auc(roc_OTS_euc_med)
    
    if (element$dim == 2){
      auc_roc_OTS_glob_avg_OTS = NA
    } else {
      roc_OTS_glob_avg_OTS <- roc(response = is_out, predictor = glob_avg_OTS, direction = "<")
      auc_roc_OTS_glob_avg_OTS = auc(roc_OTS_glob_avg_OTS)
    }
    
    
    
    
    
    ######
    
    score_spoutlier               <- qsp(IndNet, 20,normalize = FALSE)
    score_spoutlier_diff_net      <- qsp(DiffNet, 20,normalize = FALSE)
    score_spoutlier_diff_net_norm <- qsp(DiffNet, 20,normalize = TRUE)
    
    roc_spoutlier <- roc(response = is_out, predictor = score_spoutlier, direction = "<")
    auc(roc_spoutlier)
    
    roc_spoutlier_diff_net <- roc(response = is_out, predictor = score_spoutlier_diff_net, direction = "<")
    auc(roc_spoutlier_diff_net)
    
    roc_spoutlier_diff_net_norm <- roc(response = is_out, predictor = score_spoutlier_diff_net_norm, direction = "<")
    auc(roc_spoutlier_diff_net_norm)
    
    ##############
    
    
    if (element$dim == 2){
      Z_Values = evaluate_deltaPCC(sampled)
      roc_DeltaPCC<- roc(response = is_out, predictor = Z_Values, direction = "<")
      auc_DPCC = auc(roc_DeltaPCC)
    }   else {auc_DPCC = NA}
    
    
    #############
    if (dim(IndNet)[2] < dim(IndNet)[1] ) ## 
    {
      score_cook          = evaluate_Cook_distance(IndNet)
      score_cook_diff_net = evaluate_Cook_distance(DiffNet)
      
      # DiffNet[1:5]
      # IndNet[1:5]
      
      max_score_cook = apply(score_cook, 1, max ) 
      max_score_cook_diff_net = apply(score_cook_diff_net, 1, max ) 
      
      roc_cook_max <- roc(response = is_out, predictor = max_score_cook, direction = "<")
      roc_cook_max_diff_net <- roc(response = is_out, predictor = max_score_cook_diff_net, direction = "<")
      
      
      auc_roc_cook_max = auc(roc_cook_max)
      auc_roc_cook_max_diff_net = auc(roc_cook_max_diff_net)
      
      avg_score_cook = apply(score_cook, 1, mean ) 
      avg_score_cook_diff_net = apply(score_cook_diff_net, 1, mean ) 
      
      roc_cook_avg <- roc(response = is_out, predictor = avg_score_cook, direction = "<")
      roc_cook_avg_diff_net <- roc(response = is_out, predictor = avg_score_cook_diff_net, direction = "<")
      
      auc_roc_cook_avg = auc(roc_cook_avg)
      auc_roc_cook_avg_diff_net = auc(roc_cook_avg_diff_net)
      
      
      med_score_cook = apply(score_cook, 1, median ) 
      med_score_cook_diff_net = apply(score_cook_diff_net, 1, median ) 
      
      roc_cook_med <- roc(response = is_out, predictor = med_score_cook, direction = "<")
      roc_cook_med_diff_net <- roc(response = is_out, predictor = med_score_cook_diff_net, direction = "<")
      
      auc_cook_med = auc(roc_cook_med)
      auc_roc_cook_med_diff_net = auc(roc_cook_med_diff_net)
    } else {
      auc_roc_cook_max = NA
      auc_roc_cook_max_diff_net = NA
      auc_roc_cook_avg = NA
      auc_roc_cook_avg_diff_net = NA
      auc_roc_cook_med = NA
      auc_roc_cook_med_diff_net = NA
      
    }
    
   
    
    
    
    
    
    
    
    
    array_res[i,,j]= c(auc(roc_Loo_multi), auc(roc_Loo), auc(roc_KNN_log_N_P), auc(roc_KNN_log_N_P_diff_net),  auc(roc_KNN_5_sqrt_N) ,
                       auc(roc_KNN_5_sqrt_N_diff_net),  auc(roc_optics_k_mean), auc(roc_optics_k_mean_diff_net),
                       auc(roc_optics_k_5), auc(roc_optics_k_5_diff_net), auc(roc_optics_sqrt_n), auc(roc_optics_sqrt_n_diff_net), 
                       auc(roc_OTS), auc(roc_OTS_diff_net), auc(roc_OTS_cosine), auc(roc_OTS_diff_net_cosine),
                       auc_roc_OTS_cosine_med, auc(roc_OTS_euc_med), auc_roc_OTS_glob_avg_OTS, 
                       auc(roc_spoutlier), auc(roc_spoutlier_diff_net), auc(roc_spoutlier_diff_net_norm), auc_DPCC ,
                       auc_roc_cook_max, auc_roc_cook_max_diff_net, auc_roc_cook_avg, auc_roc_cook_avg_diff_net,
                       auc_roc_cook_med, auc_roc_cook_med_diff_net)    # preds = ifelse(list_results$Refusal == (element$dim^2 - element$dim) /2,0,1 )
    
    
    
    
  }
  
  print(j)
  
  median_results = apply(array_res, MARGIN = c(1,2), function(x) median(x,na.rm = TRUE) )
  results_multi = cbind(full_GR, median_results)
  colnames(results_multi) =   c("auc(roc_Loo_multi)","auc(roc_Loo)","auc(roc_KNN_log_N_P)","auc(roc_KNN_log_N_P_diff_net)","auc(roc_KNN_5_sqrt_N)",
                                "auc(roc_KNN_5_sqrt_N_diff_net)","auc(roc_optics_k_mean)","auc(roc_optics_k_mean_diff_net)",
                                "auc(roc_optics_k_5)","auc(roc_optics_k_5_diff_net)","auc(roc_optics_sqrt_n)","auc(roc_optics_sqrt_n_diff_net)", 
                                "auc(roc_OTS)","auc(roc_OTS_diff_net)","auc(roc_OTS_cosine)","auc(roc_OTS_diff_net_cosine)",
                                "auc(roc_OTS_cosine_med)","auc(roc_OTS_euc_med)","auc(roc_OTS_glob_avg_OTS)",
                                "auc(roc_spoutlier)","auc(roc_spoutlier_diff_net)","auc(roc_spoutlier_diff_net_norm)","auc_DPCC",
                                "auc(roc_cook_max)","auc(roc_cook_max_diff_net)","auc(roc_cook_avg)","auc(roc_cook_avg_diff_net)",
                                "auc(roc_cook_med)","auc(roc_cook_med_diff_net)")
  

  
  getwd()
  
  setwd("/massstorage/URT/GEN/BIO3/Federico/High_dimensional_outlier/Results_complete_run")
  aa=  paste0("Results_all_methods", j, ".txt")  
  write.csv(results_multi,aa)
}
dim(array_res)
median_results = apply(array_res, MARGIN = c(1,2), median)
results_multi = cbind(full_GR, median_results)
colnames(results_multi) = c("auc(roc_Loo_multi)","auc(roc_Loo)","auc(roc_KNN_log_N_P)","auc(roc_KNN_log_N_P_diff_net)","auc(roc_KNN_5_sqrt_N)",
                            "auc(roc_KNN_5_sqrt_N_diff_net)","auc(roc_optics_k_mean)","auc(roc_optics_k_mean_diff_net)",
                            "auc(roc_optics_k_5)","auc(roc_optics_k_5_diff_net)","auc(roc_optics_sqrt_n)","auc(roc_optics_sqrt_n_diff_net)", 
                            "auc(roc_OTS)","auc(roc_OTS_diff_net)","auc(roc_OTS_cosine)","auc(roc_OTS_diff_net_cosine)",
                            "auc(roc_OTS_cosine_med)","auc(roc_OTS_euc_med)","auc(roc_OTS_glob_avg_OTS)",
                            "auc(roc_spoutlier)","auc(roc_spoutlier_diff_net)","auc(roc_spoutlier_diff_net_norm)","auc_DPCC",
                            "auc(roc_cook_max)","auc(roc_cook_max_diff_net)","auc(roc_cook_avg)","auc(roc_cook_avg_diff_net)",
                            "auc(roc_cook_med)","auc(roc_cook_med_diff_net)")
getwd()

setwd("/massstorage/URT/GEN/BIO3/Federico/High_dimensional_outlier/Results_complete_run")

write.csv(results_multi,"Results_all_methods.txt")

