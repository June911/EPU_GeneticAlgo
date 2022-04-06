# Genetic Algorithm -------------------------------------------------------
library("GA")
library("here")

source(here::here("f_new_keywords.R"))
source(here::here("f_sentiment_cal_dfm.R"))

f_ga <- function(datasource, keywords, y_object, 
                 mat_doc_month, vec_month, vec_counts_month, 
                 # parameters for Genetic Algorithms 
                 popSize = 50, maxiter = 100, run = 30,
                 pmutation = 0.3, parallel = FALSE,
                 # parameters for EPU calculation 
                 dn_band = 1, up_band = 10, lambda = 0.1, 
                 n_year = 2, ytype = "level", name_obj = "mse", seed = 1){
  
  # 1. Run Genetic Algorithm with Extended Keywords-----------------------------
  
  # trim the datasource
  datasource_used <- dfm_select(datasource, pattern = unlist(keywords))
  
  # get the feature names 
  feat_names <- featnames(datasource_used)
  
  # number of features 
  # might be less than the keywords 
  # because certain words don"t appear in the data set 
  # in our case n_feat = 51 
  n_feat <- length(feat_names)
  
  # vector to locate topic of the keywords 
  v_loc <- rep(NA, n_feat)
  names(v_loc) <- feat_names
  
  # for topic E
  v_loc[feat_names %in% keywords$E] <- 1
  # for topic P
  v_loc[feat_names %in% keywords$P] <- 2
  # for topic U
  v_loc[feat_names %in% keywords$U] <- 3
  
  # choose objective function
  if (name_obj == "cor") {
    # absolute correlation
    fun <- f_objective_dfm_fast
  } else if  (name_obj == "mse") {
    # mean square error
    fun <- f_objective_dfm_mse_fast
  } else if (name_obj == "te") {
    # tracking error 
    fun <- f_objective_dfm_te_fast
  } else {
    print("unsupport objetive name")
    print("only: cor, mse, te")
  }
  
  # f_objective_dfm_mse_fast(lst$x_f,
  #                          dfm_select(x_train, pattern = unlist(keywords)), 
  #                          mat_doc_month_train, 
  #                          vec_month_train, 
  #                          vec_counts_month_train, 
  #                          y_object_train,
  #                          v_loc, 
  #                          dn_band=1, up_band=10, lambda=0,
  #                          n_year=2, ytype = "rets")
  
  ## Genetic Algorithm ## 
  # run ==> the number of consecutive generations without any improvement in the 
  # best fitness value before the GA is stopped 
  
  # # set defaults for selection and crossover operators of binary search
  # gaControl("binary" = list(selection = "ga_rwSelection", 
  #                           crossover = "gabin_uCrossover"))
  
  
  # initialize list to store results 
  lst <- list()
  
  # Genetic Algorithm in Package GA  
  GA <- ga(type = "binary",
           fitness = fun, 
           # parameters for objective functions 
           datasource = datasource_used,
           mat_doc_month = mat_doc_month, 
           vec_month = vec_month, 
           vec_counts_month = vec_counts_month, 
           y_object = y_object,
           v_loc = v_loc, 
           # parameters for EPU calculation 
           dn_band = dn_band, up_band = up_band, lambda = lambda,
           n_year = n_year, ytype = ytype, 
           # parameters for Genetic Algorithms 
           nBits = n_feat,  # How many keywords in total
           keepBest = FALSE,
           popSize = popSize, maxiter = maxiter, run = run,
           pmutation = pmutation,
           # parallel = parallel,
           parallel = parallel,
           seed = seed,
           # monitor = FALSE
           monitor = TRUE
  )
  
  
  # store results
  lst[["keywords"]] <- keywords
  lst[["v_loc"]] <- v_loc
  lst[["GA"]] <- GA
  # there may be more than one solution
  # we choose the solution with least keywords
  sol <- summary(GA)$solution
  lst[["x_f"]] <- sol[which.min((rowSums(summary(GA)$solution))),]
  lst[["score_f"]] <- GA@fitnessValue
  lst[["keywords_f"]] <-  f_new_keywords(unlist(keywords) %in% feat_names[lst[["x_f"]] == 1],
                                         unlist(keywords), names(keywords))
  
  return(lst)
  
}

# objective function for dfm ------------------------------------
# include matrix multiplication 
f_objective_dfm_fast <- function(keywords_binary,
                                 datasource, 
                                 mat_doc_month, 
                                 vec_month, 
                                 vec_counts_month, 
                                 y_object,
                                 v_loc,
                                 dn_band=1, up_band=10, lambda=0,
                                 n_year=2, ytype = "level"
) {
  ## 1. convert binary keywords to matrix of binary keywords 
  # where columns represent topics 
  mat_keywords <- f_new_keywords_mat(keywords_binary, v_loc)
  
  # In order to add keywords constraints, we add penalty to bad candidates 
  sum_col <- colSums(mat_keywords)
  con_bad_candidate <- sum((sum_col < dn_band) | (sum_col > up_band))
  
  # constraints a 
  # bad candidate ==> directly returns -10 
  if (con_bad_candidate > 0) {
    return(-10000)
  } else {
    ## 2. Sentiment Calculation  ------------------------
    sm <- f_sentiment_cal_dfm_fast(datasource, mat_keywords, 
                                   mat_doc_month, vec_month, vec_counts_month, 
                                   n_year = n_year, ytype = ytype) 
    # constraints b
    # bad candidate ==> directly returns -10 
    # if (typeof(sm) != "list") {
    if ((typeof(sm) != "list") || (sum(sm[, "EPU_index"]) %in% c(Inf, NaN))) {
      return(-10000)
    } else {
      
      ## 3. Score Calculation ------------------------
      score <- f_score_cal_dfm(sm, y_object, lambda, keywords_binary)
      return(score)
    }
  }
}

# objective function for dfm ------------------------------------
# fast way 
# tracking errors 

f_objective_dfm_te_fast <- function(keywords_binary,
                                    datasource, 
                                    mat_doc_month, 
                                    vec_month, 
                                    vec_counts_month, 
                                    y_object,
                                    v_loc, 
                                    dn_band=1, up_band=10, lambda=0,
                                    n_year=2, ytype = "level"
) {
  ## 1. convert binary keywords to matrix of binary keywords 
  # where columns represent topics 
  mat_keywords <- f_new_keywords_mat(keywords_binary, v_loc)
  
  # In order to add keywords constraints, we add penalty to bad candidates 
  sum_col <- colSums(mat_keywords)
  con_bad_candidate <- sum((sum_col < dn_band) | (sum_col > up_band))
  
  # constraints a 
  # bad candidate ==> directly returns -10 
  if (con_bad_candidate > 0) {
    return(-10000)
  } else {
    ## 2. Sentiment Calculation  ------------------------
    sm <- f_sentiment_cal_dfm_fast(datasource, mat_keywords, 
                                   mat_doc_month, vec_month, vec_counts_month, 
                                   n_year = n_year, ytype = ytype) 
    # constraints b
    # bad candidate ==> directly ret
    if ((typeof(sm) != "list") || (sum(sm[, "EPU_index"]) %in% c(Inf, NaN))) {
      return(-10000)
    } else {
      
      ## 3. Score Calculation ------------------------
      score <- f_score_cal_dfm_te(sm, y_object, lambda, keywords_binary)
      
      # smaller the tracking error, better the performance 
      return(-score)
    }
  }
}


# objective function for dfm ------------------------------------
# fast way 
# mean square errors  

f_objective_dfm_mse_fast <- function(keywords_binary,
                                     datasource, 
                                     mat_doc_month, 
                                     vec_month, 
                                     vec_counts_month, 
                                     y_object,
                                     v_loc, 
                                     dn_band=1, up_band=10, lambda=0,
                                     n_year=2, ytype = "level"
) {
  ## 1. convert binary keywords to matrix of binary keywords 
  # where columns represent topics 
  mat_keywords <- f_new_keywords_mat(keywords_binary, v_loc)
  
  # In order to add keywords constraints, we add penalty to bad candidates 
  sum_col <- colSums(mat_keywords)
  con_bad_candidate <- sum((sum_col < dn_band) | (sum_col > up_band))
  
  # constraints a 
  # bad candidate ==> directly returns -100000
  if (con_bad_candidate > 0) {
    return(-10000)
  } else {
    ## 2. Sentiment Calculation  ------------------------
    sm <- f_sentiment_cal_dfm_fast(datasource, mat_keywords, 
                                   mat_doc_month, vec_month, vec_counts_month, 
                                   n_year = n_year, ytype = ytype) 
    
    # there might be inf in the EPU index, as we transfer it into returns 
    if ((typeof(sm) != "list") || (sum(sm[, "EPU_index"]) %in% c(Inf, NaN))) {
      return(-10000)
    } else {
      
      ## 3. Score Calculation ------------------------
      score <- f_score_cal_dfm_mse(sm, y_object, lambda, keywords_binary)
      
      # smaller the mse, better the performance 
      return(-score)
    }
  }
}

# choose bad candidates ---------------------------------------------------

f_bad_candidate <- function(keywords_one_topic, dn_band, up_band) {
  len <- length(keywords_one_topic)
  con <- (len < dn_band) | (len > up_band)
  return (con)
}


# calculate objective score -----------------------------------------------

f_score_cal_dfm <- function(sm, y_object, lambda, keywords_binary) {
  
  # Calculate absolute correlation
  # eg, EPU vs. Industrial production 
  cor_xy <- cor(sm[,"EPU_index"], y_object[,"value"])
  
  # Calculate absolute score after penalization 
  score <- abs(cor_xy[1]) - lambda * sum(keywords_binary)
  return (score)
}

f_score_cal_dfm_te <- function(sm, y_object, lambda, keywords_binary) {
  
  # Calculate tracking errors 
  # variance of difference in returns 
  # eg, EPU vs. Industrial production 
  # te <- var(sm[,"EPU_index"] - y_object[,"value"])
  te <- f_te_cal_dfm(sm, y_object)
  
  # Calculate score after penalization 
  # smaller the tracking error, better the performance 
  # so add penalization 
  score <- te + lambda * sum(keywords_binary)
  return (score)
}

f_score_cal_dfm_mse <- function(sm, y_object, lambda, keywords_binary) {
  
  # Calculate mean square errors 
  mse <- f_mse_cal_dfm(sm, y_object)
  
  # Calculate score after penalization 
  # smaller the mse, better the performance 
  # so add penalization 
  score <- mse + lambda * sum(keywords_binary)
  return (score)
}


# correlation calculation dfm --------------------------------------------------

f_correlation_cal_dfm <- function(sm, y_object) {
  # Calculate correlation
  # eg, EPU vs. Industrial production 
  cor_xy <- cor(sm[,"EPU_index"], y_object[,"value"])
  return (cor_xy[1])
}

# tracking error calculation dfm --------------------------------------------------

f_te_cal_dfm <- function(sm, y_object) {
  # Calculate tracking errors 
  # variance of difference in returns 
  # eg, EPU vs. Industrial production 
  return (var(sm[,"EPU_index"] - y_object[,"value"]))
}

# mean square error calculation dfm --------------------------------------------------
f_mse_cal_dfm <- function(sm, y_object) {
  # Fit the linear regression model 
  mod <- .lm.fit(as.matrix(sm[, "EPU_index"]), y_object[,"value"]) # . ==> in c code
  # return sum square error
  return (mean((mod$residuals)^2))
}

