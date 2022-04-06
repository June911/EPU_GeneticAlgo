library("dplyr")
library("Rcpp")
sourceCpp("matrix_mul.cpp")

# sentiment_calculation with dfm object ---------------------------------
# dfm ==> quanteda document-feature matrix 

f_sentiment_cal_dfm <- function(dfmat_to_use, keywords, 
                                n_year = 2, is_dif = FALSE) {
  # n_year ==> number of years to standardize the series 
  # is_dif ==> if True, give returns series; if False, give price series 
  
  # look up keywords in the dfm object ==> !!!!take times to run 
  # !!! this method is not working except unigram keyword 
  #     for example, the keyword "white house" would only be considered as "white" & "house"
  #     but not "white house"
  dfmat <- dfm_lookup(dfmat_to_use, dictionary(keywords), valuetype = "glob")
  
  # counts if appear at least one keywords in all three topics
  EPU <- tibble(month = dfmat$month, EPU_index = rowSums(dfmat > 0) >= 3)
  # EPU <- tibble(month = dfmat$month, EPU_index = rowSums(dfmat) >= 3)
  
  # monthly aggregation ===> !!!!take times, but fastest way I found
  # aggregate(EPU_index~month, data=EPU, mean)
  sm <- EPU %>% 
    group_by(date = EPU$month) %>% 
    dplyr::summarize(EPU_index = mean(EPU_index))
  
  # Calculate standard deviation 
  # before certain date 
  sm_before <- subset(sm, date < sm$date[1] %m+% years(n_year))
  sd_epu <- sd(sm_before[["EPU_index"]])
  
  # it's possible that sd_epu = 0
  if (sd_epu == 0){
    return(-10)
  } else {
    # standardize to a standard deviation of 1 
    sm$EPU_index <- sm$EPU_index / sd_epu
    # sm$EPU_index <- scale(sm$EPU_index, center = FALSE, scale = sds)[, 1]
    
    # calculate mean 
    # before certain date 
    sm_before <- subset(sm, date < sm$date[1] %m+% years(n_year))
    mean_epu <- mean(sm_before[["EPU_index"]])
    
    # normalize to a mean of 100 
    sm$EPU_index <- sm$EPU_index / mean_epu * 100 
    
    # if there are 0s in the EPU_index, we assign them to 1
    # because when we take the returns, the 0 would make the returns sereis 
    # returns inf or NaN
    # sm$EPU_index[sm$EPU_index == 0] <- 1
    
  }
  
  if (is_dif) {
    # change the value  to difference 
    # number of periods
    n_periods <- dim(sm)[1]
    # take the return  
    sm$EPU_index[2:n_periods] <- sm$EPU_index[2:n_periods] / sm$EPU_index[1:(n_periods - 1)] - 1
    # take the different 
    # sm$EPU_index[2:n_periods] <- sm$EPU_index[2:n_periods] - sm$EPU_index[1:(n_periods - 1)]
    # exclude first line 
    sm <- sm[2:n_periods, ]
  }
  
  return(sm)
}


# sentiment_calculation with dfm object ---------------------------------
# dfm ==> quanteda document-feature matrix 
# fast way ==> matrix multiplication  !!!

f_sentiment_cal_dfm_fast <- function(dfmat_to_use, mat_keywords, mat_doc_month, 
                                     vec_month, vec_counts_month, 
                                     n_year = 2, ytype = "level") {
  # n_year ==> number of years to standardize the series 
  # is_dif ==> if True, give returns series; if False, give price series 
  
  # look up keywords in the dfm object ==> !!!!take times to run 
  dfmat <- eigenMapMatMult_SPARSE(dfmat_to_use, mat_keywords)
  
  # counts if appear at least one keywords in all three topics
  EPU_count <- as(Matrix(as.matrix(rowSums(dfmat > 0) >= 3), sparse = TRUE), "dgCMatrix")
  # EPU_count <- as(Matrix(as.matrix(rowSums(dfmat) >= 3), sparse = TRUE), "dgCMatrix")
  
  # monthly aggregation 
  # as matrix multiplication 
  sm <- tibble(date = vec_month, 
    EPU_index = as.vector(eigenMapMatMult_SPARSE(mat_doc_month, EPU_count)) / vec_counts_month)
  
  # Calculate standard deviation 
  # before certain date 
  sm_before <- subset(sm, date < sm$date[1] %m+% years(n_year))
  sd_epu <- sd(sm_before[["EPU_index"]])
  
  # it's possible that sd_epu = 0
  if (sd_epu == 0){
    return(-10000)
  } else {
    # standardize to a standard deviation of 1 
    sm$EPU_index <- sm$EPU_index / sd_epu
    
    # calculate mean 
    # before certain date 
    sm_before <- subset(sm, date < sm$date[1] %m+% years(n_year))
    mean_epu <- mean(sm_before[["EPU_index"]])
    
    # normalize to a mean of 100 
    sm$EPU_index <- sm$EPU_index / mean_epu * 100 
    
    # if there are 0s in the EPU_index, we assign them to 1
    # because when we take the returns, the 0 would make the returns sereis 
    # returns inf or NaN
    # sm$EPU_index[sm$EPU_index == 0] <- 1
    
  }
  
  
  if (ytype == "dif") {
    # change the value  to difference 
    # number of periods
    n_periods <- dim(sm)[1]
    # take the different 
    sm$EPU_index[2:n_periods] <- sm$EPU_index[2:n_periods] - sm$EPU_index[1:(n_periods - 1)]
    # exclude first line 
    sm <- sm[2:n_periods, ]
  } else if (ytype == "rets") {
    # change the value to series of returns  
    # number of periods
    n_periods <- dim(sm)[1]
    # take the return  
    sm$EPU_index[2:n_periods] <- sm$EPU_index[2:n_periods] / sm$EPU_index[1:(n_periods - 1)] - 1
    # exclude first line 
    sm <- sm[2:n_periods, ]
  } 
  
  return(sm)
}
