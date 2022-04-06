library("here")

source(here::here("f_gene_algo.R"))
source(here::here("f_find_lambda.R"))

# function for backtest ---------------------------------------------------
f_backtest <- function(dfmat, IN, keywords_new, keywords0,
                       df_doc_month, mat_doc_month, vec_month, vec_counts_month,
                       # parameters for Genetic Algorithms 
                       popSize = 50, maxiter = 200, run = 30,
                       pmutation = 0.3, parallel = FALSE, 
                       # parameters for EPU calculation 
                       n_year = 2, dn_band = 1, up_band = 10,
                       # other 
                       n_sim = 1000) {
  
  # dfmat ==> dfm matrix from newspapers ==> counts for features and articles 
  #           need to be sorted 
  # IN ==> all inputs
  
  ## 1. 
  # extract Inputs 
  y_object <- IN$y
  ytype <- IN$ytype
  date_train <- IN$date_train
  date_cv <- IN$date_cv
  name_obj <- IN$model
  name <- IN$name
  seed <- IN$seed
  
  # trim the dfmat
  dfmat_to_use <- dfm_select(dfmat, pattern = unlist(keywords_new))
  
  # transform y 
  y_object <- f_price2rets(y_object, ytype)
  
  # dataset for train 
  x_train <- dfm_subset(dfmat, dfmat$dates <= date_train)
  
  # vec_month and y_object may have different length when y_object is converted to returns 
  idx_train <- y_object[,"date"] <= date_train
  idx_train_month <- vec_month <= date_train
  idx_doc_train <- df_doc_month[, "month"] <= date_train
  
  y_object_train <- y_object[idx_train, ]
  mat_doc_month_train <- mat_doc_month[idx_train_month, idx_doc_train]
  vec_month_train <- vec_month[idx_train_month]
  vec_counts_month_train  <- vec_counts_month[idx_train_month]
  
  # dataset for cross validation  
  x_cv <- dfm_subset(dfmat, (dfmat$dates <= date_cv) & (dfmat$dates > date_train))
  y_object_cv <- y_object[(y_object[,"date"] <= date_cv) & (y_object[,"date"]  > date_train), ]
  
  # dataset for test 
  x_test <- dfm_subset(dfmat, dfmat$dates > date_cv)
  
  idx_test <- y_object[,"date"] > date_cv
  idx_test_month <- vec_month > date_cv
  idx_doc_test <- df_doc_month[, "month"] > date_cv

  y_object_test <- y_object[idx_test, ]
  mat_doc_month_test <- mat_doc_month[idx_test_month, idx_doc_test]
  vec_month_test <- vec_month[idx_test_month]
  vec_counts_month_test  <- vec_counts_month[idx_test_month]
  
  ## 2. 
  # find list of lambdas 
  lst_lambda <- f_find_lambda(x_train, keywords_new, y_object_train,
                              mat_doc_month_train, vec_month_train, vec_counts_month_train,
                              # parameters for Genetic Algorithms
                              popSize = popSize, maxiter = maxiter, run = run,
                              pmutation = pmutation, parallel = parallel,
                              # parameters for EPU calculation
                              dn_band = dn_band, up_band = up_band, n_year = n_year,
                              ytype = ytype, name_obj = name_obj,
                              # number of lambdas
                              n.lambda = 10)
  
  ## 2. 
  # initialize to store tracking error value for different lambda
  mat_te <- matrix(0, 
                   2, 
                   length(lst_lambda))
  # set names
  rownames(mat_te) <- c("train", "cv")
  colnames(mat_te) <- lst_lambda
  
  # initialize to store correlation value for different lambda
  mat_cor <- matrix(NaN, 
                    2, 
                    length(lst_lambda))
  # set names
  rownames(mat_cor) <- c("train", "cv")
  colnames(mat_cor) <- lst_lambda
  
  # initialize to store correlation value for different lambda
  mat_mse <- matrix(NaN, 
                    2, 
                    length(lst_lambda))
  # set names
  rownames(mat_mse) <- c("train", "cv")
  colnames(mat_mse) <- lst_lambda
  
  
  # initialize binary keywords
  # 20 candidates, different lambda 
  mat_keywords_binary <- matrix(NaN, 
                                length(unlist(keywords_new)), 
                                length(lst_lambda))
  
  # set up names 
  rownames(mat_keywords_binary) <- unlist(keywords_new)
  colnames(mat_keywords_binary) <- lst_lambda
  
  ## 3. 
  # initialize to store results for different lambda
  lst_res_lambda <- list()
  
  # loop for different penalization coefficient
  for (lambda in lst_lambda) {
    print(paste0(lambda))
    # run the genetic algorithms for the training set 
    lst <- f_ga(
      x_train, keywords_new, y_object_train,
      mat_doc_month_train, vec_month_train, vec_counts_month_train,
      # parameters for Genetic Algorithms
      popSize = popSize, maxiter = maxiter, run = run,
      pmutation = pmutation, parallel = parallel,
      # parameters for EPU calculation
      dn_band = dn_band, up_band = up_band, lambda = lambda,
      n_year = n_year, ytype = ytype, name_obj = name_obj, 
      seed = seed)
    
    # final keywords in the training set 
    keywords_f <- lst$keywords_f
    
    # set chosed keywords to 1 in matrix 
    mat_keywords_binary[, as.character(lambda)][unlist(keywords_f)] <- 1 
    
    # binary matrix for final keywords   
    mat_keywords_f <- f_new_keywords_mat(lst$x_f, lst$v_loc)
    
    # obtain EPU index with final keywords 
    epu_all <- f_sentiment_cal_dfm_fast(dfmat_to_use, mat_keywords_f,
                                        mat_doc_month, vec_month, vec_counts_month,
                                        ytype = ytype)
    
    
    # when EPU count is 0, there would be Inf when we calculate the return of EPU index
    # in those case, we simply ignore this final results 
    if (sum(epu_all[, "EPU_index"]) %in% c(Inf, NaN)) {
      print("epu not valid")
    } else {
      # EPU index in the train set 
      epu_train <- epu_all[epu_all[, "date"] <= date_train, ]
      lst[["epu_train"]] <- epu_train
      
      # # y_level in train set 
      # y_level_train <- IN$y[IN$y[, "date"] <= date_train, ]
      
      # EPU index in the cross validation 
      epu_cv <- epu_all[(epu_all[, "date"] > date_train) & (epu_all[, "date"] <= date_cv), ]
      lst[["epu_cv"]] <- epu_cv
      
      # # y_level in train set 
      # y_level_cv <- IN$y[(IN$y[, "date"] > date_train) & (IN$y[, "date"] <= date_cv), ]
      
      # calculate the correlation score --- 
      mat_cor["train", as.character(lambda)] <- f_correlation_cal_dfm(epu_train, y_object_train)
      mat_cor["cv", as.character(lambda)] <-  f_correlation_cal_dfm(epu_cv, y_object_cv)  
      
      # calculate the tracking error score --- 
      mat_te["train", as.character(lambda)] <- f_te_cal_dfm(epu_train, y_object_train)
      mat_te["cv", as.character(lambda)] <-   f_te_cal_dfm(epu_cv, y_object_cv)  
      
      # calculate the mean square root error score --- 
      mat_mse["train", as.character(lambda)] <- f_mse_cal_dfm(epu_train, y_object_train)
      mat_mse["cv", as.character(lambda)] <-   f_mse_cal_dfm(epu_cv, y_object_cv)
      
      # if (sum(epu_cv[, "EPU_index"]) %in% c(Inf, NaN)) {
      #   mat_mse["cv", as.character(lambda)] <- NaN
      # } else {
      #   mat_mse["cv", as.character(lambda)] <-   f_mse_cal_dfm(epu_cv, y_object_cv)
      # }
    }
    # save results 
    lst_res_lambda[[as.character(lambda)]] <- lst
  }
  
  ## 4. 
  # select best lambda through cross validation  
  if (name_obj == "te") {
    # the one with lowest tracking error score 
    lambda_best <- names(which.min(mat_te["cv", ]))
    
  } else if (name_obj == "mse") {
    # the one with lowest mean square root error score 
    lambda_best <- names(which.min(mat_mse["cv", ]))
    
  } else if (name_obj == "cor") {
    # the one max abs cor 
    if (grepl("indpro", name)) {
      # negative relationship 
      # so min cor
      lambda_best <- names(which.min(mat_cor["cv", ]))
    } else {
      # positive relationship 
      # so max cor 
      lambda_best <- names(which.max(mat_cor["cv", ]))
    }
  }
  print(lambda_best)
  
  # list with best lambda 
  lst_f_best <- lst_res_lambda[[as.character(lambda_best)]]
  
  # the best keywords 
  keywords_f_best <- lst_f_best$keywords_f
  
  # binary matrix for final keywords   
  mat_keywords_f <- f_new_keywords_mat(lst_f_best$x_f, lst_f_best$v_loc)
  
  # obtain epu index with final keywords 
  epu_all <- f_sentiment_cal_dfm_fast(dfmat_to_use, mat_keywords_f, 
                                      mat_doc_month, vec_month, vec_counts_month,
                                      ytype = ytype)
  
  # epu_all <- f_sentiment_cal_dfm_fast(dfmat_to_use, mat_keywords_f, 
  #                                     mat_doc_month, vec_month, vec_counts_month,
  #                                     ytype = "level")
  
  # EPU index in the test period 
  epu_test <- epu_all[epu_all[, "date"] > date_cv, ]
  
  # test performance 
  # correlation
  cor_test <- f_correlation_cal_dfm(epu_test, y_object_test) 
  # tracking error 
  te_test <- f_te_cal_dfm(epu_test, y_object_test)
  # mse 
  mse_test <- f_mse_cal_dfm(epu_test, y_object_test)
  
  ## 5.
  # plot 
  # common_prefix
  common_prefix <- IN$name
  
  # obtain epu index with final keywords ==> price series 
  epu_all <- f_sentiment_cal_dfm_fast(dfmat_to_use, mat_keywords_f, 
                                      mat_doc_month, vec_month, vec_counts_month,
                                      ytype = F)
  
  # set group 
  epu_all$group <- NA 
  epu_all <- epu_all %>% 
    mutate(group = group %>% replace(date <= date_train, "epu_train") %>%
             replace((date > date_train) & (date <= date_cv), "epu_cv") %>%
             replace(date > date_cv, "epu_test")
    ) 
  
  # EPU index with original keywords from the paper 
  epu_original <- f_sentiment_cal_dfm(dfmat, keywords0)
  
  # add orginal keywords
  epu_all <- bind_rows(epu_all, epu_original)
  
  # set group 
  epu_all <- epu_all %>% replace_na(list(group = "epu_original"))
  
  # line plot of EPU vs. y_object 
  f_ggplot_epu_y(epu_all, tibble(IN$y), common_prefix)
  
  # 6. 
  # Comparison with random generation of solutions 
  # based on 20 candidate keywords per category 
  
  # set seed
  set.seed(seed)
  
  # generate random solutions
  x_rnd <- matrix(sample(c(0,1),
                         dim(mat_keywords_f)[1] * n_sim,
                         replace = TRUE),
                  nrow = n_sim)
  
  # 
  if (ytype == "level") {
    y_object_test_s <- y_object_test
  } else {
    y_object_test_s <- y_object_test[2:dim(y_object_test)[1],]
  }
  
  # calculate objective score
  # set lambda = 0 to get the absolution correlation
  scores_rnd <- apply(x_rnd, 1, f_objective_dfm_fast,
                      datasource = dfm_select(x_test,
                                              pattern = unlist(keywords_new)),
                      mat_doc_month = mat_doc_month_test, 
                      vec_month = vec_month_test, 
                      vec_counts_month = vec_counts_month_test, 
                      y_object = y_object_test_s,
                      v_loc = lst_f_best$v_loc, 
                      lambda = 0,
                      n_year = n_year,
                      ytype = ytype)

  
  # only choose thoses that meet the criteria
  scores_rnd <- scores_rnd[scores_rnd > -1]
  
  # score from GA algorithm
  score_ga <- abs(round(f_correlation_cal_dfm(epu_test, y_object_test),2))
  
  # quantile 
  q_95 <- quantile(scores_rnd, probs = c(0.95))
  
  # histogram
  file_name <- paste0(common_prefix, "_hist")
  folder_name <- "results"
  png(filename  = here::here(folder_name, paste0(file_name, ".png")),
      width = 8, height = 6, unit = "in", res = 300)
  
  # histogram
  hist(scores_rnd,
       breaks = 30,
       main = file_name,
       xlab = "score",
       xlim = c(min(scores_rnd), (max(max(scores_rnd), score_ga) + 0.1))
  )
  
  # Add a box to the current histogram
  box()
  # Add vertical lines corresponding to 1st, 2nd and 3rd quartiles
  abline(v = c(q_95,
               score_ga),
         lty = 2, ## line type: 2 = dashed
         lwd = 2.5, ## line width: the largher, the thicker
         col = c("red","blue"))
  legend("topleft", c("0.95", "GA"), lty = 2, col = c("red","blue"))
  
  dev.off()
  
  # 7. 
  # Add noise to Genetic Algorithm 
  # based on best solution of 20 candidate keywords per category 
  
  # we can add the additional unrelated keywords to see if our algorithm chooses 
  # right ones 
  # eg. apple, green, basketball ... 
  
  keywords_picked <- list(
    c("apple", "banana", "fraction", "normal", "pen"),
    c("book", "green", "yellow", "girl", "men"),
    c("cow", "small", "user", "team", "spider"))
  
  # only choose the best keywords
  keywords_new_noise <- mapply(f_add_keywords, keywords_f_best, keywords_picked,
                               SIMPLIFY = FALSE) # force to return list 
  
  # rerun the genetic algorithms 
  lst_noise <- f_ga(
    x_train, keywords_new_noise, y_object_train,
    mat_doc_month_train, vec_month_train, vec_counts_month_train,
    # parameters for Genetic Algorithms
    popSize = popSize, maxiter = maxiter, run = run,
    pmutation = pmutation, parallel = parallel,
    # parameters for EPU calculation
    dn_band = dn_band, up_band = up_band, lambda = as.numeric(lambda_best),
    n_year = n_year, ytype = ytype, name_obj = name_obj,
    seed = seed)
  

  # lst_noise <- f_ga(
  #   dfmat, keywords_new_noise, y_object,
  #   mat_doc_month, vec_month, vec_counts_month,
  #   # parameters for Genetic Algorithms
  #   popSize = popSize, maxiter = maxiter, run = run,
  #   pmutation = pmutation, parallel = parallel,
  #   # parameters for EPU calculation
  #   dn_band = dn_band, up_band = up_band, lambda = as.numeric(lambda_best),
  #   n_year = n_year, ytype = ytype, name_obj = name_obj,
  #   seed = seed)
  
  # final keywords - noisy 
  keywords_f_noisy <- lst_noise$keywords_f
  
  # correlation score - with noise

  # check if it contains the noise keywords 
  lst_res_lambda[["contain_noise"]] <- unlist(keywords_picked)[
    unlist(keywords_picked) %in% unlist(keywords_f_noisy)]
  
  # save 
  lst_res_lambda[["lst_noise"]] <- lst_noise
  
  # save results 
  # lambda_best
  lst_res_lambda[["lambda_best"]] <- lambda_best
  # lst_lambda
  lst_res_lambda[["lst_lambda"]] <- lst_lambda
  # keywords_f_best
  lst_res_lambda[["keywords_f_best"]] <- keywords_f_best
  # mat_keywords_binary - results keywords 
  lst_res_lambda[["mat_keywords_binary"]] <- mat_keywords_binary
  # mse for different lambdas
  lst_res_lambda[["mat_mse"]] <- mat_mse
  # tracking error for different lambdas
  lst_res_lambda[["mat_te"]] <- mat_te
  # correlation for different lambdas
  lst_res_lambda[["mat_cor"]] <- mat_cor
  # test 
  lst_res_lambda[["epu_test"]] <- epu_test
  lst_res_lambda[["epu_all"]] <- epu_all
  lst_res_lambda[["y_test"]] <- y_object_test
  lst_res_lambda[["cor_test"]] <- cor_test
  lst_res_lambda[["cor_q95"]] <- q_95
  lst_res_lambda[["scores_rnd"]] <- scores_rnd
  lst_res_lambda[["te_test"]] <- te_test
  lst_res_lambda[["mse_test"]] <- mse_test
  
  # lm test 
  lm_res <- lm(as.matrix(epu_test[,"EPU_index"]) ~ y_object_test[,"value"])
  lst_res_lambda[["lm_res"]] <- lm_res
  
  return(lst_res_lambda)
}


