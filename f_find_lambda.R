source(here::here("f_gene_algo.R"))


f_find_lambda <- function(x_train, keywords_new, y_object_train,
                          mat_doc_month_train, vec_month_train, vec_counts_month_train,
                          # parameters for Genetic Algorithms 
                          popSize = 50, maxiter = 200, run = 30,
                          pmutation = 0.3, parallel = FALSE, 
                          # parameters for EPU calculation 
                          n_year = 2, dn_band = 1, up_band = 10,
                          ytype = "rets", name_obj = "cor", seed = 1,
                          # number of lambdas 
                          n.lambda = 10, lambda.min.ratio = 0.1
                          
) {
  

  
  # find lambda.max  --------------------------------------------------------

  # To find the lambda.max, 
  # we first assign the lambda.max to a large number like 10, 
  # and run the Genetic Algorithm to get the final set of selected keywords. 
  # If the final set has only one keyword per topic, 
  # we think the penalization is large enough 
  # and we log scale down the lambda to test its downside limit. 
  # So, we repeat the process until the final set of keywords has more than one 
  # keywords in any dimension and set the lambda.max as current lambda * e. 
  
  # initialize lambda.max 
  lambda.max <- 1
  # initialize condition to break the loop 
  con_break <- FALSE
  cnt_up <- 0 
  cnt_dn <- 0 

  # find lambda.max 
  while (!con_break) {
    # run the Genetic Algorithm to get the final set of selected keywords 
    lst <- f_ga(
      x_train, keywords_new, y_object_train,
      mat_doc_month_train, vec_month_train, vec_counts_month_train,
      # parameters for Genetic Algorithms
      popSize = popSize, maxiter = maxiter, run = run,
      pmutation = pmutation, parallel = parallel,
      # parameters for EPU calculation
      dn_band = dn_band, up_band = up_band, lambda = lambda.max,
      n_year = n_year, ytype = ytype, name_obj = name_obj, 
      seed = seed)
    
    # final binary keywords 
    x_f <- lst$x_f
    
    # number of selected keywords 
    n_x_f <- sum(x_f)
    
    print("-")
    print(name_obj)
    print(paste0("lambda.max = ", lambda.max, ", n_x_f: ", n_x_f))
    print(paste0(lst$keywords_f))
    print(paste0(cnt_dn, ", ", cnt_up))
    
    # number of selected keywords less than 3 
    # only one selected keywords per dimension 
    if (n_x_f <= 3) {
      # count plus 1 
      cnt_dn <- cnt_dn + 1
      # break out condition
      con_break <- ((cnt_dn > 0) & (cnt_up > 0)) | (cnt_dn > 10)
      if (!con_break) {
        # scale down the lambda to test its downside limit
        # lambda.max <- lambda.max / 10 
        lambda.max <- lambda.max / 10 
      }  
    } else {
      # count plus 1 
      cnt_up <- cnt_up + 1
      # break out condition
      con_break <- ((cnt_dn > 0) & (cnt_up > 0)) | (cnt_up > 1)
      
      # scale up the lambda to test its upside limit
      # lambda.max <- lambda.max * 10
      lambda.max <- lambda.max * 10    
      
      # # make no sense for lambda larger than 1 when objective function is correlation 
      # if (name_obj != "cor") {
      #   # count plus 1 
      #   cnt_up <- cnt_up + 1
      #   # break out condition
      #   con_break <- ((cnt_dn > 0) & (cnt_up > 0)) | (cnt_up > 1)
      #   
      #   # scale up the lambda to test its upside limit
      #   # lambda.max <- lambda.max * 10
      #   lambda.max <- lambda.max * 10 
      #   
      #   # if (!con_break) {
      #   #   # scale up the lambda to test its upside limit
      #   #   # lambda.max <- lambda.max * 10
      #   #   lambda.max <- lambda.max * 10 
      #   # } 
      # } else {
      #   con_break <- TRUE
      #   # go back to previous lambda
      #   lambda.max <- lambda.max * 10 
      # }
    }
  }
  
  print("-")
  print("-")
  print(paste0("found lambda max: ", lambda.max))
  
  # find lambda.min  --------------------------------------------------------
  
  # lambda.min
  # Smallest value for lambda, as a fraction of lambda.max
  
  # If nobs > nvars, the default is 0.0001, close to zero. 
  # If nobs < nvars, the default is 0.01
  
  lambda.min <- lambda.max * lambda.min.ratio
  
  # log sequence 
  l.lambda <-  lseq(lambda.min,  lambda.max, length.out = n.lambda)
  print(paste0("l.lambda: ", l.lambda))
  
  return(l.lambda)
  
}