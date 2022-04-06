
library(stringr)
library(gplots)

# analyze results  --------------------------------------------------------

# list of file names 
lst_file <- list.files(here::here("results_data"), full.names = TRUE)

# function to load RData and set names 
loadRData <- function(file_name){
  #loads an RData file, and returns it
  load(file_name)
  get("out")
}


# load all results 
lst_res_all <- list()
i <- 1 
for (file_name in lst_file) {
  print(file_name)
  file_name_splited <- strsplit(file_name, "/")[[1]]
  file_name_fn <- file_name_splited[[length(file_name_splited)]]
  file_name_fn2 <- paste(strsplit(file_name_fn, "_")[[1]][1:3], collapse = "_")
  lst_res_all[[file_name_fn2]] <- readRDS(file_name)
  # lst_res_all[[file_name_fn2]] <- loadRData(file_name)
  i <- i + 1
}


# noise keywords
keywords_picked <- list(
  c("apple", "banana", "fraction", "normal", "pen"),
  c("book", "green", "yellow", "girl", "men"),
  c("cow", "small", "user", "team", "spider"))


# initial mat to store value 
mat <- matrix(NA, nrow = length(names(lst_res_all)), ncol = 6,
              dimnames = list(names(lst_res_all), c("train","cv","test",
                                                    "test95","noise",
                                                    "p_value")))

mat_lambda <- matrix(NA, nrow = length(names(lst_res_all)), ncol = 3,
                dimnames = list(names(lst_res_all), c("lambda.min", "lambda.max", "lambda.selected")))

mat_p <- matrix(NA, nrow = length(names(lst_res_all)), ncol = 1,
              dimnames = list(names(lst_res_all), c("test_p_value")))

mat_keywords <- matrix(NA, nrow = length(names(lst_res_all)), 
                       ncol = dim(lst_res_all[[file_name_fn2]]$mat_keywords_binary)[1],
                       dimnames = list(names(lst_res_all),
                                       rownames(lst_res_all[[file_name_fn2]]$mat_keywords_binary)))

# lm pvalue 
lmp <- function(modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  return(p)
}


for (file_name in names(lst_res_all)) {

  # locate results 
  
  # file_name <- "vix_rets_cor"
  lst <- lst_res_all[[file_name]]
  
  print("-")
  print(file_name)
  print(lst$keywords_f_best)

  # find best keywords 
  lambda_best <- lst$lambda_best
  
  # find correlation 
  mat[file_name, c("train","cv")] <- lst$mat_cor[, lambda_best]
  mat[file_name, c("test")] <- lst$cor_test
  mat[file_name, c("test95")] <- lst$cor_q95
  # mat[file_name, c("train_noise")] <- lst$lst_noise$score_f
  
  # find the contained noise 
  con_contain <- unlist(keywords_picked) %in%  unlist(lst$lst_noise$keywords_f)
  if (sum(con_contain) == 0) {
    mat[file_name, c("noise")] <- FALSE
  } else {
    mat[file_name, c("noise")] <- paste(unlist(keywords_picked)[con_contain], collapse = "_")
  }
  
  # find p value 
  mat_p[file_name, c("test_p_value")] <- lmp(lst$lm_res)
  mat[file_name, c("p_value")] <- lmp(lst$lm_res)
  
  # find keywords 
  mat_keywords[file_name,] <- as.numeric(colnames(mat_keywords) %in% unlist(lst$keywords_f_best))
  
  # find lambdas 
  mat_lambda[file_name,] <- c(min(lst$lst_lambda), max(lst$lst_lambda), lst$lambda_best)
}

# round correlation 
mat[, c("train","cv","test","test95")] <- round(
  as.double(mat[, c("train","cv","test","test95")]), 2)

# heatmap

# set graph position 
lmat <- rbind( c(5,3,4), c(2,1,4))
lhei <- c(1, 4)
lwid <- c(1, 4, 0.5)

heatmap.2(t(mat_keywords[, 1:20]), 
          main="Economy", 
          # srtCol=0, adjCol = c(0.5,1), # make xlabel horizontal 
          srtCol=90,  adjCol = c(0.5,1),
          lmat=lmat, lhei=lhei, lwid=lwid,
          cexRow=1.1, cexCol=1, margins=c(5,5),
          dendrogram=c("none"), Rowv=FALSE, Colv=FALSE, 
          col = rev(grey.colors(2)),
          trace="both",
          tracecol = grey.colors(2)[1],
          hline = F,
          vline = F,
          key = F,
          density.info = c("none"),
)

heatmap.2(t(mat_keywords[, 21:40]), 
          main="Policy", 
          # srtCol=0, adjCol = c(0.5,1), # make xlabel horizontal 
          srtCol=90,  adjCol = c(0.5,1),
          lmat=lmat, lhei=lhei, lwid=lwid,
          cexRow=1.1, cexCol=1, margins=c(5,5),
          dendrogram=c("none"), Rowv=FALSE, Colv=FALSE, 
          col = rev(grey.colors(2)),
          trace="both",
          tracecol = grey.colors(2)[1],
          hline = F,
          vline = F,
          key = FALSE,
          density.info = c("none"),
)

heatmap.2(t(mat_keywords[, 41:60]), 
          main="Uncertainty", 
          # srtCol=0, adjCol = c(0.5,1), # make xlabel horizontal 
          srtCol=90,  adjCol = c(0.5,1),
          lmat=lmat, lhei=lhei, lwid=lwid,
          cexRow=1.1, cexCol=1, margins=c(5,5),
          dendrogram=c("none"), Rowv=FALSE, Colv=FALSE, 
          col = rev(grey.colors(2)),
          trace="both",
          tracecol = grey.colors(2)[1],
          hline = F,
          vline = F,
          key = FALSE,
          density.info = c("none"),
)

