##-------------------------.main()-------------------------------------------##
# implement code on WSJ dataset 

# clear objects in global environment and clear console
rm(list=ls())
cat("\014")

# Libraries ---------------------------------------------------------------
library("GA")
library("xts")
library("here")
library("plyr")
library("gplots")
library("tidyr")

library("repmis")
library("quanteda")
library("text2vec")
library("lubridate")
library("data.table")
library("sentometrics")
library("fastDummies")
library("parallel")

# functions 
source(here::here("f_backtest.R"))
source(here::here("f_general.R"))
source(here::here("f_word2vec.R"))

# Load data ---------------------------------------------------------------

# load data R
load(here::here("data", "WJS_processed.rda"))

# Here the data souce is a dfm object 
# dfm ==> document-feature matrix
# already cleaned 
datasource <- dat$dfm

# date vector 
dates <- as.Date(dat$date)

# the macro-variable to forecast
vix <- f_process_y_object("VIX.csv", is_rets = FALSE)
unrate <- f_process_y_object("UNRATE.csv", is_rets = FALSE)
indpro <- f_process_y_object("INDPRO.csv", is_rets = FALSE)

# combine y_objects 
y_objects <- Reduce(function(x, y) merge(x, y, by = "date"), list(vix, unrate, indpro))
names(y_objects) <- c("date", "vix", "unrate", "indpro")


# Process data ------------------------------------------------------------

# 1. match the object dates with input dates ----------------------------------
months <- floor_date(dates, "month")
y_objects <- subset(y_objects, date %in% unique(months))

# # match the input dates with object dates
is_in_month <- months %in% y_objects$date
datasource <- dfm_subset(datasource, is_in_month)
dates <- dates[is_in_month]

# set dates as one of the document vectors
datasource$dates <- dates

# set month floor
datasource$month <- floor_date(dates, "month")

# 2. Trim the dfm object ------------------------------------------------------

# we only need the datasource(dfm) to contains all needed keywords features

# Load pretrained GloVe matrix
load(here::here("data", "pretrained_matrix.RData"))
# alternative way if the above line doesnt work 
# pretrained_matrix <- f_get_pretrained_matrix(here::here("data","glove.6B.300d.txt"))

# original keywords 
keywords0 <- list(
  E = c("economy", "economic"),
  P = c("congress", "legislation", "white house",
        "regulation", "deficit", "federal reserve"),
  U = c("uncertainty", "uncertain")
)

# need to split the phrase that have more than one word 
# in order to perform GloVe serach 

# eg. 
# "white house" ==> "white" & "house"
# "federal reserve" ==> "federal" & "reserve"

keywords <- list(
  E = c("economy", "economic"),
  P = c("congress", "legislation", "white", "house",
        "regulation", "deficit", "federal", "reserve"),
  U = c("uncertainty", "uncertain")
)


# Find new keywords (in the similar GloVe dimension)
lst_keywords_new <- f_get_lst_keywords_news()
keywords_new <- lst_keywords_new$`20`

for (topic in names(keywords_new)) {
  keywords_new[[topic]] <- unique(c(keywords0[[topic]],
                                    keywords_new[[topic]]))
  print(length(keywords_new[[topic]]))
}


# noisy keywords 
keywords_picked <- list(
  c("apple", "banana", "fraction", "normal", "pen"),
  c("book", "green", "yellow", "girl", "men"),
  c("cow", "small", "user", "team", "spider"))

# combine all keywords
lst_all_keywords <- c(unname(unlist(keywords0)), 
                      unname(unlist(keywords_new)),
                      unname(unlist(keywords_picked)),
                      c("white house", "federal reserve")
)
lst_all_keywords <- unique(lst_all_keywords)

# Trim the datasource
# the working datasource 
datasource <- dfm_select(datasource, pattern = lst_all_keywords)

# sort datasource in date order
datasource <- datasource[order(datasource$month), ]

# create a map matrix for documents and months 
df_doc_month <- data.frame(doc = docnames(datasource), month = datasource$month)

# create dummy variabels for months 
# n_month * n_doc 

# !! this process take long time, so I did it once, saved the file for later loading 
# mat_doc_month <- dummy_cols(df_doc_month, select_columns = "month")
# mat_doc_month <- Matrix(as.matrix(
#   t(mat_doc_month[, 3:dim(mat_doc_month)[2]])), sparse = TRUE)
# save(mat_doc_month, file = here::here("data", "mat_doc_month"))

load(here::here("data", "mat_doc_month"))

# month vector 
vec_month <-  unique(datasource$month)

# vector that counts in every month 
vec_counts_month <- df_doc_month %>% 
  group_by(date = df_doc_month$month) %>% 
  dplyr::summarize(n = n())
vec_counts_month <- as.vector(vec_counts_month$n)

# Input -------------------------------------------------------------------

# data name 
name_data <- "wsj"

# variable y 
l.y <- c("vix", "unrate", "indpro")

# type of y 
# l.ytype <- c("level", "rets")
l.ytype <- c("rets")

# objective function to choose 
l.model <- c("cor", "te", "mse")
# l.model <- c("cor", "mse")

# Creat backtest list  ----------------------------------------------------

# number of y variable to consider
n.y <- length(l.y)

# (level vs. returns)
n.ytype <- length(l.ytype)

# # number of lambdas <== penalization parameters 
# n.lambda <- length(l.lambda)

# number of used models.
# 1. max (abs(cor) + penalization) 
# 2. max (- mse +  penalization)
# 2. max (- tracking error +  penalization)
n.models <- length(l.model) 

# number of backtest 
n.bt <- n.y * n.ytype  * n.models 

# initialize list to store all setups 
l.bt <- vector("list", n.bt)
l.bt <- list()

# loop to set all setups 
k <- 1 
for (i1 in 1:n.y) {
  for (i2 in 1:n.ytype) {
    for (i3 in 1:n.models) {
      print(k)
      print(paste0(l.y[[i1]], "_", 
                   l.ytype[[i2]], "_", 
                   l.model[[i3]]))
      print((l.model[[i3]] == "te") & (l.ytype[[i2]] == "level"))
      
      if ((l.model[[i3]] == "te") & (l.ytype[[i2]] == "level")) {
        
      } else {
        name <- paste0(l.y[[i1]], "_", 
                       l.ytype[[i2]], "_", 
                       l.model[[i3]])
        y <- y_objects[, c("date", l.y[[i1]])]
        # names(y) <- c("date", "value")
        l.bt[[k]] <- list(
          # seed = k,
          seed = 2022,
          name = name, 
          # lambda = l.lambda,
          y = y,
          ytype = l.ytype[[i2]],
          model = l.model[[i3]],
          # 
          date_train = "2011-12-31", 
          date_cv = "2016-12-31"
          # 
          
        )
        k = k + 1
      }
    }
  }
}
print(length(l.bt))





# backtest ----------------------------------------------------------------

# GA parameters 
maxiter <- 500
run <- 100
parallel <- F # not working, GA package cannot dael with RCPP function 

# EPU calculation parameters 
n_year <- 2 
up_band <- 10

# number of simulations for random solutions 
n_sim <- 1000


f_bt <- function(IN) {
  # # if test one setup with different seed 
  # # the seed should be different 
  # if (is_test_seed) {
  #   seed = IN$seed
  # }
  
  out <- f_backtest(datasource, IN, keywords_new, keywords0, 
                    df_doc_month, mat_doc_month, vec_month, vec_counts_month,
                    # parameters for Genetic Algorithms 
                    popSize = 50, maxiter = maxiter, run = run,
                    pmutation = 0.3, parallel = F, 
                    # parameters for EPU calculation 
                    n_year = n_year, dn_band = 1, up_band = up_band,
                    # other 
                    n_sim = 1000)
  saveRDS(out,
       file = here::here("results_data", paste0(IN$name, "_res_ga.rds")))  
}

# backtest in parallel ---------------------------------------------------------

n.core <- 3
cl <- parallel::makeCluster(n.core)

# load functions 
clusterExport(cl, c("f_new_keywords","f_relist", "f_new_keywords_mat", "f_add_keywords", 
                    "f_sentiment_cal_dfm_fast", "f_score_cal_dfm", 
                    "f_objective_dfm_fast", "f_objective_dfm_mse_fast", "f_objective_dfm_te_fast",
                    "f_score_cal_dfm", "f_score_cal_dfm_te", "f_score_cal_dfm_mse",
                    "f_te_cal_dfm", "f_mse_cal_dfm", "f_correlation_cal_dfm", "f_sentiment_cal_dfm", 
                    "f_objective_dfm_fast", "f_objective_dfm_te_fast", "f_objective_dfm_mse_fast",
                    "f_price2rets", "f_bad_candidate", "f_ggplot_epu_y", 
                    "f_ga", "f_find_lambda", "f_backtest", "lseq"))
# load variables 
clusterExport(cl, c("datasource", "keywords_new", "keywords0", 
                    "df_doc_month", "mat_doc_month", "vec_month", "vec_counts_month", 
                    "maxiter", "run", "n_year", "up_band"))

# load package 
clusterEvalQ(cl, c(library("Matrix"), 
                   library("mypack"),
                   library("dplyr"),
                   library("quanteda"),
                   library("lubridate"),
                   library("GA"),
                   library("here"),
                   library("ggplot2"),
                   library("cowplot"),
                   library("tidyr")))

# record start time 
start_time <- Sys.time()

# run parallel -- backtest 
parallel::clusterApply(
  cl = cl,
  x = l.bt,
  fun = f_bt
)

stopCluster(cl)

# running time
end_time <- Sys.time()
print(end_time - start_time)

