## Process GloVe pre-trained word vector in R
## from https://gist.github.com/tjvananne/8b0e7df7dcad414e8e6d5bf3947439a9
## modified due to updated text2vec library

# Load libraries ----------------------------------------------------------
require(text2vec)


# functions ---------------------------------------------------------------

f_get_lst_keywords_news <- function() {
  
  # seed keywords 
  # "white house" ==> "white" & "house"
  keywords <- list(
    # E = c("economy", "economic", "industry", "production"),
    E = c("economy", "economic"),
    P = c("congress", "legislation", "white", "house",
          "regulation", "deficit", "federal", "reserve"),
    U = c("uncertainty", "uncertain")
  )
  
  # number of keywords in every domain 
  # lst_n <- list(5, 10, 20)
  lst_n <- list(20)
  
  # initialize
  lst_keywords_new <- list()
  
  # start loop  
  for (n in lst_n) {
    print(n)
    # find nearest terms with word2vec
    keywords_new <- f_near_terms_lst(keywords, top_n_res=n)
    # save in list 
    lst_keywords_new[[as.character(n)]] <- keywords_new    
  }
  
  return (lst_keywords_new)
  
}



# find the word vector  
f_loc_wv <- function(term) {pretrained_matrix[[term]]}

# take the avg of word vectors
f_avg_wv <- function(terms) {rowMeans(sapply(terms, f_loc_wv))}

# find nearest terms 
f_near_terms <- function(terms, top_n_res=50) {
  names(find_sim_wvs(f_avg_wv(terms), pretrained_matrix, top_n_res=top_n_res))
}

# find nearest terms from list
f_near_terms_lst <- function(keywords, top_n_res=30) {
  lapply(keywords, f_near_terms, top_n_res=top_n_res)
}

# find nearest terms according to similarity score 
find_sim_wvs <- function(this_wv, all_wvs, top_n_res=50) {
  # this_wv will be a numeric vector; 
  # all_wvs will be a data.frame with words as columns and dimesions as rows
  this_wv_mat <- matrix(this_wv, ncol=length(this_wv), nrow=1)
  all_wvs_mat <- as.matrix(all_wvs)
  
  if(dim(this_wv_mat)[[2]] != dim(all_wvs_mat)[[2]]) {
    # print("switching dimensions on the all_wvs_matrix")
    all_wvs_mat <- t(all_wvs_mat)
  }
  
  cos_sim = sim2(x=all_wvs_mat, y=this_wv_mat, method="cosine", norm="l2")
  sorted_cos_sim <- sort(cos_sim[,1], decreasing = T) 
  return(head(sorted_cos_sim, top_n_res))
  
}

f_get_pretrained_matrix <- function(file_name) {
  # load pretrained matrix 
  pretrained_matrix <- scan(file = file_name, what="", sep="\n")
  # processing pretrained_matrix
  pretrained_matrix <- proc_pretrained_vec(pretrained_matrix) 
  return (pretrained_matrix)
}


# from GloVe to text2vec
proc_pretrained_vec <- function(p_vec) {
  # initialize space for values and the names of each word in vocab
  vals <- vector(mode = "list", length(p_vec))
  names <- character(length(p_vec))
  
  # loop through to gather values and names of each word
  for(i in 1:length(p_vec)) {
    if(i %% 1000 == 0) {print(i)}
    this_vec <- p_vec[i]
    this_vec_unlisted <- unlist(strsplit(this_vec, " "))
    this_vec_values <- as.numeric(this_vec_unlisted[-1])  # this needs testing, does it become numeric?
    this_vec_name <- this_vec_unlisted[1]
    
    vals[[i]] <- this_vec_values
    names[[i]] <- this_vec_name
  }
  
  # convert lists to data.frame and attach the names
  glove <- data.frame(vals)
  names(glove) <- names
  
  return(glove)
}




