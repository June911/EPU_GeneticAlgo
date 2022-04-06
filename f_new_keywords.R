library("Matrix")


# Convert binary vector to keywords 
f_new_keywords <- function(x0, keywords_unlisted, lst_names){
  
  # get new list from selected keywords
  new_list <- keywords_unlisted[x0==1]
  # relist the new list
  keywords_new <- lapply(lst_names, f_relist, new_list=new_list)
  # rename new list 
  names(keywords_new) <- lst_names
  return (keywords_new)
}

# classfy in topic 
f_relist <- function (i, new_list) {
  unname(new_list[startsWith(names(new_list), as.character(i))])
}


# Convert binary vector to matrix of keywords
# where columns represent every topic 

f_new_keywords_mat <- function(keywords_binary, v_loc) {
  ## 1. convert binary keywords to matrix of binary keywords 
  # where columns represent topics 
  mat_keywords <- Matrix(0, length(keywords_binary), 3, sparse = T)
  
  # the dfm features are in alphabet orders 
  # we need to locate them by vector v_loc
  loc_e <- (v_loc == 1) & keywords_binary
  loc_p <- (v_loc == 2) & keywords_binary
  loc_u <- (v_loc == 3) & keywords_binary
  
  if (sum(loc_e) > 0) {
    mat_keywords[loc_e, 1] <- 1
  }
  if (sum(loc_p) > 0) {
    mat_keywords[loc_p, 2] <- 1
  }
  if (sum(loc_u) > 0) {
    mat_keywords[loc_u, 3] <- 1
  }
  
  return(mat_keywords)
}
