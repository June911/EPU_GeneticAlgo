# some general functions for the project ----------------------------------
# library("xts")
library("gplots")
library("ggplot2")
library("cowplot")



# rolling average function 
# side=1 ==> for only past values
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 1)}

# add them together and drop duplicates
f_add_keywords <- function(keywords_og, keywords_picked) {
  unique(c(keywords_og, keywords_picked))
}


# process objective y 
f_process_y_object <- function(file_name, is_rets=FALSE) {
  # read data 
  y_object <- read.csv(here::here("data", file_name))
  names(y_object) <- c("date", "value")
  y_object$date <- as.Date(y_object$date)
  
  if (is_rets) {
    # change the value to returns 
    n_dates <- dim(y_object)[1]
    y_object$value[2:n_dates] <- y_object$value[2:n_dates] / y_object$value[1:(n_dates - 1)] - 1
    y_object <- y_object[2:n_dates,]
  }
  return(y_object)
}

# convert price series to return series or difference series 
f_price2rets <- function(y_object, ytype) {
  
  # change name 
  names(y_object) <- c("date", "value")
  
  # number of periods
  n_periods <- dim(y_object)[1]
  # number of y variable 
  n_y <- dim(y_object)[2]
  
  if (ytype == "rets") {
    # convert level to returns 
    y_object[2:n_periods, 2:n_y] <- y_object[2:n_periods, 2:n_y] / y_object[1:(n_periods-1), 2:n_y] - 1
    # exclude first line
    y_object <- y_object[2:n_periods, ]
  } else if (ytype == "dif") {
    # convert level to difference 
    y_object[2:n_periods, 2:n_y] <- y_object[2:n_periods, 2:n_y] - y_object[1:(n_periods-1), 2:n_y] 
    # exclude first line
    y_object <- y_object[2:n_periods, ]
  } else {
    print("level remains")
  }
  return(y_object)
}


# matching dates between data
f_match_dates <- function(datasource, y_object) {
  # datasource ==> the dfm object with docvar of dates
  # y_object ==> macro varibale to forcaste with colname = c("date", "value")
  months <- floor_date(datasource$dates, "month")
  y_object <- subset(y_object, date %in% unique(months))

  # # match the input dates with object dates
  is_in_month <- months %in% y_object$date
  datasource <- dfm_subset(datasource, is_in_month)
  dates <- dates[is_in_month]

  # set dates as one of the document vectors
  datasource$dates <- dates
  
  return(list(datasource, y_object))
}


# logarithmic spaced sequence
# from library("emdbook")
lseq <- function(from=1, to=100000, length.out=6) {
  exp(seq(log(from), log(to), length.out = length.out))
}

# plot matrix -- heatmap 
f_heatmap <- function(mat, file_name, main_name, folder_name="results", is_png=F,
                      srtCol=0, adjCol = c(0.5,1)) {
  #  
  if (is_png) {
    png(file=here::here(folder_name, file_name),
        width = 8, height = 8, unit = "in", res = 300)
  }

  # set graph position 
  lmat <- rbind( c(5,3,4), c(2,1,4))
  lhei <- c(0.5, 4)
  lwid <- c(0.5, 4.5, 0.5)
  
  heatmap.2(mat, 
            main=main_name, 
            # srtCol=0, adjCol = c(0.5,1), # make xlabel horizontal 
            srtCol=srtCol,  adjCol = adjCol,
            lmat=lmat, lhei=lhei, lwid=lwid, 
            cexRow=1.1, cexCol=1, margins=c(5,8),
            dendrogram=c("none"), Rowv=FALSE, Colv=FALSE, 
            col = grey.colors(2), 
            trace="row",
            key = FALSE,
            density.info = c("none"),
  )
  
  if (is_png) {
    dev.off()
  }
}


# plot line by xts 
f_lineplot_xts <- function(lst_sm, y_object, name_y_object, file_name, folder_name,
                           is_dif=FALSE){
  # make sure the right column name 
  names(y_object) <- c("date", name_y_object)
  
  # combine EPU index 
  sms <- suppressWarnings(Reduce(function(x, y) merge(x, y, by="date"), lst_sm))
  
  # correlation function 
  f_col <- function(sm, y) {round(cor(sm, y), 2)}
  
  # correlation score  
  score_col <- apply(sms[, -1], 2, f_col, y=y_object[, name_y_object])
  
  # set names
  names(sms) <- c("date", paste(names(lst_sm), score_col, sep="_"))
  
  # # convert into an xts object
  xts_sms <- xts(x=sms[, -1], order.by = sms$date)
  
  # plot y_object 
  y_object <- xts(x=y_object[, name_y_object], order.by= y_object[, "date"])
  
  # file name 
  png(file=here::here(folder_name, 
                      paste0(file_name, ".png")),
      width = 9, height = 8, unit = "in", res=300)
  
  # plot with moving average 
  par(mfrow = c(2, 1), mar = c(5,5,2,5))
  # need to print to save the plot 
  print(plot(xts_sms, type="l", ylab="value",
             main=file_name, legend.loc="topleft"))
  print(plot(y_object))
  
  # close 
  dev.off()
  
  # plot cumsum 
  if (is_dif) {
  
    # file name 
    png(file=here::here(folder_name, 
                        paste0(file_name, "_cumsum", ".png")),
        width = 9, height = 8, unit = "in", res=300)
    
    # plot -- cumsum 
    par(mfrow = c(2, 1), mar = c(5,5,2,5))
    # need to print to save the plot 
    print(plot(cumsum(xts_sms), type="l", ylab="value",
               main=paste0(file_name, "_cumsum"), legend.loc="topleft"))
    print(plot(cumsum(y_object)))
    
    # close 
    dev.off()
    
  }
  
}

# plot for epu_all and y 
f_ggplot_epu_y <- function(epu_all, y, common_prefix) {
  # plot epu 
  p_epu <- ggplot(epu_all, aes(date, EPU_index)) +
    geom_line(aes(color = group, size = group, linetype = group)) +
    
    # geom_line(data = epu_original, aes(date, EPU_index), 
    #           size = 0.6, linetype="twodash",  color="grey") +
    
    scale_colour_manual(name="", values = c(epu_train="black", 
                                            epu_cv="steelblue", 
                                            epu_test="darkred",
                                            epu_original="darkgrey")) +
    scale_size_manual(name="", values = c(epu_train=1,
                                          epu_cv=1,
                                          epu_test=1,
                                          epu_original=0.6)) +
    scale_linetype_manual(name="", values = c(epu_train="solid",
                                              epu_cv="solid",
                                              epu_test="solid",
                                              epu_original="dashed")) +
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) + 
    theme(legend.position = "top", 
          legend.key.size = unit(3, 'cm')) + 
    labs(
      x = "",
      y = "",
      colour = "",
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    # add period in grey 
    annotate("rect", fill = "grey", alpha = 0.5,
             xmin = as.Date(c("2007-01-01")), 
             xmax = as.Date(c("2009-01-01")), 
             ymin = -Inf, ymax = Inf) + 
    annotate("text", x = as.Date(c("2006-01-01")), y = max(epu_all[, 2]) *0.95, 
             label = "Great Financial Crisis", size = 3)
  
  
  # # convert y to tibble 
  # y <- tibble(y_objects[,c(1,i)])
  
  # plot y 
  p_y <- ggplot(y, aes(date, .data[[names(y)[2]]])) + 
    geom_line(size = 1) + 
    # theme_cowplot(12) + 
    theme_bw() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    theme(legend.position = c(0.8, 0.8)) + 
    labs(
      x = "",
      y = "",
      colour = "",
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
    # add period in grey 
    annotate("rect", fill = "grey", alpha = 0.5,
             xmin = as.Date(c("2007-01-01")), 
             xmax = as.Date(c("2009-01-01")), 
             ymin = -Inf, ymax = Inf) +
    annotate("text", x = as.Date(c("2006-01-01")), y = max(y[, 2]) *0.95, 
             label = "Great Financial Crisis", size = 3)
  
  # combine with x-axis aligned 
  # p_combined <- plot_grid(p_epu, p_y, labels = c("EPU Index", names(y)[2]), label_size = 10,
  #                         ncol = 1, align = "v", rel_heights=c(1.5,1))
  
  p_combined <- plot_grid(p_epu, p_y, ncol = 1, align = "v", rel_heights=c(1.5,1))
  
  # save figule 
  ggsave(here::here("results", paste0(common_prefix, "_epu.png")), p_epu, 
         width = 12, height = 8, unit = "in",
         dpi = 300)
  
  ggsave(here::here("results", paste0(common_prefix, "_y.png")), p_y, 
         width = 12, height = 8, unit = "in",
         dpi = 300)
  
  ggsave(here::here("results", paste0(common_prefix, "_combined.png")), p_combined, 
         width = 12, height = 8, unit = "in",
         dpi = 300)
  
}
