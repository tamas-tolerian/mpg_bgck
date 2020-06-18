#====================================================#
#  Prepping to make figures for Florida Courts Data  #
#   - Load and clean STATA data                      #
#   - Plotting utilities                             #
#  Edited: 27 April 2018                             #
#====================================================#

# Nice reference: http://www.matthieugomez.com/statar/manipulate-data.html

####========================== Libraries ==============================####
# cowplot = helps with setting widths for combined figures. Ues with gridExtra
# ggplot2 = all the pretty plotting
# tidyverse = subsetting and such
# ggthemes = more control over how graphs look
# RColorBrewer = more colors
# gtable = multiple plots overlaid
# stargazer = pretty tables, beautiful LaTeX regression tables!
# ggExtra = margins plots
# gridExtra = margins plots
# ggridges = distributions in ridge plots
# grid
# usmap, maps = mapping
# stringi, stringr = string manipulation
# lubridate = dates. And puns.
# RCurl = importing robust function
# sandwich = robust std errors
# lmtest = linear regression
# readxl = read excel files
# xtable = exporting hand-constructed tables 
# gender = categorizing names by gender
# wru - categorizing last names by race
# reldist = weighted quantiles
# data.table = faster programs for certain things e.g. lags and leads
# questionr = weighted tables
# lfe = fixed effects and clustered se
# ggrepel = move apart labels on ggplots
# glmnet = lasso
# restore.point = for predict for felm objects
# modeest = calculating the mode
# geosphere= find the miles between two points
# tigris = help with FIPs codes
# cpt and changepoint = identifying RDs in the data
# usmap = county/state plots
# fastdummies = making dummy columns
# scales = add commas to graphs
# boot = for bootstrapped standard errors
# parallel = parallelize apply statements 
# encryptr = create hashed identifiers
devtools::install_github("pdil/usmap")
for (library_name in c("ggthemes", "RColorBrewer", "gtable", "stargazer",
                       "usmap", "ggplot2", "maps", "lubridate", "stringi", "stringr", "RCurl",
                       "sandwich", "lmtest", "tidyverse", "readxl", "xtable", "gender", "wru",
                       "reshape2", "reldist", "data.table", "questionr", "lfe",
                       "ggrepel", "glmnet", "restorepoint", "modeest", "geosphere",
                       "tigris", "cpt", "changepoint", "gridExtra", "cowplot", 
                       "fastDummies", "scales", "boot", "parallel",
                       "encryptr")) {
  library_name
  if (!require(library_name,character.only=TRUE)) {
    install.packages(library_name, character.only=TRUE)
  }
  library(library_name, character.only=TRUE)
}
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
options(lfe.eps=1e-6)
####============================= Settings & Themes ================================####
# Graphing Settings
options("scipen" =100, "digits" = 4) # override R's tendency to use scientific notation
# Style settings: color-blind-friendly and simple graphs
g15c_colors <- c("#56B4E9", #sky
                "#D55E00", #vermillion
                "#999999", #grey
                "#0072B2", #sea blue
                "#F0E442", #yellow
                "#009E73", #greeny blueish
                "#E69F00", #orange
                "#CC79A7", #pinkish red   
                "#000000", #black
                "#9932CC", #darkorchid
                "tomato1", 
                "seagreen",
                "red4",
                "royalblue",
                "palevioletred4"
                )
g15c_linetypes <- c("solid", "dashed", "longdash", "dotted", "dotdash", "twodash",  "F1", "4C88C488", "12345678")
g15c_shapes <- c(16, 17, 18, 15, 1, 2, 5, 0, 8, 3)
theme_g15c <- function () { 
  theme_hc(base_size=15, base_family="sans") %+replace% 
    theme(
      legend.position="bottom",
      legend.title=element_blank(),
      legend.text=element_text(size=13),
      axis.text=element_text(size=13),
      axis.title=element_text(size=15,face="bold"),
      axis.line = element_line(colour = "grey80"),
      panel.grid.major.x = element_line(colour = "#F7F7F7"),
      panel.grid.major.y = element_line(colour = "#F7F7F7"),
      strip.text = element_text(size = 15, face = "bold"),
      plot.title = element_text(hjust = 0.5, face="bold"),
      strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")
    )
}
  
g15c_style <- list(theme_g15c(), 
                   scale_color_manual(values = g15c_colors), 
                   scale_fill_manual(values = g15c_colors), 
                   scale_shape_manual(values = g15c_shapes), 
                   scale_linetype_manual(values = g15c_linetypes),
                   scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                  scientific = FALSE))) #scale_y_continuous(labels = comma)


g15c_style_cont <- list(theme_g15c(), 
                        scale_color_gradient2(low = "peachpuff", mid = "darkred", high = "black"), 
                        scale_fill_gradient2(low = "peachpuff", mid = "darkred", high = "black"))

theme_g15c_blank <- function () { 
  theme_hc(base_size=14, base_family="sans") %+replace% 
    theme(
      legend.position = "right",
      legend.title=element_blank(),
      axis.line = element_line(colour = "grey80"),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(), 
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(), 
      panel.grid.minor.y = element_blank()
    )
}
g15c_style_no_grid <- list(theme_g15c_blank(), scale_color_manual(values = g15c_colors),
                          scale_fill_manual(values = g15c_colors), 
                          scale_shape_manual(values = g15c_shapes), 
                          scale_linetype_manual(values = g15c_linetypes),
                          scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                         scientific = FALSE))) #scale_y_continuous(labels = comma)

theme_g15c_with_legend <- function () { 
  theme_hc(base_size=14, base_family="sans") %+replace% 
    theme(
      legend.position = "right",
      axis.line = element_line(colour = "grey80"),
      panel.grid.major.x = element_line(colour = "#F7F7F7"),
      panel.grid.major.y = element_line(colour = "#F7F7F7")
    )
}
g15c_style_with_legend <- list(theme_g15c_with_legend(), 
                               scale_color_manual(values = g15c_colors), 
                               scale_fill_manual(values = g15c_colors), 
                               scale_shape_manual(values = g15c_shapes), 
                               scale_linetype_manual(values = g15c_linetypes),
                               scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                                              scientific = FALSE))) #scale_y_continuous(labels = comma)

####============================= Factor Grid lines: annotate ================================####

ann_y <- annotate("segment", x=-Inf, xend= -Inf, y=-Inf, yend= Inf, colour = "grey80") 
ann_x <- annotate("segment", x=-Inf, xend=  Inf, y=-Inf, yend= -Inf, colour = "grey80") 
ann_x_date <- annotate("segment", x=as.Date(-Inf), xend= as.Date( Inf), y=-Inf, yend= -Inf, colour = "grey80") 
ann_y_date <- annotate("segment", x=as.Date(-Inf), xend= as.Date(-Inf), y=-Inf, yend= Inf, colour = "grey80") 

####============================= Setting NAs ================================####

# set so always see NAs if they exist
table = function (..., useNA = 'ifany') base::table(..., useNA = useNA)


####========== Left Join with Overwrite instead of New Column Creation ================================####
#' Left-join and overwrite values in "left" dataframe
#'
#' If you want to recalculate Z-scores, for example, without worrying about columns in the
#' "right" dataframe having the same names as columns in the left data frame
#' (which would cause their names to become something like "colname.x" and "colname.y")
#' this function replaces all the columns in the first data frame with the ones in the second.
#' The arguments are just like \code{\link[dplyr]{left_join}}, and builds off that code.
#'
#' @param x,y the "left" and "right" data frames, respectively
#' @param by the columns you want to join by would appear by default.
#' @param copy whatever this argument means in the regular \code{\link[dplyr]{left_join}}
#' @param \dots Optional arguments
#' @return a data frame or tibble or whatever, just like \code{\link[dplyr]{left_join}} but with columns overwritten
#'
#' @export
left_join_and_overwrite <- function(x, y, by = NULL, copy = FALSE, warnifnoprevious = FALSE, ...) {
  by <- dplyr::common_by(by,x,y)
  incoming_cols <- dplyr::setdiff(names(y),by$y)
  tryCatch({
    dplyr::left_join(dplyr::select(x,-one_of(incoming_cols)), y, by, copy, ...)
  }, warning = function(war) {
    if (!(warnifnoprevious) &
        paste0(war)==paste0("simpleWarning in one_of(incoming_cols): Unknown variables: `",
                            paste0(incoming_cols, collapse = "`,`"),
                            "`\n")) {
      suppressWarnings(dplyr::left_join(dplyr::select(x,-one_of(incoming_cols)), y, by, copy, ...))
    } else {
      dplyr::left_join(dplyr::select(x, -one_of(incoming_cols)), y, by, copy, ...)
    }
    
  })
  # dplyr::left_join(dplyr::select(x,-one_of(incoming_cols)), y, by, copy, ...)
}

#### Predict for FELM objects ####
examples.predict.felm = function() {
  library(regtools)
  
  T = 12
  x <- 1:T
  fe1 = sample(c("yes","no"), T, replace=TRUE)
  fe2 = sample(c("a","b","c"), T, replace=TRUE)
  y <- x  + (fe1=="yes") + 2*(fe2=="a") + (fe2=="b")+ rnorm(T)
  
  new <- data.frame(
    x = (T+1):(2*T),
    fe1 = sample(c("yes","no"), T, replace=TRUE),
    fe2 = sample(c("a","b","c"), T, replace=TRUE)    
  )
  p.lm = predict(lm(y~x+log(x)+fe1+fe2), new)
  
  library(lfe)
  felm = felm(y~x+log(x)+fe1|fe2)
  p.felm = predict.felm(felm,new)
  
  # Compare lm and felm predictions
  # the predicted values should be the same
  cbind(p.lm, p.felm)
  
  library(regtools)
  
  T = 12
  x <- 1:T
  fe1 = sample(c("yes","no"), T, replace=TRUE)
  fe2 = sample(c("a","b","c"), T, replace=TRUE)
  y <- x  + (fe1=="yes") + 2*(fe2=="a") + (fe2=="b")+ rnorm(T)
  
  new <- data.frame(
    x = (T+1):(2*T),
    fe1 = sample(c("yes","no"), T, replace=TRUE),
    fe2 = sample(c("a","b","c"), T, replace=TRUE)    
  )
  p.lm = predict(lm(y~x+log(x)+fe1+fe2), new)
  
  library(lfe)
  felm = felm(y~x+log(x)+fe1+fe2)
  p.felm = predict.felm(felm,new)
  
  # Compare lm and felm predictions
  # the predicted values should be the same
  cbind(p.lm, p.felm)
}

#' An implementaion of predict for felm
predict.felm = function(object, newdata, use.fe = TRUE,...) {
  restore.point("predict.felm")
  co = coef(object)
  
  # too make code compatible with tibbles
  newdata = as.data.frame(newdata)
  
  rownames(newdata) = seq_along(newdata[,1])
  
  form = formula(object)
  # Need to extract part before first |
  # use brute force string manipulation
  #library(Formula) # will be loaded by lfe anyways
  #form.str = as.character(Formula(form))
  
  form.str = capture.output(form)
  pos = regexpr("|", form.str, fixed=TRUE)
  if (pos > 0) {
    form.str = substr(form.str,1,pos-1)
    form = as.formula(form.str)
  }
  
  
  # model matrix
  print(form)
  mf = model.frame(form, newdata)
  print(head(newdata))
  print(head(mf))
  mm = model.matrix(form,data=mf)
  print(head(mm))
  
  if (NROW(mm)<NROW(newdata)) {
    warning("Observations dropped from newdata due to NA.")
  }
  
  # remove intercept if not included in coefficients
  if (NCOL(mm)==length(co)+1) {
    mm = mm[,-1,drop=FALSE]
  }
  print(dim(mm))
  print(length(co))
  y.pred = mm %*% co
  
  fe.vars = names(object$fe)
  if (use.fe & length(fe.vars)>0) {
    rows = as.integer(rownames(mm))
    nd = newdata[rows,]
    all.fe = getfe(object)
    fe.var = fe.vars[1]
    for (fe.var in fe.vars) {
      df = all.fe[all.fe$fe == fe.var,]
      frows = match(nd[[fe.var]],df$idx)    
      myfe = df$effect[frows]
      myfe[is.na(myfe)] = 0
      
      y.pred = y.pred + myfe
    }
  }
  as.vector(y.pred)
}
