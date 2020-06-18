#### Sample R Code ####
# Purpose: provide some context for a few basic tools

####----------------------------- Settings ---------------------------------------####
# clear everything
rm(list=ls())
closeAllConnections()

# Working directory Local
setwd("~/Dropbox/DV/")
data.input.path <- "Data/Raw"
data.output.path <- "Data/Cleaned" 
output.path <- "Output"

# graphing utilities
source("Code/Prep_for_Images.R")


# switches
debug <- T

####======================================= Data Load =======================================####
data <- read_csv(file.path(data.input.path, "raw_data.csv"))

####======================================= Explore The Data =======================================####

# Take a look at some data
head(iris)
# look at how many rows and variables it has
dim(iris) 

# find values of a certain variable
table(iris$Species)

if(debug){
  # put "View" statements in a debug statement so that if you set debug=F above, it doesn't annoying open.
  iris %>% 
    filter(Sepal.Width > 3) %>% 
      View()
}

####======================================= Data Wrangling =======================================####
# KEY piece: any time you group_by, you Must ungroup afterward

# Two helpful cheat-sheets:
# https://4.files.edl.io/b9e2/07/12/19/142839-a23788fb-1d3a-4665-9dc4-33bfd442c296.pdf
# https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf


####======================================= Merging Datasets =======================================####
# I'm making a faux dataset to show you merging 
iris_means <- iris %>%
  group_by(Species) %>% 
  summarise(mean_sepal_width = mean(Sepal.Width, na.rm=T),
            mean_sepal_length = mean(Sepal.Length, na.rm=T)) %>% 
  ungroup()

# check dimensions before and after
# put variables in to see which ones merged and which didn't.
dim(iris)
iris_with_means <- iris %>%
  mutate(in_iris = T) %>% 
  left_join(iris_means %>% mutate(in_means = T),
            by = "Species")
dim(iris_with_means)

# check how many merged -- here everything merged nicely! So we can drop the merge vars
table(iris_with_means$in_iris, iris_with_means$in_means)
iris_with_means <- iris_with_means %>% 
  select(-in_iris, -in_means)

# check if we had same var names that are causing clash -- here were fine
names(iris_with_means %>% select(ends_with(".x"), ends_with(".y")))


# clean up after yourself
rm(iris_with_mean, iris_means)
####======================================= Figures =======================================####
# Nice reference: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# a histogram
iris %>%
  ggplot(aes(x = Sepal.Width)) + 
  geom_histogram() + g15c_style

# facet-grid 
iris %>%
  ggplot(aes(x = Sepal.Width)) + 
  geom_histogram() + 
  facet_grid(Species ~ .)+
  g15c_style

# scatter plot
iris %>% 
  ggplot(aes(x = Sepal.Width, y = Petal.Width, color = Species)) +
  geom_point(alpha = 0.5) +
  g15c_style + labs(x = "Sepal Width", y = "Petal Width") 
# save a figure -- this will save the figure you just ran
ggsave(file.path(output.path, "scatter_sepal_vs_petal.png"), width = 8, height = 4.25, dpi = 300)


####======================================= Regressions =======================================####
# Generally use felm() from the lfe package for regressions
# How to write the formula: https://rdrr.io/cran/lfe/man/felm.html. 
# Approximately: reg <- felm(y ~ x + controls | fixed effects | (z ~ instrument)) | cluster variable, data = dataset)
model1 <- felm(Sepal.Length ~ Sepal.Width | Species | 0 | 0,
               data = iris)
model2 <- felm(Sepal.Length ~ Sepal.Width + Petal.Length | Species | 0 | 0,
               data = iris)


# Table to do basic results comparison
stargazer(model1, model2, type = "text")

# Outputting a pretty latex table
stargazer(model_1, model_2, 
          type = "latex",
          keep = c("Sepal.Width"),
          omit.stat = c("adj.rsq", "ser", "F"),
          float=F,
          covariate.labels = "Sepal Width",
          dep.var.labels = c("Sepal Length", "Sepal Length"),
          add.lines = list(c("Mean", round(mean(model1$response),3), 
                             round(mean(model2$response),3))),
          out = file.path(output.path, "reg_table.tex"))


# If you need to do predictions with many fixed effects, consider using the fixest package:
# https://stackoverflow.com/questions/30491545/predict-method-for-felm-from-lfe-package/58773655#58773655

# Make a habit of cleaning up after yourself: delete the files you no longer need
rm(model1, model2)

####======================================= Tables =======================================####
# if you need to make homemade tables, and output them, xtable is the way to go:
print(xtable(abbrev_desc), 
      booktabs=T, 
      include.rownames = FALSE, 
      format.args = list(big.mark = ",", decimal.mark = "."), 
      file = file.path(output.path, "custom_table.tex"))



####======================================= Regression Loop =======================================####
# if you need to run through a few different specifications and/or 
# want to graph the results afterward:

outcomes <- c("Sepal.Width", "Petal.Width")
controls <- c("+0", "+ Petal.Length")
reg_nums <- seq(1:length(controls))
names(controls) <- reg_nums
counter <- 1

for(outcome in outcomes){
  for(control in controls){
  #print which regression you're on
    print(paste0(counter, ": ", outcome, " and ", control, " at time ", Sys.time()))
    incent <- felm(as.formula(sprintf("%s ~ Sepal.Length %s | Species | 0 | Species", outcome, control)),
                   data = iris)
    summary(incent)
    
    # get the average of the outcome variable
    mu <- mean(incent$response)
    
    cur_fit <- tibble(input = rownames(coef(summary(incent))),
                      coef = coef(summary(incent))[,1],
                      se = coef(summary(incent))[,2]) %>%
      mutate(dep_var = outcome,
             adjusted_r2 = summary(incent)$adj.r.squared,
             controls = control,
             r2 = summary(incent)$r.squared,
             degrees_of_freedom = summary(incent)$rdf,
             counter = counter,
             mu_baseline = mu,
             N = incent$N)
    print(cur_fit)
    if(counter == 1) {
      reg_outcomes <- cur_fit
    } else {
      reg_outcomes <- bind_rows(reg_outcomes, cur_fit)
    }
    counter = counter +1
  }
}


# clean the regs
reg_outcomes <- reg_outcomes %>%
  mutate(beta = 100*coef,
         se_pct = 100*se,
         lcl = beta - 1.96*se_pct,
         ucl = beta + 1.96*se_pct) 

# save the regs
saveRDS(reg_outcomes, file.path(output.path, "reg_outcomes.Rds"))

# make a picture of the coefficients and standard errors from various different models
reg_outcomes %>%
  filter(dep_var == "Sepal.Width") %>% 
  ggplot(aes(x = input, y = beta, ymin = lcl, ymax = ucl,
      color = factor(controls, levels = c("+0", "+ Petal.Length"),
                     labels = c("No Controls", "Petal Length Controls")))) + 
  geom_point(position = position_dodge(width = 0.3)) + 
  geom_errorbar(position = position_dodge(width = 0.3), width = 0.3)+
  g15c_style + labs(x = "Explanatory Variable", y = "Coefficient")
#save the picture
ggsave(file.path(output.path, "coef_plot.png"), width = 8, height = 4.25, dpi = 300)

