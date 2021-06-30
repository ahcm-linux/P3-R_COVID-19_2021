################################################################################
# Data analysis of COVID-19 published at: (article submitted for publication)
# date of creation: 06/27/2021 (date in US format)
# R version: 4.0.5
# script name: script.R
# aim: data analysis
# input: files from the folder 'data'
# output: files saved in the subdirectories of folder 'outputs'
# external sources: none
################################################################################

# script's parameters ----------------------------------------------------------
# you can change the parameters below according to your needs

# data related parameters
glog_data <- FALSE # set to TRUE if you want to transform the data using glog

std_data <- FALSE # set to TRUE if you wnat to standardize the data

# random forest related parameters
n_varibles_in_impt_plots <- 3 # choose number of variables to show plots

n_trees <- 10000 # initial number of trees

# graphics related parameters
width_factor <- 2 # increase width_factor to save plots with larger width

height_factor <- 2 # increase height_factor to save plots with larger height

color_ramp <- RColorBrewer::brewer.pal(n = 7, name = "RdYlBu") # heatmap color

color_border <- grey(0.4) # heatmap border color

fontsize <- 8 # heatmap font size

# load sources -----------------------------------------------------------------
source("./codes/packages.R") # load packages
source("./codes/functions.R") # load functions

# seed -------------------------------------------------------------------------
set.seed(2021) # seed for replicate the replicating the analysis

# load dataset -----------------------------------------------------------------
my_data <- read.csv("./data/my_data.csv", header = TRUE) # original data
my_data$Group <- factor(my_data$Group) # groups as factors

n <- ncol(my_data) # number of columns

if (glog_data) my_data[, -n] <- LogSt(my_data[, -n]) # apply glog
if (std_data) my_data[, -n] <- scale(my_data[, -n]) # standardize data

# descriptive measures ---------------------------------------------------------
# basic data struture
desc_data <- c(
  n_obs = nrow(my_data), n_vars = ncol(my_data[, -n]),
  n_in_group = table(my_data$Group)
)

desc_stat <- mvn(my_data[, -n])$Descriptives # standard descriptive statistics

desc_cor <- cor(my_data[, -n], method = "spearman") # spearman correlation

# plot desc_cor using clustering on rows and columns
p_cor <- pheatmap(desc_cor,
  clustering_method = "ward.D",
  cluster_rows = TRUE, cluster_cols = TRUE,
  color = color_ramp, border_color = color_border, fontsize = fontsize
)

# split dataset into training and testing samples ------------------------------
data_split <- initial_split(my_data, prop = 0.75, strata = "Group")
sample_train <- training(data_split)
sample_test <- testing(data_split)

# create a 'recipe' object -----------------------------------------------------
sample_recipe <- recipe(Group ~ ., data = sample_train)
sample_prep <- sample_recipe %>% prep(training = sample_train, retain = TRUE)

# tune random forest hyperparameters (mtry) ------------------------------------
mtry <- tuneRF(sample_train[, -n], as.factor(sample_train$Group),
  ntreeTry = n_trees, stepFactor = 1.5, improve = 0.01,
  trace = FALSE, plot = FALSE
)

m <- mtry[mtry[, 2] == min(mtry[, 2]), 1][1] # best value of mtry

# fit random forest model ------------------------------------------------------
rf <- rand_forest(trees = n_trees, mtry = m, mode = "classification") %>%
  set_engine("randomForest", importance = TRUE, localImp = TRUE) %>%
  fit(Group ~ ., data = juice(sample_prep))

print(rf$fit) # show basic results for the random forest model

# warning: the next plot will be displayed but not saved (save it manually)
plot(rf$fit) # error rate

# evaluate the model -----------------------------------------------------------
# roc curves
pred_for_roc_curve <- predict(rf, sample_test[, -n], type = "prob")

groups <- levels(sample_test$Group)
group_color <- c("#F8766D", "#00BA38", "#619CFF")

auc <- numeric(length(groups)) # vector for holding values of area under ROC
names(auc) <- grougps

# warning: the next plot will be displayed but not saved (save it manually)
for (i in seq_len(length(groups))) {
  # Define which observations belong to class[i]
  true_values <- sample_test$Group == groups[i]
  # Assess the performance of classifier for class[i]
  pred <- prediction(pred_for_roc_curve[, i], true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i == 1) {
    plot(perf, main = "ROC Curve", col = group_color[i])
  }
  else {
    plot(perf, col = group_color[i], add = TRUE)
  }
  # Calculate the area under the curve (AUC) and print it to screen
  auc[i] <- performance(pred, measure = "auc")
}

# confusion matrix
pred_for_table <- predict(rf, sample_test[, -n])

confusion_mat <- table(
  observed = sample_test[, n],
  predicted = unlist(pred_for_table)
)

# plot main results ------------------------------------------------------------
# tree with least number of nodes
tree_num <- which(rf$fit$forest$ndbigtree == min(rf$fit$forest$ndbigtree))
p_rf_tree <- tree_func(final_model = rf$fit, tree_num)

# measures of variable importance for the fitted random forest model -----------
impt_measures <- measure_importance(rf_model$fit)

# chosing best set of importance measures to use
p_choose_imp_1 <- plot_importance_ggpairs(impt_measures)
p_choose_imp_2 <- plot_importance_rankings(impt_measures)

# define your chosen measures replacing NULL by the measure' name
first_measure <- NULL
second_measure <- NULL
third_measure <- NULL

# test if user has chosen the three importance measures
if (is.null(first_measure) | is.null(first_measure) | is.null(first_measure)) {
  stop("Error! You did not choose the three inportance meansures...
        please start the hole script again")
}

# plot the chosen measures
p_imp <- plot_multi_way_importance(impt_measures,
  x_measure = first_measure,
  y_measure = second_measure,
  size_measure = third_measure,
  no_of_labels = n_varibles_in_impt_plots
)

# plot variable depth distribution
min_depth <- min_depth_distribution(rf_model$fit)
p_min_depth <- plot_min_depth_distribution(min_depth, mean_sample = "top_trees")

# plot interaction between pairs of variables in the random forest
impt_vars <- important_variables(impt_measures,
  k = n_varibles_in_impt_plots,
  measures = c(first_measure, second_measure, third_measure)
)

# level of interacton between variables (min depth interacitons)
interaction_vars <- min_depth_interactions(rf_model$fit, impt_vars)
p_interaction <- plot_min_depth_interactions(interaction_vars)

# generate outputs ---------------------------------------------------------
source("./codes/outputs.R")