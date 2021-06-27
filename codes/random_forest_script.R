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

n_varibles_in_impt_plots <- 3 # choose number of variables to show plots

width_factor <- 2 # increase width_factor to save plots with larger width

height_factor <- 2 # increase height_factor to save plots with larger height

# install/load packages --------------------------------------------------------
suppressMessages({
  load_lib <- c(
    "tidyverse",
    "tidymodels",
    "randomForestExplainer",
    "ggplot2"
  )
  install_lib <- load_lib[!(load_lib %in% installed.packages())] # check packages
  if (length(install_lib)) for (i in install_lib) install.packages(i) # install
  sapply(load_lib, require, character = TRUE) # load
})

# seed -------------------------------------------------------------------------
set.seed(2021) # seed for replicate the replicating the analysis

# load dataset -----------------------------------------------------------------
my_data <- read.csv("./data/my_data.csv", header = TRUE)

# split dataset into training and testing samples ------------------------------
data_split <- initial_split(my_data, prop = 0.75, strata = "Group")
sample_train <- training(data_split)
sample_test <- testing(data_split)

# create a 'recipe' object -----------------------------------------------------
sample_recipe <- recipe(Group ~ ., data = sample_train)
sample_prep <- sample_recipe %>% prep(training = sample_train, retain = TRUE)

# fit random forest model ------------------------------------------------------
rf_model <- rand_forest(trees = 2000, mtry = NULL, mode = "classification") %>%
  set_engine("randomForest", importance = TRUE, localImp = TRUE) %>%
  fit(Group ~ ., data = juice(sample_prep))

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

# save plots ---------------------------------------------------------------

# save plot p_choose_imp_1
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS1", ".pdf"),
  plot = p_choose_imp_1,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS1", ".png"),
  plot = p_choose_imp_1,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS1", ".jpg"),
  plot = p_choose_imp_1, width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_choose_imp_2
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigS2", ".pdf"),
  plot = p_choose_imp_2,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigS2", ".png"),
  plot = p_choose_imp_2,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigS2", ".jpg"),
  plot = p_choose_imp_2, width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_imp
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigA1", ".pdf"),
  plot = p_imp,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigA1", ".png"),
  plot = p_imp,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigA1", ".jpg"),
  plot = p_imp, width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_min_depth
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigA2", ".pdf"),
  plot = p_min_depth,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigA2", ".png"),
  plot = p_min_depth,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigA2", ".jpg"),
  plot = p_min_depth, width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)

# save plot p_interaction
ggsave(
  filename = paste0("./outputs/figures/pdf/", "FigA3", ".pdf"),
  plot = p_interaction,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/png/", "FigA3", ".png"),
  plot = p_interaction,
  width = width_factor * 105, height = height_factor * 74.25, units = "mm"
)
ggsave(
  filename = paste0("./outputs/figures/jpg_low-quality/", "FigA3", ".jpg"),
  plot = p_interaction, width = width_factor * 105, height = height_factor * 74.25,
  units = "mm", dpi = 100
)