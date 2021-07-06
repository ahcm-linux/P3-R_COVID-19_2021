################################################################################
# Data analysis of COVID-19 published at: (article submitted for publication)
# date of creation: 07/03/2021 (date in US format)
# R version: 4.0.5
# script name: script.R
# aim: data analysis
# input: files from the folder 'data'
# output: files saved in the subdirectories of folder 'outputs'
# external sources: packages.R, outputs.R located in the folder 'codes'
################################################################################

# USER PARAMETERS --------------------------------------------------------------
# figures
width_factor <- 4 # factor for determining figure width

height_factor <- 2 # factor for determining figure height

# SEED -------------------------------------------------------------------------
set.seed(2021) # fix seed

# EXTERNAL SOURCES -------------------------------------------------------------
source("./codes/packages.R") # load packages

# DATA -------------------------------------------------------------------------
# load dataset
wide_data <- read.csv("./data/my_data2.csv", header = TRUE, check.names = FALSE)

original_colnames <- colnames(wide_data) # store the original column names

k <- which(original_colnames == "Group") # column number of treatment variable

long_data <- reshape2::melt(wide_data, id.vars = "Group") # dataset in long form

# DESCRIPTIVE ANALYSIS ---------------------------------------------------------
summ_data <- summary(long_data$value) # basic statistics for all values

tab_desc <- MVN:::descriptives(wide_data[, -k]) # descriptive statistics

# INFERENTIAL ANALYSIS ---------------------------------------------------------
# non-parametric manova
colnames(wide_data)[-k] <- paste0("C", 1:(k - 1))
form <- paste0(paste(colnames(wide_data)[-k], collapse = "|"), " ~ Group")
suppressWarnings(
  npar_manova <- nonpartest(formula(form),
    data = wide_data, plot = FALSE, permtest = FALSE
  )
)

# non-parametric two sample tests
npar_t_tests <- lapply(colnames(wide_data[, -k]), function(.x) {
  test_df <- wide_data[, c(.x, "Group")]
  colnames(test_df)[1] <- "var"
  npar.t.test(var ~ Group, data = test_df, method = "t.app", info = FALSE)
})
npar_t_tests_res <- do.call(rbind, lapply(npar_t_tests, "[[", 2))
rownames(npar_t_tests_res) <- original_colnames[-k]

tab_npar_t_tests <- subset(npar_t_tests_res, p.Value < 0.05) # subset by p-value
tab_npar_t_tests$vars <- rownames(tab_npar_t_tests)
tab_npar_t_tests <- arrange(tab_npar_t_tests, Estimator)
tab_npar_t_tests$vars <- factor(tab_npar_t_tests$vars,
  levels = tab_npar_t_tests$vars, ordered = TRUE
)

# plot variables with significantly different relative effect on the treatment
fig_pval_relef <- ggplot(tab_npar_t_tests, aes(x = vars, y = Estimator)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", alpha = 0.7) +
  geom_point() +
  geom_ribbon(
    alpha = 0.4, fill = "grey",
    aes(x = seq_len(nrow(tab_npar_t_tests)), ymin = Lower, ymax = Upper)
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  theme_light(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, size = 6),
    panel.grid = element_blank()
  ) +
  labs(y = "Relative effect", x = "")

# OUTPUTS ----------------------------------------------------------------------
# save tables and figures
suppressMessages(suppressWarnings(source("./codes/outputs.R")))