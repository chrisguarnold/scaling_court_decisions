# ------------------------------------------------------------------------------
# Master File
# 
# Replication Files for Arnold, Engst, Gschwend: 
# "Scaling Court Decisions with Citation Networks"
# 
# Chris Arnold, Cardiff University; Benjamin Engst, University of Mannheim
# August 2021
# ------------------------------------------------------------------------------


# -- Housekeeping --------------------------------------------------------------

# Libraries
library(XML) 
library(stringr) 
library(plyr) 
library(shinystan)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()) 
library(reticulate)
use_virtualenv("AEG_slc") 
library(stargazer)

# Seed
project.seed <- 1701

# Global settings for running the script 
run.sampling <- TRUE
save.sampling <- TRUE
load.sampling <- TRUE
start.shinystan <- FALSE



# Helper Functions
# Plots for adjacency matrices
source("code_helpers/r_helpers_data_checks.r")
# Running the Stan models
source("code_helpers/r_helpers_scaling_cases_stan.r")
# Visualise the results of the runs
source("code_helpers/r_helpers_visualise_estimates.r")
# Implement and visualise the posterior predictive checks
source("code_helpers/r_helpers_posterior_predictive_checks.r")


# Predefined Cardiff colours
# primary
cardiffred <- rgb(211,55,74, maxColorValue = 255)
cardiffblack <- rgb(35,21,32, maxColorValue = 255)
cardiffgrey <- rgb(47,68,78, maxColorValue = 255)
cardiffgold <- rgb(189,158,94, maxColorValue = 255)
# secondary
cardiffblue <- rgb(21,44,81, maxColorValue = 255)
cardiffpurple1 <- rgb(29,15,51, maxColorValue = 255)
cardiffpurple2 <- rgb(60,44,89, maxColorValue = 255)

# Dependencies 
# For dependencies, see the bottom of the script



# == 1 Query the JURIS DB ======================================================

# -- 1.1 Press Law -------------------------------------------------------------
# Don't run. Since we cannot provide access to the ElasticSearch with data from
# JURIS, we have to leave this code as a dry run. For transparency, we do report
# it here, though, so that you can understand what we did.
run = FALSE
if (run) source_python("query_db_presslaw.py")

# -- 1.2 Antitrust Law ---------------------------------------------------------
# Here, we chose to query the Juris DB via the frontend and download the data
# via the browser.
run = FALSE
# With this help file you can extract the citations 
# from the original decisions existing in .html 
if (run) source("code_helpers/antitrust_citation.r")
# The citations extracted in this way were corrected 
# manually by a trained coder. The manually corrected 
# files are stored in "data/1_original_seed_decisions/antitrust_manuel" 
# and were further edited assessing the documents creating final citation
# documents per decision in "data/1_original_seed_decisions/antitrust_final"
if (run) source("code_helpers/antitrust_corrections.r")
# from the documents in "data/1_original_seed_decisions/antitrust_final"
# we create the final citation matrix for the antitrust decisions
if (run) source("code_helpers/antitrust_matrix.r")



# == 2 Generate Data For Estimation ============================================
# -- 2.1 Press Law -------------------------------------------------------------
source_python("from_pickle_to_csvs_presslaw.py")

# -- 2.2 Antitrust Law ---------------------------------------------------------
source("code_helpers/r_helpers_data_reformatting_cartels.r")



# == 3 Estimate Idealpoints ====================================================
# ------- 3.1 Press Law Images MLT-------------
data_location <-"../data/3_for_estimation/presslaw_images/mlt/"

if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count= TRUE, cit_filter = TRUE, 
                                 seed.input = project.seed,
                                 nr.iterations = 8000, 
                                 init.model = 'stan', anchors.input = TRUE,
                                 model = 'poisson')

if(start.shinystan) launch_shinystan(fit.stan)

if(save.sampling) save(fit.stan, 
                       file = '../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData')

draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)
 
# Plotting decisions
setEPS()
postscript("../figures_and_tables/main/figure_2a.eps",
    height = 8)
plot.nice.theta.w.courts.wrapper(draws.stan, more.vars.dat, inverter = -1,
                                 gerort.prior.knowledge.courts = c("Köln",
                                                                   "Hamburg"))
dev.off()


# -- First Differences for case study  
source("first_differences_case_study.r")


# --- 3.2 Press Law Online Linking MLT ---------------
data_location <-"../data/3_for_estimation/presslaw_online_linking/mlt/"

if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count = TRUE, cit_filter = TRUE, 
                                 seed.input = project.seed, init.model = 'stan',
                                 nr.iterations = 8000, anchors.input = TRUE,
                                 model = 'poisson')

if(start.shinystan) launch_shinystan(fit.stan)

if(save.sampling) save(fit.stan, file = '../data/4_sampling_outputs/presslaw_online_linking_mlt_poisson.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/presslaw_online_linking_mlt_poisson.RData')
more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/main/figure_2b.eps", 
    height = 8)
plot.nice.theta.w.courts.wrapper(draws.stan, more.vars.dat, inverter = 1,
                                 gerort.prior.knowledge.courts = c("Köln", 
                                                                   "Hamburg"))
dev.off()


# -- 3.3 Antitrust -------------------------------------------------------------
data_location <-"../data/3_for_estimation/antitrust/"

if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count = TRUE, cit_filter = TRUE, 
                                 seed.input = project.seed, init.model = 'vb',
                                 nr.iterations = 8000, anchors.input = TRUE,
                                 model = 'poisson')
if(start.shinystan) launch_shinystan(fit.stan)
# print(fit.stan, pars = c('theta1', 'theta'))

if(save.sampling) save(fit.stan, file = '../data/4_sampling_outputs/cartels_count_poisson.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/cartels_count_poisson.RData')
draws.stan <- extract(fit.stan)

more.vars.dat <- read.in.data(data_location)

setEPS()
postscript("../figures_and_tables/main/figure_6.eps", height = 10)
plot.nice.theta.w.courts.wrapper(
    draws.stan, more.vars.dat, inverter = -1,
    gerort.prior.knowledge.courts = c("Mannheim", "Dortmund", "Köln"))
dev.off()




# == 4 Posterior Predictive Checks =============================================
# Press Law
plot_PPC_from_model(
  path_to_model_posterior = '../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData',
  path_to_orig_data = "../data/3_for_estimation/presslaw_images/mlt/y_count_cit_filter.csv",
  path_to_stan_model = "code_helpers/stan_poisson_1D_post_pred.stan",
  path_to_figure = "../figures_and_tables/main/figure_5a.eps"
)

plot_PPC_from_model(
  path_to_model_posterior = '../data/4_sampling_outputs/presslaw_online_linking_mlt_poisson.RData',
  path_to_orig_data = "../data/3_for_estimation/presslaw_online_linking/mlt/y_count_cit_filter.csv",
  path_to_stan_model = "code_helpers/stan_poisson_1D_post_pred.stan",
  path_to_figure = "../figures_and_tables/main/figure_5b.eps"
)



# == X Appendix ================================================================
# -- 1 Proof of Concept --------------------------------------------------------
# -- 1.1 Logit --------------


# Data location
data_location <-"../data/3_for_estimation/toy_data/"
 
# Connectivity matrix 
setEPS()
postscript("../figures_and_tables/appendix/figure_A1a.eps")
plot.adj.matrix(data_location)
dev.off()

# Estimation
model_file <- "code_helpers/stan_1D_logit.stan"

dat.temp <- read.in.connectivity.data(
  data_location, count = FALSE, cit_filter = FALSE)
data.final <- c(dat.temp, left_anchor = list(1), right_anchor = list(5))

if (run.sampling) fit.stan <- stan(
  file = model_file,  # Stan program
  data = data.final,  # Named list of data
  chains = 8,
  iter = 8000,  # Total number of iterations per chain
  cores = 8,   # Number of cores (using 2 just for the vignette)
  seed = project.seed,
  control=list(adapt_delta=0.8) # this can reduce divergence errors
)

# Check
if(start.shinystan) launch_shinystan(fit.stan)

# Add the text for the figure
more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_A2a.eps")
plot.nice.theta(draws.stan, more.vars.dat)
dev.off()



# Generate randomly connected toy data
# set.seed(project.seed)
# y <- read.csv("../data/3_for_estimation/toy_data/y.csv", header = FALSE)
# y <- sample(y)
# write.table(y, file = "../data/3_for_estimation/toy_data_random_order/y.csv",
#             col.names = FALSE, row.names = FALSE, sep = ",")

# ------- Random Order
# Data location
data_location <-"../data/3_for_estimation/toy_data_random_order/"

# Connectivity matrix 
setEPS()
postscript("../figures_and_tables/appendix/figure_A1b.eps")
plot.adj.matrix(data_location)
dev.off()

# Estimation
model_file <- "code_helpers/stan_1D_logit.stan"

dat.temp <- read.in.connectivity.data(
  data_location, count = FALSE, cit_filter = FALSE)
data.final <- c(dat.temp, left_anchor = list(1), right_anchor = list(5))


if (run.sampling) fit.stan <- stan(
  file = model_file,  # Stan program
  data = data.final,  # Named list of data
  chains = 8,
  iter = 8000,  # Total number of iterations per chain
  cores = 8,   # Number of cores (using 2 just for the vignette)
  seed = project.seed,
  control=list(adapt_delta=0.8) # this can reduce divergence errors
)

# Check
if(start.shinystan) launch_shinystan(fit.stan)

# Add the text for the figure
more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_A2b.eps")
plot.nice.theta(draws.stan, more.vars.dat)
dev.off()




# -- 1.2 Poisson --------------
# Data location
data_location <-"../data/3_for_estimation/toy_data_count/"

# Connectivity matrix 
setEPS()
postscript("../figures_and_tables/appendix/figure_A3a.eps")
plot.adj.matrix.counts.toydata(data_location)
dev.off()

# Estimation
model_file <- "code_helpers/stan_poisson_1D_decenter_anchors.stan"

dat.temp <- read.in.connectivity.data(
  data_location, count = TRUE, cit_filter = FALSE)
data.final <- c(dat.temp, left_anchor = list(1), right_anchor = list(5))

if (run.sampling) fit.stan <- stan(
  file = model_file,  # Stan program
  data = data.final,  # Named list of data
  chains = 8,
  iter = 8000,  # Total number of iterations per chain
  cores = 8,   # Number of cores (using 2 just for the vignette)
  seed = project.seed,
  control=list(adapt_delta=0.8) # this can reduce divergence errors
)

# Check
if(start.shinystan) launch_shinystan(fit.stan)

# Add the text for the figure
more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_A4a.eps")
plot.nice.theta(draws.stan, more.vars.dat, inverter = 1)
dev.off()


# ------- Random Order

# Generate randomly connected toy data
# set.seed(project.seed)
# y <- read.csv("../data/3_for_estimation/toy_data_count/y_count.csv", header = FALSE)
# y <- sample(y)
# write.table(y, file = "../data/3_for_estimation/toy_data_count_random_order/y_count.csv",
#             col.names = FALSE, row.names = FALSE, sep = ",")

# Data location
data_location <-"../data/3_for_estimation/toy_data_count_random_order/"

# Connectivity matrix 
setEPS()
postscript("../figures_and_tables/appendix/figure_A3b.eps")
plot.adj.matrix.counts.toydata(data_location)
dev.off()

# Estimation
model_file <- "code_helpers/stan_poisson_1D_decenter_anchors.stan"

dat.temp <- read.in.connectivity.data(
  data_location, count = TRUE, cit_filter = FALSE)
data.final <- c(dat.temp, left_anchor = list(1), right_anchor = list(5))

if (run.sampling) fit.stan <- stan(
  file = model_file,  # Stan program
  data = data.final,  # Named list of data
  chains = 8,
  iter = 8000,  # Total number of iterations per chain
  cores = 8,   # Number of cores (using 2 just for the vignette)
  seed = project.seed,
  control=list(adapt_delta=0.8) # this can reduce divergence errors
)

# Check
if(start.shinystan) launch_shinystan(fit.stan)

# Add the text for the figure
more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_A4b.eps")
plot.nice.theta(draws.stan, more.vars.dat, inverter = 1)
dev.off()




# == 2 Estimate Idealpoints ====================================================
# --- 2.1 Press Law Images Exact ------------------
data_location <- "../data/3_for_estimation/presslaw_images/exact/"
if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count= TRUE, cit_filter = TRUE,
                                                   seed.input = project.seed,
                                                   nr.iterations = 8000, 
                                                   init.model = 'vb', anchors.input = TRUE,
                                                   model = 'poisson')

# Running
if(start.shinystan) launch_shinystan(fit.stan)

if(save.sampling) save(fit.stan, 
                       file = '../data/4_sampling_outputs/presslaw_images_exact_poisson.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/presslaw_images_exact_poisson.RData')

draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_D1a.eps", 
    height = 8)
plot.nice.theta.w.courts.wrapper(draws.stan, more.vars.dat, inverter = 1,
                                 gerort.prior.knowledge.courts = c("Köln", "Hamburg"))
dev.off()



# --- 2.2 Press Law Online Linking Exact -------------
data_location <-"../data/3_for_estimation/presslaw_online_linking/exact/"

if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count = TRUE, cit_filter = TRUE, 
                                                   seed.input = project.seed, init.model = 'stan',
                                                   nr.iterations = 8000, anchors.input = TRUE,
                                                   model = 'poisson')

# Running
if(start.shinystan) launch_shinystan(fit.stan)


if(save.sampling) save(fit.stan, 
                       file = '../data/4_sampling_outputs/presslaw_online_linking_exact_poisson.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/presslaw_online_linking_exact_poisson.RData')
more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)


# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_D1b.eps", 
    height = 8)
plot.nice.theta.w.courts.wrapper(draws.stan, more.vars.dat, inverter = -1,
                                 gerort.prior.knowledge.courts = c("Köln", 
                                                                   "Hamburg"))
dev.off()



# -- 3 Connectivity Matrices -----------------------------------------------------

setEPS()
postscript("../figures_and_tables/appendix/figure_C1a.eps")
plot.adj.matrix.counts("../data/3_for_estimation/presslaw_images/exact/")
dev.off()

setEPS()
postscript("../figures_and_tables/appendix/figure_C2a.eps")
plot.adj.matrix.counts("../data/3_for_estimation/presslaw_images/mlt/")
dev.off()

setEPS()
postscript("../figures_and_tables/appendix/figure_C1b.eps")
plot.adj.matrix.counts("../data/3_for_estimation/presslaw_online_linking/exact/")
dev.off()

setEPS()
postscript("../figures_and_tables/appendix/figure_C2b.eps")
plot.adj.matrix.counts("../data/3_for_estimation/presslaw_online_linking/mlt/")
dev.off()

setEPS()
postscript("../figures_and_tables/appendix/figure_C3a.eps")
plot.adj.matrix.counts("../data/3_for_estimation/antitrust/")
dev.off()

# Ordering the citation matrix from antitrust by time
setEPS()
postscript("../figures_and_tables/appendix/figure_C3b.eps")
plot.adj.matrix.counts("../data/3_for_estimation/antitrust_ordered_by_time/")
dev.off()



# -- 4 Convergence Diagnostics ---------------------------------------------------

if (load.sampling) load('../data/4_sampling_outputs/presslaw_images_exact_poisson.RData')
theta.summary <- summary(fit.stan, pars = c("theta"), probs = c(0.1, 0.9))$summary
theta.summary.table <- theta.summary[, c("mean", "se_mean", "n_eff", "Rhat")]
stargazer(theta.summary.table, 
          digits = 2, 
          title = "Convergence Diagnostics for Idealpoints. Presslaw Images Exact (Case 1a).",
          out = "../figures_and_tables/appendix/theta_conv_presslaw_images_exact_poisson.tex"
          )

if (load.sampling) load('../data/4_sampling_outputs/presslaw_images_mlt_poisson.RData')
theta.summary <- summary(fit.stan, pars = c("theta"), probs = c(0.1, 0.9))$summary
theta.summary.table <- theta.summary[, c("mean", "se_mean", "n_eff", "Rhat")]
stargazer(theta.summary.table, 
          digits = 2, 
          title = "Convergence Diagnostics for Idealpoints. Presslaw Images MLT (Case 2a).",
          out = "../figures_and_tables/appendix/theta_conv_presslaw_images_mlt_poisson.tex"
)


if (load.sampling) load('../data/4_sampling_outputs/presslaw_online_linking_exact_poisson.RData')
theta.summary <- summary(fit.stan, pars = c("theta"), probs = c(0.1, 0.9))$summary
theta.summary.table <- theta.summary[, c("mean", "se_mean", "n_eff", "Rhat")]
stargazer(theta.summary.table, 
          digits = 2, 
          title = "Convergence Diagnostics for Idealpoints. Presslaw Online Linking Exact (Case 1b).",
          out = "../figures_and_tables/appendix/theta_conv_presslaw_online_linking_exact_poisson.tex"
)

if (load.sampling) load('../data/4_sampling_outputs/presslaw_online_linking_mlt_poisson.RData')
theta.summary <- summary(fit.stan, pars = c("theta"), probs = c(0.1, 0.9))$summary
theta.summary.table <- theta.summary[, c("mean", "se_mean", "n_eff", "Rhat")]
stargazer(theta.summary.table, 
          digits = 2, 
          title = "Convergence Diagnostics for Idealpoints. Presslaw Online Linking MLT (Case 2b).",
          out = "../figures_and_tables/appendix/theta_conv_presslaw_online_linking_mlt_poisson.tex"
)

if (load.sampling) load('../data/4_sampling_outputs/cartels_count_poisson.RData')
theta.summary <- summary(fit.stan, pars = c("theta"), probs = c(0.1, 0.9))$summary
theta.summary.table <- theta.summary[, c("mean", "se_mean", "n_eff", "Rhat")]
stargazer(theta.summary.table, 
          digits = 2, 
          title = "Convergence Diagnostics for Idealpoints. Antitrust (Case 3).",
          out = "../figures_and_tables/appendix/theta_conv_antitrust_poisson.tex"
)



# -- 5 Validity ---------------------------------------------------
# Assessment of outcome in relation to average position 
# of a decision by each court with at least three decisions 
# per assessed legal area.
source("validity.r")



# -- 6 Random Order Positions --------------------------------------------------
# -- 6.1 Press Law Images MLT --------------------------------------------------

# Generate randomly connected toy data
# y <- read.csv("../data/3_for_estimation/presslaw_images/mlt/y_count_cit_filter.csv",
#               header = FALSE)
# set.seed(projebt.seed)
# y <- sample(y)
# # y <- rpois(length(y), .1)
# 
# write.table(y, file = "../data/3_for_estimation/presslaw_images/mlt_random_order/y_count_cit_filter.csv",
#             col.names = FALSE, row.names = FALSE, sep = ",")


data_location <-"../data/3_for_estimation/presslaw_images/mlt_random_order/"

if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count= TRUE, cit_filter = TRUE, 
                                                   seed.input = project.seed,
                                                   nr.iterations = 8000, 
                                                   init.model = 'stan', anchors.input = TRUE,
                                                   model = 'poisson')

if(save.sampling) save(fit.stan, 
                       file = '../data/4_sampling_outputs/presslaw_images_mlt_poisson_random_order.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/presslaw_images_mlt_poisson_random_order.RData')


if(start.shinystan) launch_shinystan(fit.stan)

draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_H1a.eps",
    height = 8)
plot.nice.theta.w.courts.wrapper(draws.stan, more.vars.dat, inverter = -1,
                                 gerort.prior.knowledge.courts = c("Köln",
                                                                   "Hamburg"))
dev.off()



# PPC
plot_PPC_from_model(
  path_to_model_posterior = '../data/4_sampling_outputs/presslaw_images_mlt_poisson_random_order.RData',
  path_to_orig_data = "../data/3_for_estimation/presslaw_images/mlt_random_order/y_count_cit_filter.csv",
  path_to_stan_model = "code_helpers/stan_poisson_1D_post_pred.stan",
  path_to_figure = "../figures_and_tables/appendix/figure_H2a.eps"
)



# --- 6.2 Press Law Online Linking MLT ---------------
# y <- read.csv("../data/3_for_estimation/presslaw_online_linking/mlt/y_count_cit_filter.csv",
#               header = FALSE)
# set.seed(project.seed)
# y <- sample(y)
# write.table(y, file = "../data/3_for_estimation/presslaw_online_linking/mlt_random_order/y_count_cit_filter.csv",
#             col.names = FALSE, row.names = FALSE, sep = ",")


data_location <-"../data/3_for_estimation/presslaw_online_linking/mlt_random_order/"
if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count = TRUE, cit_filter = TRUE, 
                                                   seed.input = project.seed, init.model = 'stan',
                                                   nr.iterations = 8000, anchors.input = TRUE,
                                                   model = 'poisson')

if(start.shinystan) launch_shinystan(fit.stan)


if(save.sampling) save(fit.stan, 
                       file = '../data/4_sampling_outputs/presslaw_online_linking_random_order.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/presslaw_online_linking_random_order.RData')


more.vars.dat <- read.in.data(data_location)
draws.stan <- extract(fit.stan)

# Plotting decisions
setEPS()
postscript("../figures_and_tables/appendix/figure_H1b.eps", 
    height = 8)
plot.nice.theta.w.courts.wrapper(draws.stan, more.vars.dat, inverter = -1,
                                 gerort.prior.knowledge.courts = c("Köln", 
                                                                   "Hamburg"))
dev.off()


# PPC
plot_PPC_from_model(
  path_to_model_posterior = '../data/4_sampling_outputs/presslaw_online_linking_random_order.RData',
  path_to_orig_data = "../data/3_for_estimation/presslaw_online_linking/mlt_random_order/y_count_cit_filter.csv",
  path_to_stan_model = "code_helpers/stan_poisson_1D_post_pred.stan",
  path_to_figure = "../figures_and_tables/appendix/figure_H2b.eps"
)



# -- 6.3 Antitrust -------------------------------------------------------------
# y <- read.csv("../data/3_for_estimation/antitrust/y_count_cit_filter.csv",
#               header = FALSE)
# set.seed(project.seed)
# y <- sample(y)
# write.table(y, file = "../data/3_for_estimation/antitrust_random_order/y_count_cit_filter.csv",
#             col.names = FALSE, row.names = FALSE, sep = ",")


data_location <-"../data/3_for_estimation/antitrust_random_order/"

if (run.sampling) fit.stan <- run.stan.to.estimate(data_location, count = TRUE, cit_filter = TRUE, 
                                                   seed.input = project.seed, init.model = 'vb',
                                                   nr.iterations = 8000, anchors.input = TRUE,
                                                   model = 'poisson')
if(start.shinystan) launch_shinystan(fit.stan)
# print(fit.stan, pars = c('theta1', 'theta'))

if(save.sampling) save(fit.stan, 
                       file = '../data/4_sampling_outputs/cartels_count_poisson_random_order.RData')
if(load.sampling) load(file = '../data/4_sampling_outputs/cartels_count_poisson_random_order.RData')



draws.stan <- extract(fit.stan)
more.vars.dat <- read.in.data(data_location)


setEPS()
postscript("../figures_and_tables/appendix/figure_H1c.eps", 
    height = 8)
plot.nice.theta.w.courts.wrapper(
  draws.stan, more.vars.dat, inverter = -1,
  gerort.prior.knowledge.courts = c("Mannheim", "Dortmund", "Köln"))
dev.off()


# PPC
plot_PPC_from_model(
  path_to_model_posterior = '../data/4_sampling_outputs/cartels_count_poisson_random_order.RData',
  path_to_orig_data = "../data/3_for_estimation/antitrust_random_order/y_count_cit_filter.csv",
  path_to_stan_model = "code_helpers/stan_poisson_1D_post_pred.stan",
  path_to_figure = "../figures_and_tables/appendix/figure_H2c.eps"
)



# -- Dependencies --------------------------------------------------------------

# sessionInfo()
# R version 4.0.3 (2020-10-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 10.16
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils    
# [5] datasets  methods   base     
# 
# other attached packages:
#   [1] stargazer_5.2.2      reticulate_1.18     
# [3] rstan_2.21.1         StanHeaders_2.21.0-6
# [5] ggplot2_3.3.5        shinystan_2.5.0     
# [7] shiny_1.5.0          plyr_1.8.6          
# [9] stringr_1.4.0        XML_3.99-0.5        
# 
# loaded via a namespace (and not attached):
#   [1] jsonlite_1.7.2     gtools_3.8.2      
# [3] RcppParallel_5.0.2 threejs_0.3.3     
# [5] assertthat_0.2.1   stats4_4.0.3      
# [7] pillar_1.6.1       lattice_0.20-41   
# [9] glue_1.4.2         digest_0.6.27     
# [11] promises_1.1.1     colorspace_2.0-0  
# [13] Matrix_1.2-18      htmltools_0.5.0   
# [15] httpuv_1.5.4       dygraphs_1.1.1.6  
# [17] pkgconfig_2.0.3    purrr_0.3.4       
# [19] xtable_1.8-4       scales_1.1.1      
# [21] processx_3.5.2     later_1.1.0.1     
# [23] tibble_3.1.2       farver_2.0.3      
# [25] bayesplot_1.7.2    generics_0.1.0    
# [27] ellipsis_0.3.2     DT_0.16           
# [29] withr_2.3.0        shinyjs_2.0.0     
# [31] cli_2.5.0          magrittr_2.0.1    
# [33] crayon_1.4.1       mime_0.9          
# [35] ps_1.5.0           fansi_0.4.1       
# [37] xts_0.12.1         pkgbuild_1.2.0    
# [39] colourpicker_1.1.0 rsconnect_0.8.16  
# [41] tools_4.0.3        loo_2.4.1         
# [43] prettyunits_1.1.1  lifecycle_1.0.0   
# [45] matrixStats_0.57.0 V8_3.4.0          
# [47] munsell_0.5.0      callr_3.7.0       
# [49] compiler_4.0.3     rlang_0.4.11      
# [51] grid_4.0.3         ggridges_0.5.2    
# [53] htmlwidgets_1.5.3  crosstalk_1.1.0.1 
# [55] igraph_1.2.6       miniUI_0.1.1.1    
# [57] labeling_0.4.2     base64enc_0.1-3   
# [59] gtable_0.3.0       codetools_0.2-16  
# [61] inline_0.3.17      DBI_1.1.1         
# [63] curl_4.3           markdown_1.1      
# [65] reshape2_1.4.4     R6_2.5.0          
# [67] gridExtra_2.3      zoo_1.8-8         
# [69] dplyr_1.0.7        fastmap_1.0.1     
# [71] utf8_1.1.4         shinythemes_1.1.2 
# [73] stringi_1.5.3      parallel_4.0.3    
# [75] Rcpp_1.0.5         vctrs_0.3.8       
# [77] tidyselect_1.1.0  